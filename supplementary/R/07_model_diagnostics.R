# 07 diagnostics following 06
#
# Model 6: postgraduate x 1957-64 cell
# syntax vs overview: female coefficient

library(sandwich)
library(lmtest)

source("supplementary/R/06_model_sensitivity.R")

out_dir  <- "supplementary/output/model_diagnostics"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

save_csv <- function(x, name) {
  write.csv(x, file.path(out_dir, name), row.names = FALSE, na = "")
}

open_png <- function(name, width = 1600, height = 1200) {
  png(file.path(plot_dir, name), width = width, height = height, res = 200)
}

min_cell <- 5
common <- c("historical_common", "syntax_common", "overview_common")
model_names <- names(model_rhs)

hc3 <- function(fit) vcovHC(fit, type = "HC3")

# match model rows back to the Stage 06 data before using ISCO
rows_in <- function(fit, d) {
  k <- match(rownames(model.frame(fit)), rownames(d))
  if (anyNA(k)) stop("model rows do not map back to the data")
  k
}


# Model 6 interaction blocks ----

m6_blocks <- list(
  # all_6 = c("bach_x_c2", "bach_x_c3", "bach_x_c4",
  #           "post_x_c2", "post_x_c3", "post_x_c4"),
  bachelor_3 = c("bach_x_c2", "bach_x_c3", "bach_x_c4"),
  postgraduate_3 = c("post_x_c2", "post_x_c3", "post_x_c4")
)

wald_row <- function(fit, terms, vc = NULL) {
  lh <- if (is.null(vc)) {
    car::linearHypothesis(fit, terms)
  } else {
    car::linearHypothesis(fit, terms, vcov. = vc)
  }

  data.frame(
    q = lh$Df[2],
    df2 = df.residual(fit),
    F_stat = lh$F[2],
    p_F = lh[["Pr(>F)"]][2]
  )
}

block_wald <- data.frame()
for (v in names(fits)) {
  fit <- fits[[v]]$Model6
  for (b in names(m6_blocks)) {
    block_wald <- rbind(
      block_wald,
      cbind(version = v, block = b, covariance = "classical",
            wald_row(fit, m6_blocks[[b]])),
      cbind(version = v, block = b, covariance = "HC3",
            wald_row(fit, m6_blocks[[b]], hc3(fit)))
    )
  }
}

print(block_wald, row.names = FALSE)


# Model 6 classical against HC3 ----

se_terms <- c("edu_postgrad", "bach_x_c2", "bach_x_c3", "bach_x_c4",
              "post_x_c2", "post_x_c3", "post_x_c4")

se_ratio <- coefficient_results[
  coefficient_results$model == "Model6" & coefficient_results$term %in% se_terms,
  c("version", "model", "term", "estimate", "se_classical", "p_classical",
    "se_hc3", "p_hc3")
]

se_ratio$term_group <- "postgraduate_interaction"
se_ratio$term_group[grepl("^bach_x_", se_ratio$term)] <- "bachelor_interaction"
se_ratio$term_group[se_ratio$term == "edu_postgrad"] <- "postgraduate_main"
se_ratio$hc3_se_ratio <- se_ratio$se_hc3 / se_ratio$se_classical
se_ratio <- se_ratio[
  order(match(se_ratio$version, names(fits)), match(se_ratio$term, se_terms)),
]
save_csv(se_ratio, "07_model6_se_ratio.csv")


# sparse postgraduate reference cell ----

# with the cohort interactions in Model 6, edu_postgrad is the postgraduate
# contrast for cohort 1; inspect that cell directly
pg_cell_index <- function(fit) {
  mf <- model.frame(fit)
  which(mf$edu_postgrad == 1 & mf$cohort2 == 0 & mf$cohort3 == 0 & mf$cohort4 == 0)
}

sparse_cell <- data.frame()
for (v in names(fits)) {
  fit <- fits[[v]]$Model6
  idx <- pg_cell_index(fit)
  if (length(idx) == 0) stop("no postgraduates in cohort 1 for ", v)

  mf <- model.frame(fit)
  y  <- as.numeric(model.response(mf))
  h  <- hatvalues(fit)
  rs <- rstudent(fit)
  cd <- cooks.distance(fit)
  hc3_adj <- 1 / (1 - h)^2

  sparse_cell <- rbind(
    sparse_cell,
    data.frame(
      version = v,
      N_pg_cohort1 = length(idx),
      cell_outcome_min = min(y[idx]),
      cell_outcome_max = max(y[idx]),
      mean_cell_leverage = mean(h[idx]),
      leverage_ratio = mean(h[idx]) / mean(h),
      mean_hc3_adj = mean(hc3_adj[idx]),
      max_abs_studentized_residual = max(abs(rs[idx])),
      max_cooks_distance = max(cd[idx])
    )
  )
}

# Keep the diagnostics internally, but do not publish the outcome range
# for a cell with fewer than five respondents.
sparse_cell_public <- sparse_cell
small_cell <- sparse_cell_public$N_pg_cohort1 < 5
sparse_cell_public$cell_outcome_min[small_cell] <- NA_real_
sparse_cell_public$cell_outcome_max[small_cell] <- NA_real_

save_csv(sparse_cell_public, "07_model6_sparse_cell.csv")
print(
  sparse_cell_public[, c("version", "N_pg_cohort1", "leverage_ratio",
                         "mean_hc3_adj", "max_abs_studentized_residual",
                         "max_cooks_distance")],
  row.names = FALSE
)

fit6 <- fits$syntax_common$Model6
h6   <- hatvalues(fit6)
e6   <- residuals(fit6)
rs6  <- rstudent(fit6)
cell <- pg_cell_index(fit6)

open_png("07_model6_diagnostics.png", width = 2200, height = 1100)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

plot(fitted(fit6), e6, pch = 16, col = "grey55", cex = 0.6,
     xlab = "fitted ISEI", ylab = "residual", main = "residuals against fitted")
abline(h = 0, col = "grey20")
points(fitted(fit6)[cell], e6[cell], pch = 21, bg = "grey30", cex = 1.3)

plot(h6, rs6, pch = 16, col = "grey55", cex = 0.6,
     xlab = "leverage", ylab = "studentized residual",
     main = "studentized residuals against leverage")
abline(h = 0, col = "grey20")
abline(v = 2 * mean(h6), lty = 2, col = "grey30")
points(h6[cell], rs6[cell], pch = 21, bg = "grey30", cex = 1.3)
legend("topright", c("postgraduate, cohort 1", "twice mean leverage"),
       pch = c(21, NA), pt.bg = c("grey30", NA), lty = c(NA, 2), bty = "n")

dev.off()


# female coefficient under the three common-support mappings ----

female_sensitivity <- data.frame()
for (v in common) {
  for (m in model_names) {
    fit <- fits[[v]][[m]]
    cls <- coeftest(fit)
    rob <- coeftest(fit, vcov. = hc3(fit))

    female_sensitivity <- rbind(
      female_sensitivity,
      data.frame(
        version = v,
        model = m,
        N = nobs(fit),
        estimate = cls["female", 1],
        se_classical = cls["female", 2],
        p_classical = cls["female", 4],
        se_hc3 = rob["female", 2],
        p_hc3 = rob["female", 4]
      )
    )
  }
}

save_csv(female_sensitivity, "07_female_crosswalk_sensitivity.csv")
print(female_sensitivity, row.names = FALSE)


# how syntax and overview relate ----

d_s <- versions$syntax_common
d_o <- versions$overview_common
y_s <- d_s$.y
y_o <- d_o$.y
isco_common <- as.numeric(d_s$isco_main)

compression <- lm(y_o ~ y_s)
comp_a <- unname(coef(compression)[1])
comp_b <- unname(coef(compression)[2])
comp_resid <- as.numeric(residuals(compression))

# Stage 05 found one syntax/overview score pair per ISCO; stop if that changes
code_level <- unique(data.frame(
  isco_main = isco_common,
  syntax = y_s,
  overview = y_o
))
if (anyDuplicated(code_level$isco_main)) {
  stop("an ISCO code has more than one score pair")
}
n_by_code <- table(isco_common)
code_level$N <- as.integer(n_by_code[as.character(code_level$isco_main)])

compression_benchmark <- data.frame(
  N_persons = length(y_s),
  N_ISCO_codes = nrow(code_level),
  intercept = comp_a,
  slope = comp_b,
  R2 = summary(compression)$r.squared,
  syntax_mean = mean(y_s),
  overview_mean = mean(y_o),
  syntax_sd = sd(y_s),
  overview_sd = sd(y_o)
)

save_csv(compression_benchmark, "07_syntax_overview_compression.csv")
cat(sprintf("overview = %.3f + %.4f x syntax, R2 %.4f\n",
            comp_a, comp_b, summary(compression)$r.squared))

open_png("07_compression.png")
par(mar = c(4.5, 4.5, 3, 1))
with(code_level,
     plot(syntax, overview, pch = 21, bg = "grey70", col = "grey30",
          cex = 0.4 + sqrt(N) / 4,
          xlab = "ISEI, syntax crosswalk", ylab = "ISEI, overview crosswalk",
          main = "one point per ISCO code, size by number of people"))
abline(0, 1, lty = 2, col = "grey40")
abline(compression, lwd = 2, col = "grey20")
legend("topleft", c("fitted", "45 degrees"), lwd = c(2, 1), lty = c(1, 2),
       col = c("grey20", "grey40"), bty = "n")
dev.off()


# female coefficient decomposition ----

# split the female shift into scaling and the remaining part
female_decomp <- data.frame()
for (m in model_names) {
  fit_s <- fits$syntax_common[[m]]
  fit_o <- fits$overview_common[[m]]
  X <- model.matrix(fit_s)
  k <- rows_in(fit_s, d_s)

  b_s <- coef(fit_s)
  b_o <- coef(fit_o)
  b_r <- lm.fit(X, comp_resid[k])$coefficients
  names(b_r) <- colnames(X)

  beta_s <- b_s[["female"]]
  beta_o <- b_o[["female"]]
  beta_r_f <- b_r[["female"]]
  beta_pure <- comp_b * beta_s
  delta <- beta_o - beta_s

  female_decomp <- rbind(
    female_decomp,
    data.frame(
      model = m,
      N = nobs(fit_s),
      beta_syntax = beta_s,
      beta_overview = beta_o,
      actual_change = delta,
      beta_scaled = beta_pure,
      compression_component = beta_pure - beta_s,
      residual_component = beta_r_f,
      compression_share = (beta_pure - beta_s) / delta,
      residual_share = beta_r_f / delta
    )
  )
}

save_csv(female_decomp, "07_female_coefficient_decomposition.csv")


# which occupations carry the non-rescaling part ----

# FWL weights for the female coefficient
female_weights <- function(fit) {
  X <- model.matrix(fit)
  j <- match("female", colnames(X))
  r <- lm.fit(X[, -j, drop = FALSE], X[, j])$residuals
  r / sum(r * X[, j])
}

by_code <- function(fit, d, value, female) {
  k <- rows_in(fit, d)
  isco <- as.numeric(d$isco_main[k])
  w <- female_weights(fit)
  groups <- split(seq_along(isco), isco)
  out <- vector("list", length(groups))
  j <- 1

  for (i in groups) {
    out[[j]] <- data.frame(
      isco_main = isco[i[1]],
      N = length(i),
      female_share = mean(female[i]),
      value = value[i[1]],
      female_weight_sum = sum(w[i]),
      contribution = sum(w[i]) * value[i[1]]
    )
    j <- j + 1
  }

  do.call(rbind, out)
}

top_codes <- function(tab, id_cols, score_cols, n = 15) {
  big <- tab[tab$N >= min_cell, ]
  small <- tab[tab$N < min_cell, ]

  out <- data.frame(
    id_cols,
    group = "ISCO",
    isco_main = big$isco_main,
    N_reported = big$N,
    female_share_reported = big$female_share,
    big[score_cols],
    female_weight_sum = big$female_weight_sum,
    contribution = big$contribution,
    abs_contribution = abs(big$contribution)
  )

  if (nrow(small) > 0) {
    blank <- setNames(as.list(rep(NA_real_, length(score_cols))), score_cols)
    pooled <- data.frame(
      id_cols,
      group = "small_cells_pooled",
      isco_main = NA,
      N_reported = sum(small$N),
      female_share_reported = NA,
      blank,
      female_weight_sum = sum(small$female_weight_sum),
      contribution = sum(small$contribution),
      abs_contribution = sum(abs(small$contribution))
    )
    out <- rbind(out, pooled)
  }

  out <- out[order(-out$abs_contribution), ]
  out$abs_share <- out$abs_contribution / sum(out$abs_contribution)
  head(out, n)
}

residual_by_code <- function(m) {
  fit_s <- fits$syntax_common[[m]]
  fit_o <- fits$overview_common[[m]]
  mf <- model.frame(fit_s)
  k <- rows_in(fit_s, d_s)

  tab <- by_code(fit_s, d_s, comp_resid[k], mf$female)
  names(tab)[names(tab) == "value"] <- "residual_from_compression"

  ys <- as.numeric(model.response(mf))
  yo <- as.numeric(model.response(model.frame(fit_o)))
  first <- match(tab$isco_main, as.numeric(d_s$isco_main[k]))
  tab$syntax <- ys[first]
  tab$overview <- yo[first]
  tab$compression_pred <- comp_a + comp_b * tab$syntax

  beta_r <- sum(female_weights(fit_s) * comp_resid[k])
  top <- top_codes(
    tab,
    data.frame(model = m),
    c("syntax", "overview", "compression_pred", "residual_from_compression")
  )
  top$cum_abs_share_all <- cumsum(top$abs_share)
  top$share_of_residual_net <- top$contribution / beta_r
  top
}

residual_top <- rbind(residual_by_code("Model1"), residual_by_code("Model6"))
save_csv(residual_top, "07_female_isco_residual_top_contributions.csv")


# raw syntax-to-overview changes for Model 1 and Model 6 ----

cw_summary <- data.frame()
cw_top <- data.frame()

for (m in c("Model1", "Model6")) {
  fit_from <- fits$syntax_common[[m]]
  fit_to <- fits$overview_common[[m]]
  mf <- model.frame(fit_from)

  y_from <- as.numeric(model.response(mf))
  y_to <- as.numeric(model.response(model.frame(fit_to)))
  diff_y <- y_to - y_from
  beta_from <- coef(fit_from)[["female"]]
  beta_to <- coef(fit_to)[["female"]]
  beta_change <- beta_to - beta_from

  fem <- mf$female == 1
  cw_summary <- rbind(
    cw_summary,
    data.frame(
      model = m,
      beta_from = beta_from,
      beta_to = beta_to,
      beta_change = beta_change,
      mean_diff_female = mean(diff_y[fem]),
      mean_diff_male = mean(diff_y[!fem]),
      female_male_raw_diff = mean(diff_y[fem]) - mean(diff_y[!fem])
    )
  )

  codes <- by_code(fit_from, d_s, diff_y, mf$female)
  names(codes)[names(codes) == "value"] <- "raw_difference"
  first <- match(codes$isco_main, as.numeric(d_s$isco_main[rows_in(fit_from, d_s)]))
  codes$from_score <- y_from[first]
  codes$to_score <- y_to[first]

  top <- top_codes(
    codes,
    data.frame(model = m),
    c("from_score", "to_score", "raw_difference")
  )
  top$net_share <- top$contribution / beta_change
  cw_top <- rbind(cw_top, top)
}

print(cw_summary, row.names = FALSE)
save_csv(cw_top, "07_female_crosswalk_top_contributions.csv")

z <- cw_top[cw_top$model == "Model1", ]
z <- z[order(z$contribution), ]
labels <- ifelse(is.na(z$isco_main), "pooled small cells", as.character(z$isco_main))

open_png("07_female_contributions.png", width = 1600, height = 1400)
par(mar = c(4.5, 8, 3, 1))
barplot(z$contribution, horiz = TRUE, names.arg = labels, las = 1,
        col = "grey55",
        border = NA,
        xlab = "contribution to the change in the female coefficient",
        main = "Model 1, syntax to overview")
abline(v = 0, col = "grey20")
dev.off()
