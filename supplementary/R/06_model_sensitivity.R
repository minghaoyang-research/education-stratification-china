# 06 Models 1-7 under alternative ISEI mappings
# the three common versions use the same 1646 cases, the _full ones add the 4 ISCO 1000 cases

source("R/03_original_isei.R")

rep_dir <- "output/original_replication"
out_dir <- "supplementary/output/model_sensitivity"
map_file <- "supplementary/output/isei_sensitivity/05_code_level_mapping_audit.csv"
prov_file <- "supplementary/output/isei_sensitivity/05_package_provenance.csv"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# score check and stage 04 benchmarks
same_values <- function(x, y, tol = 1e-8, label = "") {
  stopifnot(length(x) == length(y), identical(is.na(x), is.na(y)))
  keep <- !is.na(x)
  d <- if (any(keep)) max(abs(x[keep] - y[keep])) else 0
  if (!isTRUE(d <= tol)) stop(label, ": max diff ", format(d), call. = FALSE)
  invisible(d)
}

# HC3 robustness check alongside the thesis SEs
vcov_hc3 <- function(fit) sandwich::vcovHC(fit, type = "HC3")


# scores and samples ----

if (!file.exists(map_file)) stop("run 05_isei_sensitivity.R first")

mapping <- read.csv(map_file)
stopifnot(!anyDuplicated(mapping$isco_numeric))

stage6 <- stage3_dat
stage6$row_no <- seq_len(nrow(stage6))

k <- match(as.integer(stage6$isco_main), as.integer(mapping$isco_numeric))
stopifnot(!anyNA(k))
same_values(stage6$isei, mapping$isei_historical[k], 1e-8, "03 vs 05 historical ISEI")

stage6$isei_syntax <- mapping$isei_syntax[k]
stage6$isei_overview <- mapping$isei_overview[k]

stage6$bach_x_rural <- stage6$edu_bachelor * stage6$rural_origin
stage6$post_x_rural <- stage6$edu_postgrad * stage6$rural_origin
stage6$bach_x_c2 <- stage6$edu_bachelor * stage6$cohort2
stage6$bach_x_c3 <- stage6$edu_bachelor * stage6$cohort3
stage6$bach_x_c4 <- stage6$edu_bachelor * stage6$cohort4
stage6$post_x_c2 <- stage6$edu_postgrad * stage6$cohort2
stage6$post_x_c3 <- stage6$edu_postgrad * stage6$cohort3
stage6$post_x_c4 <- stage6$edu_postgrad * stage6$cohort4

core_flag <- !is.na(stage6$rural_origin) & !is.na(stage6$par_edu_yrs)
historical_flag <- core_flag & !is.na(stage6$isei)
syntax_full_flag <- core_flag & !is.na(stage6$isei_syntax)
overview_full_flag <- core_flag & !is.na(stage6$isei_overview)
common_flag <- historical_flag & !is.na(stage6$isei_syntax) & !is.na(stage6$isei_overview)
recovered_flag <- syntax_full_flag & !historical_flag

stopifnot(
  sum(core_flag) == 1650,
  sum(historical_flag) == 1646,
  sum(common_flag) == 1646,
  sum(syntax_full_flag) == 1650,
  sum(overview_full_flag) == 1650,
  sum(recovered_flag) == 4
)

# ISCO 1000 is missing in the historical map and mapped by the package
rec <- stage6[recovered_flag, ]
stopifnot(all(rec$isco_main == 1000), all(rec$rural_origin == 0), all(rec$edu_lvl == 2))


# five versions ----

make_version <- function(flag, y, label) {
  d <- stage6[flag, , drop = FALSE]
  d$.y <- y[flag]
  d$.version <- label
  d
}

versions <- list(
  historical_common = make_version(common_flag, stage6$isei, "historical_common"),
  syntax_common = make_version(common_flag, stage6$isei_syntax, "syntax_common"),
  overview_common = make_version(common_flag, stage6$isei_overview, "overview_common"),
  syntax_full = make_version(syntax_full_flag, stage6$isei_syntax, "syntax_full"),
  overview_full = make_version(overview_full_flag, stage6$isei_overview, "overview_full")
)

# row order must match across the three common versions
rows <- lapply(versions[1:3], function(d) d$row_no)
stopifnot(identical(rows[[1]], rows[[2]]), identical(rows[[1]], rows[[3]]))


# models ----

common_terms <- c("edu_bachelor", "edu_postgrad", "female")
model_rhs <- list(
  Model1 = c(common_terms, "age"),
  Model2 = c(common_terms, "age", "par_edu_yrs"),
  Model3 = c(common_terms, "age", "par_edu_yrs", "rural_origin"),
  Model4 = c(common_terms, "age", "par_edu_yrs", "rural_origin",
             "bach_x_rural", "post_x_rural"),
  Model5 = c(common_terms, "par_edu_yrs", "rural_origin",
             "cohort2", "cohort3", "cohort4"),
  Model6 = c(common_terms, "par_edu_yrs", "rural_origin",
             "cohort2", "cohort3", "cohort4",
             "bach_x_c2", "bach_x_c3", "bach_x_c4",
             "post_x_c2", "post_x_c3", "post_x_c4"),
  Model7 = c(common_terms, "age", "par_edu_yrs", "rural_origin_f")
)

fits <- lapply(versions, function(d) {
  lapply(model_rhs, function(x) lm(reformulate(x, ".y"), data = d))
})

stopifnot(
  sapply(fits[1:3], function(x) sapply(x, nobs)) == 1646,
  sapply(fits[4:5], function(x) sapply(x, nobs)) == 1650
)

vif <- function(fit) {
  X <- model.matrix(fit)[, -1, drop = FALSE]
  out <- sapply(seq_len(ncol(X)), function(j) {
    1 / (1 - summary(lm(X[, j] ~ X[, -j]))$r.squared)
  })
  setNames(out, colnames(X))
}

each_fit <- function(f) {
  out <- list()
  for (v in names(fits)) {
    for (m in names(fits[[v]])) out[[length(out) + 1]] <- f(fits[[v]][[m]], v, m)
  }
  do.call(rbind, out)
}

coefficient_results <- each_fit(function(fit, v, m) {
  cm <- summary(fit)$coefficients
  se_hc3 <- sqrt(diag(vcov_hc3(fit)))
  t_hc3 <- coef(fit) / se_hc3
  y_sd <- sd(fit$model$.y)
  data.frame(
    version = v, model = m, term = rownames(cm),
    N = nobs(fit), residual_df = df.residual(fit),
    estimate = cm[, 1],
    se_classical = cm[, 2],
    t_classical = cm[, 3],
    p_classical = cm[, 4],
    se_hc3 = unname(se_hc3),
    t_hc3 = unname(t_hc3),
    p_hc3 = 2 * pt(abs(t_hc3), df.residual(fit), lower.tail = FALSE),
    outcome_sd = y_sd,
    estimate_over_y_sd = cm[, 1] / y_sd,
    row.names = NULL
  )
})

fit_statistics <- each_fit(function(fit, v, m) {
  sm <- summary(fit)
  data.frame(version = v, model = m,
             N = nobs(fit), residual_df = df.residual(fit),
             R2 = unname(sm$r.squared), adjusted_R2 = unname(sm$adj.r.squared),
             residual_SE = unname(sm$sigma), RSS = sum(residuals(fit)^2),
             F = unname(sm$fstatistic[1]),
             model_df = unname(sm$fstatistic[2]),
             denominator_df = unname(sm$fstatistic[3]))
})

vif_results <- each_fit(function(fit, v, m) {
  x <- vif(fit)
  data.frame(version = v, model = m, term = names(x),
             VIF = unname(x), tolerance = 1 / unname(x))
})


# check the historical version against stage 04 ----
# looser tolerance for RSS

old_coef <- read.csv(file.path(rep_dir, "original_models_coefficients.csv"))
old_fit <- read.csv(file.path(rep_dir, "original_models_fit_statistics.csv"))
old_vif <- read.csv(file.path(rep_dir, "original_models_vif.csv"))
old_joint <- read.csv(file.path(rep_dir, "original_joint_interaction_tests.csv"))

hist_rows <- function(d, m) d[d$version == "historical_common" & d$model == m, ]

for (m in names(model_rhs)) {
  now <- hist_rows(coefficient_results, m)
  was <- old_coef[old_coef$model == m, ]
  stopifnot(identical(now$term, was$term))
  same_values(now$estimate, was$B, 1e-8, paste(m, "B"))
  same_values(now$se_classical, was$SE, 1e-8, paste(m, "SE"))

  now <- hist_rows(fit_statistics, m)
  was <- old_fit[old_fit$model == m, ]
  stopifnot(now$N == was$N, now$residual_df == was$residual_df)
  same_values(c(now$R2, now$adjusted_R2), c(was$R2, was$adjusted_R2), 1e-10, paste(m, "R2"))
  same_values(now$RSS, was$RSS, 1e-6, paste(m, "RSS"))

  now <- hist_rows(vif_results, m)
  was <- old_vif[old_vif$model == m, ]
  stopifnot(identical(now$term, was$term))
  same_values(now$VIF, was$VIF, 1e-8, paste(m, "VIF"))

}


# interaction blocks ----

blocks <- list(
  Model3_vs_Model4 = list(
    small = "Model3", big = "Model4",
    block = "education_x_rural_origin"
  ),
  Model5_vs_Model6 = list(
    small = "Model5", big = "Model6",
    block = "education_x_cohort"
  )
)

interaction_tests <- do.call(rbind, lapply(names(fits), function(v) {
  do.call(rbind, lapply(names(blocks), function(b) {
    bl <- blocks[[b]]
    small <- fits[[v]][[bl$small]]
    big <- fits[[v]][[bl$big]]

    a <- anova(small, big)
    h <- lmtest::waldtest(small, big, vcov = vcov_hc3, test = "F")

    q <- length(coef(big)) - length(coef(small))
    stopifnot(q > 0)
    hc3_F <- h$F[2]
    hc3_chisq <- hc3_F * q

    data.frame(
      version = v, comparison = b, block = bl$block,
      classical_num_df = a$Df[2],
      classical_den_df = df.residual(big),
      classical_F = a$F[2],
      classical_p = a$`Pr(>F)`[2],
      hc3_num_df = q,
      hc3_den_df = h$Res.Df[2],
      hc3_wald_chisq = hc3_chisq,
      hc3_p_chisq = pchisq(hc3_chisq, q, lower.tail = FALSE),
      hc3_F = hc3_F,
      hc3_p_F = h$`Pr(>F)`[2]
    )
  }))
}))

hist_tests <- interaction_tests[interaction_tests$version == "historical_common", ]
same_values(hist_tests$classical_F, old_joint$F, 1e-8, "joint F")
same_values(hist_tests$classical_p, old_joint$p, 1e-8, "joint p")


# descriptives and contrasts ----

cohort_labels <- c("1957-64", "1965-74", "1975-84", "1985-92")
edu_names <- c("junior_college", "bachelor", "postgraduate")
cells <- data.frame(cohort = rep(1:4, each = 3), edu_lvl = rep(1:3, 4))

cohort_education_descriptives <- do.call(rbind, lapply(names(versions), function(v) {
  d <- versions[[v]]
  do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    ii <- d$cohort == cells$cohort[i] & d$edu_lvl == cells$edu_lvl[i]
    y <- d$.y[ii]
    n_cell <- sum(ii)
    data.frame(version = v,
               cohort = cells$cohort[i], cohort_label = cohort_labels[cells$cohort[i]],
               edu_lvl = cells$edu_lvl[i], education = edu_names[cells$edu_lvl[i]],
               n = n_cell,
               mean_y = if (n_cell >= 5) mean(y) else NA_real_,
               sd_y = if (n_cell >= 5) sd(y) else NA_real_)
  }))
}))

coef_contrast <- function(old_v, new_v, name) {
  cols <- c("model", "term", "N", "estimate", "se_classical", "p_classical",
            "se_hc3", "p_hc3", "outcome_sd", "estimate_over_y_sd")
  z <- merge(coefficient_results[coefficient_results$version == old_v, cols],
             coefficient_results[coefficient_results$version == new_v, cols],
             by = c("model", "term"), suffixes = c("_old", "_new"))
  z$contrast <- name
  z$delta_B <- z$estimate_new - z$estimate_old
  z$delta_B_over_y_sd <- z$estimate_over_y_sd_new - z$estimate_over_y_sd_old
  z$delta_se_classical <- z$se_classical_new - z$se_classical_old
  z$delta_se_hc3 <- z$se_hc3_new - z$se_hc3_old
  z
}

fit_contrast <- function(old_v, new_v, name) {
  cols <- c("model", "N", "R2", "adjusted_R2", "residual_SE", "RSS")
  z <- merge(fit_statistics[fit_statistics$version == old_v, cols],
             fit_statistics[fit_statistics$version == new_v, cols],
             by = "model", suffixes = c("_old", "_new"))
  z$contrast <- name
  z$delta_R2 <- z$R2_new - z$R2_old
  z$delta_adjusted_R2 <- z$adjusted_R2_new - z$adjusted_R2_old
  z
}

# measurement, crosswalk choice, coverage, then old vs preferred new
coefficient_contrasts <- rbind(
  coef_contrast("historical_common", "syntax_common", "measurement"),
  coef_contrast("syntax_common", "overview_common", "crosswalk_choice"),
  coef_contrast("syntax_common", "syntax_full", "coverage"),
  coef_contrast("historical_common", "syntax_full", "historical_to_syntax_full")
)

fit_contrasts <- rbind(
  fit_contrast("historical_common", "syntax_common", "measurement"),
  fit_contrast("syntax_common", "overview_common", "crosswalk_choice"),
  fit_contrast("syntax_common", "syntax_full", "coverage"),
  fit_contrast("historical_common", "syntax_full", "historical_to_syntax_full")
)


# save ----

prov <- read.csv(prov_file)
stopifnot(nrow(prov) == 1)

write.csv(coefficient_results,
          file.path(out_dir, "06_all_model_coefficients.csv"),
          row.names = FALSE, na = "")
write.csv(coefficient_contrasts,
          file.path(out_dir, "06_coefficient_contrasts.csv"),
          row.names = FALSE, na = "")
write.csv(fit_statistics,
          file.path(out_dir, "06_model_fit_statistics.csv"),
          row.names = FALSE, na = "")
write.csv(fit_contrasts,
          file.path(out_dir, "06_model_fit_contrasts.csv"),
          row.names = FALSE, na = "")
write.csv(vif_results,
          file.path(out_dir, "06_vif_all_versions.csv"),
          row.names = FALSE, na = "")
write.csv(interaction_tests,
          file.path(out_dir, "06_interaction_joint_tests.csv"),
          row.names = FALSE, na = "")
write.csv(cohort_education_descriptives,
          file.path(out_dir, "06_cohort_education_descriptives.csv"),
          row.names = FALSE, na = "")

cat("\nISCO08ConveRsions", prov$version, "| Model 3, main terms\n")
print(coefficient_results[
  coefficient_results$model == "Model3" &
    coefficient_results$term %in% c("edu_bachelor", "edu_postgrad", "rural_origin"),
  c("version", "term", "N", "estimate", "se_classical", "p_classical", "se_hc3", "p_hc3")],
  row.names = FALSE)

cat("\nInteraction blocks\n")
print(interaction_tests[, c("version", "comparison", "classical_F", "classical_p", "hc3_F", "hc3_p_F")],
      row.names = FALSE)
