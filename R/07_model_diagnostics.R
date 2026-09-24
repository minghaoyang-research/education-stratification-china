# =============================================================================
# 07_model_diagnostics.R
#
# Diagnostics for the Stage 06 model comparison.
#
# Stage 07 re-runs Stage 06 to reconstruct the model objects used below. It
# does not change the Stage 06 model specifications. The additional analyses
# focus on the sparse postgraduate reference cell, classical-versus-HC3
# inference, and sensitivity of coefficients to alternative ISEI crosswalks.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

sign_class <- function(x, tol = 1e-10) {
  ifelse(abs(x) <= tol, 0L, ifelse(x > 0, 1L, -1L))
}

stage06_script <- "R/06_isei_model_comparison.R"
stage06_output <- "output/isei_model_comparison"
stage07_output <- "output/model_diagnostics"
run_started_07 <- Sys.time()

check(file.exists(stage06_script), paste0("Missing Stage 06 script: ", stage06_script))
if (!dir.exists(stage07_output)) dir.create(stage07_output, recursive = TRUE)

cat("\nStage 07: model diagnostics and measurement sensitivity\n")
cat("Re-running Stage 06 to reconstruct the model objects...\n")
cat("Stage 06 script MD5: ", unname(tools::md5sum(stage06_script)), "\n", sep = "")

stage06_console_07 <- capture.output(
  source(stage06_script, local = FALSE)
)

check(dir.exists(stage06_output), "Stage 06 did not create its output directory.")
check(exists("fits", inherits = FALSE), "Stage 06 did not create object 'fits'.")
check(exists("versions", inherits = FALSE), "Stage 06 did not create object 'versions'.")
check(exists("vcov_hc3", inherits = FALSE), "Stage 06 did not create function 'vcov_hc3'.")

version_order_07 <- c(
  "historical_common", "syntax_common", "overview_common",
  "syntax_full", "overview_full"
)
common_versions_07 <- c("historical_common", "syntax_common", "overview_common")
model_names_07 <- paste0("Model", 1:7)

check(identical(names(fits), version_order_07), "Unexpected Stage 06 version structure.")
check(all(common_versions_07 %in% names(versions)), "Required common-support data are unavailable.")
check(nobs(fits[["historical_common"]][["Model6"]]) == 1646L,
      "Historical common-support N is not 1646.")
check(nobs(fits[["syntax_full"]][["Model6"]]) == 1650L,
      "Syntax full N is not 1650.")

cat("Stage 06 reconstruction complete.\n")


# -----------------------------------------------------------------------------
# 1. Model 6 interaction-block Wald tests
# -----------------------------------------------------------------------------

model6_blocks <- list(
  all_6 = c("bach_x_c2", "bach_x_c3", "bach_x_c4",
            "post_x_c2", "post_x_c3", "post_x_c4"),
  bachelor_3 = c("bach_x_c2", "bach_x_c3", "bach_x_c4"),
  postgraduate_3 = c("post_x_c2", "post_x_c3", "post_x_c4")
)

wald_from_vcov_07 <- function(fit, terms, V, covariance_type) {
  b <- coef(fit)
  check(all(terms %in% names(b)) &&
          all(terms %in% rownames(V)) &&
          all(terms %in% colnames(V)),
        "Wald-test terms are missing from coefficients or covariance matrix.")

  beta <- b[terms]
  V_sub <- V[terms, terms, drop = FALSE]
  q <- length(terms)
  df2 <- df.residual(fit)

  check(all(is.finite(beta)) && all(is.finite(V_sub)) && qr(V_sub)$rank == q,
        "Wald covariance submatrix is not finite and full rank.")

  W <- as.numeric(crossprod(beta, solve(V_sub, beta)))
  F_stat <- W / q

  data.frame(
    covariance = covariance_type,
    q = q,
    df2 = df2,
    wald_chisq = W,
    F_stat = F_stat,
    p_F = pf(F_stat, df1 = q, df2 = df2, lower.tail = FALSE),
    p_chisq = pchisq(W, df = q, lower.tail = FALSE),
    vcov_sub_rcond = rcond(V_sub),
    stringsAsFactors = FALSE
  )
}

wald_results_07 <- list()
k_07 <- 1L

for (v in version_order_07) {
  fit <- fits[[v]][["Model6"]]
  check(inherits(fit, "lm"), paste0("Model 6 fit missing for version: ", v))

  V_classical <- vcov(fit)
  V_hc3 <- vcov_hc3(fit)

  for (block_name in names(model6_blocks)) {
    terms <- model6_blocks[[block_name]]
    a <- wald_from_vcov_07(fit, terms, V_classical, "classical")
    h <- wald_from_vcov_07(fit, terms, V_hc3, "HC3")
    a$version <- v
    a$block <- block_name
    h$version <- v
    h$block <- block_name
    wald_results_07[[k_07]] <- a
    k_07 <- k_07 + 1L
    wald_results_07[[k_07]] <- h
    k_07 <- k_07 + 1L
  }
}

model6_block_wald_07 <- do.call(rbind, wald_results_07)
model6_block_wald_07 <- model6_block_wald_07[, c(
  "version", "block", "covariance", "q", "df2", "wald_chisq",
  "F_stat", "p_F", "p_chisq", "vcov_sub_rcond"
)]
rownames(model6_block_wald_07) <- NULL

check(nrow(model6_block_wald_07) == 30L, "Expected 30 Model 6 block Wald tests.")
check(all(is.finite(model6_block_wald_07$wald_chisq)) &&
        all(is.finite(model6_block_wald_07$F_stat)) &&
        all(model6_block_wald_07$vcov_sub_rcond > 0),
      "Invalid Model 6 block-Wald result.")

# The six-df rows should agree with the Stage 06 results table generated in
# the same run. This is an internal consistency check rather than an external
# frozen benchmark.
joint06 <- read.csv(
  file.path(stage06_output, "06_interaction_joint_tests.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
anchor06 <- joint06[joint06$comparison == "Model5_vs_Model6", , drop = FALSE]
check(nrow(anchor06) == 5L, "Stage 06 Model 6 interaction table should contain five rows.")

for (v in version_order_07) {
  ref <- anchor06[anchor06$version == v, , drop = FALSE]
  calc_classical <- model6_block_wald_07[
    model6_block_wald_07$version == v &
      model6_block_wald_07$block == "all_6" &
      model6_block_wald_07$covariance == "classical", , drop = FALSE
  ]
  calc_hc3 <- model6_block_wald_07[
    model6_block_wald_07$version == v &
      model6_block_wald_07$block == "all_6" &
      model6_block_wald_07$covariance == "HC3", , drop = FALSE
  ]

  check(nrow(ref) == 1L && nrow(calc_classical) == 1L && nrow(calc_hc3) == 1L,
        paste0("Model 6 Wald row-count failure for ", v))
  check(abs(calc_classical$F_stat - ref$classical_F) < 1e-8,
        paste0("Classical six-df Wald mismatch for ", v))
  check(abs(calc_hc3$wald_chisq - ref$hc3_wald_chisq) < 1e-8 &&
          abs(calc_hc3$F_stat - ref$hc3_F) < 1e-8,
        paste0("HC3 six-df Wald mismatch for ", v))
}

write.csv(
  model6_block_wald_07,
  file.path(stage07_output, "07_model6_block_wald_tests.csv"),
  row.names = FALSE
)
cat("Model 6 block-Wald table written (30 rows).\n")


# -----------------------------------------------------------------------------
# 2. Classical-versus-HC3 standard errors
# -----------------------------------------------------------------------------

coef07 <- read.csv(
  file.path(stage06_output, "06_all_model_coefficients.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_coef_cols07 <- c(
  "version", "model", "term", "estimate", "se_classical", "p_classical",
  "se_hc3", "p_hc3"
)
check(all(required_coef_cols07 %in% names(coef07)),
      "Stage 06 coefficient file has an unexpected schema.")

se_terms07 <- c(
  "edu_postgrad", "bach_x_c2", "bach_x_c3", "bach_x_c4",
  "post_x_c2", "post_x_c3", "post_x_c4"
)

model6_se_ratio07 <- coef07[
  coef07$model == "Model6" & coef07$term %in% se_terms07,
  required_coef_cols07,
  drop = FALSE
]
check(nrow(model6_se_ratio07) == 35L,
      "Expected 35 Model 6 coefficient rows for the SE comparison.")

model6_se_ratio07$term_group <- ifelse(
  model6_se_ratio07$term == "edu_postgrad", "postgraduate_main",
  ifelse(grepl("^bach_x_", model6_se_ratio07$term),
         "bachelor_interaction", "postgraduate_interaction")
)
model6_se_ratio07$hc3_to_classical_se_ratio <-
  model6_se_ratio07$se_hc3 / model6_se_ratio07$se_classical

hc3_df07 <- vapply(
  model6_se_ratio07$version,
  function(v) df.residual(fits[[v]][["Model6"]]),
  numeric(1)
)
model6_se_ratio07$p_hc3_recomputed <-
  2 * pt(abs(model6_se_ratio07$estimate / model6_se_ratio07$se_hc3),
         df = hc3_df07, lower.tail = FALSE)
model6_se_ratio07$hc3_p_reference <- "t(df.residual); diagnostic"

check(all(is.finite(model6_se_ratio07$hc3_to_classical_se_ratio)) &&
        all(model6_se_ratio07$se_classical > 0) &&
        all(model6_se_ratio07$se_hc3 > 0),
      "Invalid standard error in Model 6 SE comparison.")
model6_se_ratio07$p_hc3_minus_recomputed_t <-
  model6_se_ratio07$p_hc3 - model6_se_ratio07$p_hc3_recomputed

model6_se_ratio07 <- model6_se_ratio07[
  order(match(model6_se_ratio07$version, version_order_07),
        match(model6_se_ratio07$term, se_terms07)), , drop = FALSE
]
rownames(model6_se_ratio07) <- NULL

write.csv(
  model6_se_ratio07,
  file.path(stage07_output, "07_model6_se_ratio_audit.csv"),
  row.names = FALSE
)
cat("Classical/HC3 standard-error comparison written (35 rows).\n")


# -----------------------------------------------------------------------------
# 3. Sparse postgraduate reference cell in Model 6
# -----------------------------------------------------------------------------

pg_terms_07 <- c("post_x_c2", "post_x_c3", "post_x_c4")
pg_influence_terms_07 <- c("edu_postgrad", pg_terms_07)

sparse_cell_diagnostics_07 <- function(v) {
  fit <- fits[[v]][["Model6"]]
  mf <- model.frame(fit)
  X <- model.matrix(fit)
  y <- as.numeric(model.response(mf))

  needed_mf <- c("edu_postgrad", "cohort2", "cohort3", "cohort4")
  check(all(needed_mf %in% names(mf)), paste0("Missing Model 6 variables: ", v))
  check(all(pg_influence_terms_07 %in% colnames(X)),
        paste0("Missing Model 6 design columns: ", v))

  idx <- which(
    mf$edu_postgrad == 1 & mf$cohort2 == 0 & mf$cohort3 == 0 & mf$cohort4 == 0
  )
  check(length(idx) == 2L,
        paste0("Expected exactly two postgraduate x cohort1 observations: ", v))

  d <- versions[[v]]
  k <- match(rownames(mf), rownames(d))
  check(!anyNA(k), paste0("Could not align Model 6 rows to version data: ", v))
  cell_isco <- unique(as.numeric(d$isco_main[k[idx]]))
  check(length(cell_isco) == 1L, paste0("Sparse cell spans multiple ISCO codes: ", v))
  check(diff(range(y[idx])) < 1e-12,
        paste0("Sparse-cell outcome is not constant within mapping: ", v))

  h <- hatvalues(fit)
  e <- residuals(fit)
  sigma2 <- sum(e^2) / df.residual(fit)
  hc3_weight <- (e / (1 - h))^2

  A <- solve(crossprod(X), t(X))
  rownames(A) <- colnames(X)
  A_cell <- A[pg_influence_terms_07, idx, drop = FALSE]
  G <- A[pg_terms_07, , drop = FALSE]

  V_hc3_sub <- vcov_hc3(fit)[pg_terms_07, pg_terms_07, drop = FALSE]
  V_reconstructed <- sweep(G, 2, hc3_weight, "*") %*% t(G)
  check(max(abs(V_reconstructed - V_hc3_sub)) < 1e-8,
        paste0("HC3 covariance reconstruction failed: ", v))

  beta_pg <- coef(fit)[pg_terms_07]
  z <- solve(V_hc3_sub, beta_pg)
  W <- as.numeric(crossprod(beta_pg, z))
  check(is.finite(W) && W > 0, paste0("Invalid postgraduate HC3 Wald statistic: ", v))

  obs_cov_trace <- hc3_weight * colSums(G^2)
  zGi <- as.numeric(crossprod(z, G))
  obs_wald_direction <- hc3_weight * zGi^2

  classical_cell_share <- vapply(
    pg_influence_terms_07,
    function(term) sum(A[term, idx]^2) / sum(A[term, ]^2),
    numeric(1)
  )
  largest_single_share <- vapply(
    pg_influence_terms_07,
    function(term) max(A[term, idx]^2) / sum(A[term, ]^2),
    numeric(1)
  )

  # With a constant outcome and residuals summing to zero within this two-case
  # cell, the two residuals are exactly +/- half the fitted-value contrast.
  dx <- X[idx[1], ] - X[idx[2], ]
  design_contrast <- sum(dx * coef(fit))
  expected_pair <- c(-0.5 * design_contrast, 0.5 * design_contrast)
  pair_identity_error <- max(abs(unname(e[idx]) - expected_pair))

  data.frame(
    version = v,
    N_model = nrow(X),
    p_model = ncol(X),
    N_pg_cohort1 = length(idx),
    isco_main = cell_isco,
    cell_outcome = y[idx[1]],
    mean_model_leverage = mean(h),
    mean_pg_c1_leverage = mean(h[idx]),
    max_pg_c1_leverage = max(h[idx]),
    leverage_multiple = mean(h[idx]) / mean(h),
    mean_hc3_leverage_correction = mean(1 / (1 - h[idx])^2),
    mean_abs_residual = mean(abs(e[idx])),
    max_abs_residual = max(abs(e[idx])),
    cell_residual_sum = sum(e[idx]),
    two_case_identity_error = pair_identity_error,
    local_mse_over_pooled_sigma2 = mean(e[idx]^2) / sigma2,
    sum_hc3_weight = sum(hc3_weight[idx]),
    hc3_covariance_trace_share = sum(obs_cov_trace[idx]) / sum(obs_cov_trace),
    hc3_wald_direction_share = sum(obs_wald_direction[idx]) / W,
    classical_cell_share_edu_postgrad = classical_cell_share[["edu_postgrad"]],
    classical_cell_share_post_x_c2 = classical_cell_share[["post_x_c2"]],
    classical_cell_share_post_x_c3 = classical_cell_share[["post_x_c3"]],
    classical_cell_share_post_x_c4 = classical_cell_share[["post_x_c4"]],
    largest_single_share_edu_postgrad = largest_single_share[["edu_postgrad"]],
    largest_single_share_post_x_c2 = largest_single_share[["post_x_c2"]],
    largest_single_share_post_x_c3 = largest_single_share[["post_x_c3"]],
    largest_single_share_post_x_c4 = largest_single_share[["post_x_c4"]],
    d_sum_edu_postgrad = sum(A_cell["edu_postgrad", ]),
    d_sum_post_x_c2 = sum(A_cell["post_x_c2", ]),
    d_sum_post_x_c3 = sum(A_cell["post_x_c3", ]),
    d_sum_post_x_c4 = sum(A_cell["post_x_c4", ]),
    max_abs_individual_derivative = max(abs(A_cell)),
    postgraduate_hc3_W = W,
    stringsAsFactors = FALSE
  )
}

pg_reference_audit07 <- do.call(
  rbind,
  lapply(version_order_07, sparse_cell_diagnostics_07)
)
rownames(pg_reference_audit07) <- NULL

syntax_weight_07 <- pg_reference_audit07$sum_hc3_weight[
  pg_reference_audit07$version == "syntax_common"
]
historical_weight_07 <- pg_reference_audit07$sum_hc3_weight[
  pg_reference_audit07$version == "historical_common"
]
pg_reference_audit07$hc3_weight_ratio_to_syntax_common <-
  pg_reference_audit07$sum_hc3_weight / syntax_weight_07
pg_reference_audit07$hc3_weight_ratio_to_historical_common <-
  pg_reference_audit07$sum_hc3_weight / historical_weight_07

check(all(pg_reference_audit07$N_pg_cohort1 == 2L), "Sparse-cell size changed.")
check(all(pg_reference_audit07$isco_main == 1120), "Sparse-cell ISCO code is not 1120.")
check(max(abs(pg_reference_audit07$cell_residual_sum)) < 1e-8,
      "Sparse-cell residuals do not sum to zero.")
check(max(pg_reference_audit07$two_case_identity_error) < 1e-10,
      "Two-case residual identity failed.")
check(max(abs(pg_reference_audit07$d_sum_edu_postgrad - 1)) < 1e-8 &&
        max(abs(pg_reference_audit07$d_sum_post_x_c2 + 1)) < 1e-8 &&
        max(abs(pg_reference_audit07$d_sum_post_x_c3 + 1)) < 1e-8 &&
        max(abs(pg_reference_audit07$d_sum_post_x_c4 + 1)) < 1e-8,
      "Sparse-cell derivative identity failed.")

share_cols_07 <- grep("^classical_cell_share_", names(pg_reference_audit07), value = TRUE)
for (nm in share_cols_07) {
  check(diff(range(pg_reference_audit07[1:3, nm])) < 1e-12,
        paste0("Common-support classical variance share differs across mappings: ", nm))
}

write.csv(
  pg_reference_audit07,
  file.path(stage07_output, "07_model6_sparse_reference_cell_audit.csv"),
  row.names = FALSE
)

cat("Sparse Model 6 postgraduate reference-cell diagnostics written.\n")
cat("  ISCO: 1120; N in cell: 2\n")
cat("  Overview/syntax HC3 raw-weight ratio: ",
    format(pg_reference_audit07$hc3_weight_ratio_to_syntax_common[
      pg_reference_audit07$version == "overview_common"
    ], digits = 8), "\n", sep = "")
cat("  Overview/historical HC3 raw-weight ratio: ",
    format(pg_reference_audit07$hc3_weight_ratio_to_historical_common[
      pg_reference_audit07$version == "overview_common"
    ], digits = 8), "\n", sep = "")


# -----------------------------------------------------------------------------
# 4. Selected coefficient and sign sensitivity
# -----------------------------------------------------------------------------

control_term_map_07 <- function(fit) {
  nms <- names(coef(fit))
  out <- list()
  k <- 1L

  for (term in c("female", "age", "par_edu_yrs")) {
    if (term %in% nms) {
      out[[k]] <- data.frame(term = term, coefficient_name = term,
                             stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }

  rural_nm <- grep("^rural_origin(_f)?$", nms, value = TRUE)
  check(length(rural_nm) <= 1L, "More than one rural-origin coefficient found.")
  if (length(rural_nm) == 1L) {
    out[[k]] <- data.frame(term = "rural_origin", coefficient_name = rural_nm,
                           stringsAsFactors = FALSE)
  }

  if (length(out) == 0L) {
    return(data.frame(term = character(), coefficient_name = character()))
  }
  do.call(rbind, out)
}

extract_control_sensitivity_07 <- function(version_name, model_name) {
  fit <- fits[[version_name]][[model_name]]
  mapping <- control_term_map_07(fit)
  if (nrow(mapping) == 0L) return(NULL)

  classical <- summary(fit)$coefficients
  V_hc3 <- vcov_hc3(fit)
  hc3_se <- sqrt(diag(V_hc3))
  y <- as.numeric(model.response(model.frame(fit)))
  y_sd <- sd(y)

  do.call(rbind, lapply(seq_len(nrow(mapping)), function(i) {
    term <- mapping$term[i]
    coef_name <- mapping$coefficient_name[i]
    b <- unname(coef(fit)[coef_name])
    se_classical <- unname(classical[coef_name, "Std. Error"])
    p_classical <- unname(classical[coef_name, "Pr(>|t|)"])
    se_hc3 <- unname(hc3_se[coef_name])
    p_hc3 <- 2 * pt(abs(b / se_hc3), df = df.residual(fit), lower.tail = FALSE)

    data.frame(
      version = version_name,
      model = model_name,
      term = term,
      coefficient_name = coef_name,
      N = nobs(fit),
      outcome_sd = y_sd,
      estimate = b,
      estimate_over_y_sd = b / y_sd,
      se_classical = se_classical,
      p_classical = p_classical,
      se_hc3 = se_hc3,
      p_hc3 = p_hc3,
      stringsAsFactors = FALSE
    )
  }))
}

control_sensitivity_07 <- do.call(
  rbind,
  lapply(common_versions_07, function(v) {
    do.call(rbind, lapply(model_names_07, function(m) {
      extract_control_sensitivity_07(v, m)
    }))
  })
)
rownames(control_sensitivity_07) <- NULL
check(nrow(control_sensitivity_07) > 0L, "Control-sensitivity table is empty.")
check(all(is.finite(control_sensitivity_07$estimate)), "Non-finite control coefficient found.")

# Explicit syntax-versus-overview sign comparison for the selected main effects.
main_s <- control_sensitivity_07[
  control_sensitivity_07$version == "syntax_common",
  c("model", "term", "coefficient_name", "estimate"),
  drop = FALSE
]
main_o <- control_sensitivity_07[
  control_sensitivity_07$version == "overview_common",
  c("model", "term", "coefficient_name", "estimate"),
  drop = FALSE
]
main_sign_07 <- merge(main_s, main_o, by = c("model", "term"),
                      suffixes = c("_syntax", "_overview"), sort = FALSE)
main_sign_07$sign_syntax <- sign_class(main_sign_07$estimate_syntax)
main_sign_07$sign_overview <- sign_class(main_sign_07$estimate_overview)
main_sign_07$sign_changed <- main_sign_07$sign_syntax != main_sign_07$sign_overview

female_changes_07 <- main_sign_07[
  main_sign_07$term == "female" & main_sign_07$sign_changed, , drop = FALSE
]
nonfemale_changes_07 <- main_sign_07[
  main_sign_07$term != "female" & main_sign_07$sign_changed, , drop = FALSE
]
check(nrow(female_changes_07) == 7L &&
        setequal(female_changes_07$model, model_names_07),
      "Female main-effect sign reversal does not occur in all seven models.")
check(nrow(nonfemale_changes_07) == 0L,
      "A selected non-female main effect changes sign.")

# Model 6 interaction coefficients can change sign even when the corresponding
# cohort-specific education effects do not.
interaction_terms_07 <- c(
  "bach_x_c2", "bach_x_c3", "bach_x_c4",
  "post_x_c2", "post_x_c3", "post_x_c4"
)
b6_s <- coef(fits[["syntax_common"]][["Model6"]])
b6_o <- coef(fits[["overview_common"]][["Model6"]])
check(all(interaction_terms_07 %in% names(b6_s)) &&
        all(interaction_terms_07 %in% names(b6_o)),
      "Model 6 interaction terms are unavailable.")

interaction_sign_07 <- data.frame(
  type = "model6_interaction",
  model = "Model6",
  term = interaction_terms_07,
  estimate_syntax = unname(b6_s[interaction_terms_07]),
  estimate_overview = unname(b6_o[interaction_terms_07]),
  stringsAsFactors = FALSE
)
interaction_sign_07$sign_changed <-
  sign_class(interaction_sign_07$estimate_syntax) !=
  sign_class(interaction_sign_07$estimate_overview)

cohort_effects_07 <- function(fit) {
  b <- coef(fit)
  required <- c("edu_bachelor", "edu_postgrad", interaction_terms_07)
  check(all(required %in% names(b)), "Model 6 education terms are incomplete.")

  data.frame(
    education = rep(c("bachelor", "postgraduate"), each = 4L),
    cohort = rep(paste0("cohort", 1:4), times = 2L),
    effect = c(
      b["edu_bachelor"],
      b["edu_bachelor"] + b["bach_x_c2"],
      b["edu_bachelor"] + b["bach_x_c3"],
      b["edu_bachelor"] + b["bach_x_c4"],
      b["edu_postgrad"],
      b["edu_postgrad"] + b["post_x_c2"],
      b["edu_postgrad"] + b["post_x_c3"],
      b["edu_postgrad"] + b["post_x_c4"]
    ),
    stringsAsFactors = FALSE
  )
}

cohort_s <- cohort_effects_07(fits[["syntax_common"]][["Model6"]])
cohort_o <- cohort_effects_07(fits[["overview_common"]][["Model6"]])
cohort_sign_07 <- merge(cohort_s, cohort_o, by = c("education", "cohort"),
                        suffixes = c("_syntax", "_overview"), sort = FALSE)
cohort_sign_07$type <- "model6_cohort_effect"
cohort_sign_07$model <- "Model6"
cohort_sign_07$term <- paste(cohort_sign_07$education, cohort_sign_07$cohort, sep = "_")
cohort_sign_07$sign_changed <-
  sign_class(cohort_sign_07$effect_syntax) != sign_class(cohort_sign_07$effect_overview)
check(!any(cohort_sign_07$sign_changed),
      "A cohort-specific education effect changes sign between syntax and overview.")

sign_output_07 <- rbind(
  data.frame(
    type = "selected_main_effect",
    model = main_sign_07$model,
    term = main_sign_07$term,
    estimate_syntax = main_sign_07$estimate_syntax,
    estimate_overview = main_sign_07$estimate_overview,
    sign_changed = main_sign_07$sign_changed,
    stringsAsFactors = FALSE
  ),
  interaction_sign_07[, c("type", "model", "term", "estimate_syntax",
                           "estimate_overview", "sign_changed")],
  data.frame(
    type = cohort_sign_07$type,
    model = cohort_sign_07$model,
    term = cohort_sign_07$term,
    estimate_syntax = cohort_sign_07$effect_syntax,
    estimate_overview = cohort_sign_07$effect_overview,
    sign_changed = cohort_sign_07$sign_changed,
    stringsAsFactors = FALSE
  )
)

write.csv(
  control_sensitivity_07,
  file.path(stage07_output, "07_crosswalk_control_sensitivity.csv"),
  row.names = FALSE
)
write.csv(
  sign_output_07,
  file.path(stage07_output, "07_crosswalk_sign_sensitivity.csv"),
  row.names = FALSE
)

cat("Crosswalk sign comparison written.\n")
cat("  Female main-effect sign reversals: ", nrow(female_changes_07), " of 7 models\n", sep = "")
cat("  Model 6 interaction sign reversals: ", sum(interaction_sign_07$sign_changed), " of 6\n", sep = "")
cat("  Model 6 cohort-specific education-effect sign reversals: ",
    sum(cohort_sign_07$sign_changed), " of 8\n", sep = "")


# -----------------------------------------------------------------------------
# 5. Common-support compression benchmark
# -----------------------------------------------------------------------------

d_h_07 <- versions[["historical_common"]]
d_s_07 <- versions[["syntax_common"]]
d_o_07 <- versions[["overview_common"]]
required_version_vars_07 <- c("id", "isco_main", "female", ".y")

for (d in list(d_h_07, d_s_07, d_o_07)) {
  check(is.data.frame(d) && all(required_version_vars_07 %in% names(d)),
        "Required variables are missing from common-support data.")
  check(nrow(d) == 1646L, "Common-support sample size is not 1,646.")
}
check(identical(d_h_07$id, d_s_07$id) && identical(d_s_07$id, d_o_07$id),
      "Common-support respondent ordering differs across mappings.")
check(identical(rownames(d_h_07), rownames(d_s_07)) &&
        identical(rownames(d_s_07), rownames(d_o_07)),
      "Common-support row names differ across mappings.")

X1_s <- model.matrix(fits[["syntax_common"]][["Model1"]])
X1_o <- model.matrix(fits[["overview_common"]][["Model1"]])
check(identical(colnames(X1_s), colnames(X1_o)) && max(abs(X1_s - X1_o)) < 1e-12,
      "Common-support Model 1 design matrices differ.")

y_h_common_07 <- as.numeric(d_h_07$.y)
y_s_common_07 <- as.numeric(d_s_07$.y)
y_o_common_07 <- as.numeric(d_o_07$.y)
isco_common_07 <- as.numeric(d_s_07$isco_main)

check(all(is.finite(y_h_common_07)) && all(is.finite(y_s_common_07)) &&
        all(is.finite(y_o_common_07)) && all(is.finite(isco_common_07)),
      "Non-finite common-support mapping value found.")

compression_fit_07 <- lm(y_o_common_07 ~ y_s_common_07)
compression_intercept_07 <- unname(coef(compression_fit_07)[1])
compression_slope_07 <- unname(coef(compression_fit_07)[2])
compression_R2_07 <- summary(compression_fit_07)$r.squared
compression_residual_direct_07 <-
  y_o_common_07 - (compression_intercept_07 + compression_slope_07 * y_s_common_07)
compression_residual_lm_07 <- as.numeric(residuals(compression_fit_07))
check(max(abs(compression_residual_direct_07 - compression_residual_lm_07)) < 1e-10,
      "Direct and lm compression residuals disagree.")

# Scale-free orthogonality check for the OLS compression residuals.
Z_07 <- cbind(intercept = 1, syntax = y_s_common_07)
orthogonality_relative_07 <-
  abs(drop(crossprod(Z_07, compression_residual_direct_07))) /
  (sqrt(colSums(Z_07^2)) * sqrt(sum(compression_residual_direct_07^2)))
check(max(orthogonality_relative_07) < 1e-12,
      "Compression residuals are not numerically orthogonal to the fitted regressors.")

isco_split_07 <- split(seq_along(isco_common_07), isco_common_07)
code_compression_07 <- do.call(rbind, lapply(isco_split_07, function(ii) {
  check(diff(range(y_s_common_07[ii])) < 1e-12,
        "Syntax score is not deterministic within exact ISCO code.")
  check(diff(range(y_o_common_07[ii])) < 1e-12,
        "Overview score is not deterministic within exact ISCO code.")
  check(diff(range(compression_residual_direct_07[ii])) < 1e-12,
        "Compression residual is not deterministic within exact ISCO code.")

  data.frame(
    isco_main = isco_common_07[ii[1]],
    N = length(ii),
    syntax = y_s_common_07[ii[1]],
    overview = y_o_common_07[ii[1]],
    residual_from_compression = compression_residual_direct_07[ii[1]],
    stringsAsFactors = FALSE
  )
}))
rownames(code_compression_07) <- NULL
check(nrow(code_compression_07) == 247L, "Expected 247 common-support ISCO codes.")
check(sum(code_compression_07$N) == 1646L, "ISCO counts do not reconstruct common-support N.")

code_compression_fit_07 <- lm(overview ~ syntax, data = code_compression_07, weights = N)
check(max(abs(coef(code_compression_fit_07) - coef(compression_fit_07))) < 1e-10,
      "Person-level and code-weighted compression lines differ.")

compression_benchmark_07 <- data.frame(
  N_persons = 1646L,
  N_ISCO_codes = 247L,
  intercept = compression_intercept_07,
  slope = compression_slope_07,
  R2 = compression_R2_07,
  historical_mean = mean(y_h_common_07),
  syntax_mean = mean(y_s_common_07),
  overview_mean = mean(y_o_common_07),
  syntax_sd = sd(y_s_common_07),
  overview_sd = sd(y_o_common_07),
  residual_mean = mean(compression_residual_direct_07),
  max_lm_vs_direct_residual_difference =
    max(abs(compression_residual_lm_07 - compression_residual_direct_07)),
  max_relative_orthogonality = max(orthogonality_relative_07),
  stringsAsFactors = FALSE
)

major_group_labels_07 <- c(
  "0" = "Armed forces occupations",
  "1" = "Managers",
  "2" = "Professionals",
  "3" = "Technicians and associate professionals",
  "4" = "Clerical support workers",
  "5" = "Service and sales workers",
  "6" = "Skilled agricultural, forestry and fishery workers",
  "7" = "Craft and related trades workers",
  "8" = "Plant and machine operators and assemblers",
  "9" = "Elementary occupations"
)
major_group_07 <- floor(isco_common_07 / 1000)
major_split_07 <- split(seq_along(major_group_07), major_group_07)
major_group_summary_07 <- do.call(rbind, lapply(major_split_07, function(ii) {
  g <- as.character(major_group_07[ii[1]])
  data.frame(
    major_group = as.integer(g),
    label = unname(major_group_labels_07[g]),
    N = length(ii),
    historical = mean(y_h_common_07[ii]),
    syntax = mean(y_s_common_07[ii]),
    overview = mean(y_o_common_07[ii]),
    historical_to_overview = mean(y_o_common_07[ii] - y_h_common_07[ii]),
    syntax_to_overview = mean(y_o_common_07[ii] - y_s_common_07[ii]),
    stringsAsFactors = FALSE
  )
}))
rownames(major_group_summary_07) <- NULL
major_group_summary_07 <- major_group_summary_07[order(major_group_summary_07$major_group), ]

write.csv(
  compression_benchmark_07,
  file.path(stage07_output, "07_syntax_overview_compression_benchmark.csv"),
  row.names = FALSE
)
write.csv(
  major_group_summary_07,
  file.path(stage07_output, "07_crosswalk_major_group_means.csv"),
  row.names = FALSE
)

cat("Common-support compression benchmark written.\n")
cat("  Slope: ", format(compression_slope_07, digits = 10), "\n", sep = "")
cat("  R2: ", format(compression_R2_07, digits = 10), "\n", sep = "")


# -----------------------------------------------------------------------------
# 6. Coefficient decomposition
# -----------------------------------------------------------------------------

decompose_model_07 <- function(model_name) {
  fit_s <- fits[["syntax_common"]][[model_name]]
  fit_o <- fits[["overview_common"]][[model_name]]
  mf_s <- model.frame(fit_s)
  mf_o <- model.frame(fit_o)
  X_s <- model.matrix(fit_s)
  X_o <- model.matrix(fit_o)

  check(identical(rownames(mf_s), rownames(mf_o)),
        paste0(model_name, ": syntax and overview row ordering differs."))
  check(identical(colnames(X_s), colnames(X_o)) && max(abs(X_s - X_o)) < 1e-12,
        paste0(model_name, ": syntax and overview design matrices differ."))

  k <- match(rownames(mf_s), rownames(d_s_07))
  check(!anyNA(k), paste0(model_name, ": model rows cannot be aligned to common support."))
  r <- compression_residual_direct_07[k]

  beta_s <- coef(fit_s)
  beta_o <- coef(fit_o)
  beta_r <- lm.fit(X_s, r)$coefficients
  names(beta_r) <- colnames(X_s)
  check(identical(names(beta_s), names(beta_o)) && identical(names(beta_s), names(beta_r)),
        paste0(model_name, ": coefficient names differ across decomposition pieces."))
  check(all(is.finite(beta_r)), paste0(model_name, ": residual regression is not finite."))

  pure <- compression_slope_07 * beta_s
  pure["(Intercept)"] <- pure["(Intercept)"] + compression_intercept_07
  beta_o_reconstructed <- pure + beta_r
  reconstruction_error <- beta_o - beta_o_reconstructed

  check(max(abs(reconstruction_error)) < 1e-9,
        paste0(model_name, ": overview coefficients are not reconstructed by the compression decomposition."))

  actual_change <- beta_o - beta_s
  compression_component <- pure - beta_s
  occupation_specific_component <- beta_r
  reconstructed_change <- compression_component + occupation_specific_component

  departure_pct <- rep(NA_real_, length(pure))
  names(departure_pct) <- names(pure)
  ok <- abs(pure) > 1e-12
  departure_pct[ok] <- 100 * occupation_specific_component[ok] / pure[ok]

  data.frame(
    model = model_name,
    term = names(beta_s),
    N = nobs(fit_s),
    beta_syntax = unname(beta_s),
    beta_if_pure_compression = unname(pure),
    beta_overview = unname(beta_o),
    compression_component = unname(compression_component),
    occupation_specific_component = unname(occupation_specific_component),
    actual_change = unname(actual_change),
    reconstructed_change = unname(reconstructed_change),
    reconstruction_error = unname(reconstruction_error),
    departure_from_pure_compression_pct = unname(departure_pct),
    stringsAsFactors = FALSE
  )
}

all_decompositions_07 <- lapply(model_names_07, decompose_model_07)
names(all_decompositions_07) <- model_names_07

female_decomposition_07 <- do.call(rbind, lapply(all_decompositions_07, function(d) {
  z <- d[d$term == "female", , drop = FALSE]
  check(nrow(z) == 1L, paste0(d$model[1], ": female coefficient unavailable."))
  z$delta_actual <- z$actual_change
  z$delta_reconstructed <- z$reconstructed_change
  z$compression_share_of_net <- z$compression_component / z$actual_change
  z$occupation_specific_share_of_net <- z$occupation_specific_component / z$actual_change
  z[, c(
    "model", "N", "beta_syntax", "beta_overview", "delta_actual",
    "beta_if_pure_compression", "compression_component",
    "occupation_specific_component", "compression_share_of_net",
    "occupation_specific_share_of_net", "delta_reconstructed",
    "reconstruction_error"
  )]
}))
rownames(female_decomposition_07) <- NULL

model3_decomposition_07 <- all_decompositions_07[["Model3"]]
model3_decomposition_07 <- model3_decomposition_07[
  model3_decomposition_07$term != "(Intercept)", , drop = FALSE
]

write.csv(
  female_decomposition_07,
  file.path(stage07_output, "07_female_coefficient_decomposition.csv"),
  row.names = FALSE
)
write.csv(
  model3_decomposition_07,
  file.path(stage07_output, "07_model3_coefficient_decomposition.csv"),
  row.names = FALSE
)

cat("Coefficient decomposition written.\n")
cat("  Model 3 non-intercept coefficients: ", nrow(model3_decomposition_07), "\n", sep = "")


# -----------------------------------------------------------------------------
# 7. Exact ISCO decomposition of the occupation-specific female component
# -----------------------------------------------------------------------------

female_weights_07 <- function(fit) {
  X <- model.matrix(fit)
  female_pos <- match("female", colnames(X))
  check(!is.na(female_pos), "Female design column unavailable.")

  A <- solve(crossprod(X), t(X))
  weight_matrix <- as.numeric(A[female_pos, ])

  female_x <- X[, female_pos]
  X_other <- X[, -female_pos, drop = FALSE]
  female_residual <- lm.fit(X_other, female_x)$residuals
  denom <- sum(female_residual * female_x)
  check(abs(denom) > 1e-12, "Female FWL denominator is effectively zero.")
  weight_fwl <- female_residual / denom

  max_difference <- max(abs(weight_matrix - weight_fwl))
  check(max_difference < 1e-10, "Matrix and FWL female weights disagree.")
  check(abs(sum(weight_fwl)) < 1e-10, "Female coefficient weights do not sum to zero.")

  list(weights = weight_fwl, weight_sum = sum(weight_fwl),
       max_matrix_fwl_difference = max_difference)
}

make_residual_code_table_07 <- function(model_name) {
  fit_s <- fits[["syntax_common"]][[model_name]]
  fit_o <- fits[["overview_common"]][[model_name]]
  mf_s <- model.frame(fit_s)
  mf_o <- model.frame(fit_o)
  X_s <- model.matrix(fit_s)
  X_o <- model.matrix(fit_o)

  check(identical(rownames(mf_s), rownames(mf_o)) &&
          identical(colnames(X_s), colnames(X_o)) && max(abs(X_s - X_o)) < 1e-12,
        paste0(model_name, ": common-support alignment failed."))

  k <- match(rownames(mf_s), rownames(d_s_07))
  check(!anyNA(k), paste0(model_name, ": rows cannot be matched to common-support data."))

  y_s <- as.numeric(model.response(mf_s))
  y_o <- as.numeric(model.response(mf_o))
  isco <- as.numeric(d_s_07$isco_main[k])
  female <- as.numeric(mf_s$female)
  r <- y_o - (compression_intercept_07 + compression_slope_07 * y_s)
  check(max(abs(r - compression_residual_direct_07[k])) < 1e-12,
        paste0(model_name, ": compression residual alignment failed."))

  w_info <- female_weights_07(fit_s)
  w <- w_info$weights
  residual_beta <- sum(w * r)
  expected_beta <- unname(
    coef(fit_o)[["female"]] - compression_slope_07 * coef(fit_s)[["female"]]
  )
  check(abs(residual_beta - expected_beta) < 1e-10,
        paste0(model_name, ": exact residual female coefficient failed."))

  sp <- split(seq_along(isco), isco)
  out <- do.call(rbind, lapply(sp, function(ii) {
    check(diff(range(y_s[ii])) < 1e-12 &&
            diff(range(y_o[ii])) < 1e-12 &&
            diff(range(r[ii])) < 1e-12,
          paste0(model_name, ": non-deterministic value within exact ISCO code."))

    data.frame(
      isco_main = isco[ii[1]],
      N = length(ii),
      female_share = mean(female[ii]),
      syntax = y_s[ii[1]],
      overview = y_o[ii[1]],
      compression_pred = compression_intercept_07 + compression_slope_07 * y_s[ii[1]],
      residual_from_compression = r[ii[1]],
      female_weight_sum = sum(w[ii]),
      contribution = sum(w[ii]) * r[ii[1]],
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL

  check(abs(sum(out$female_weight_sum)) < 1e-10,
        paste0(model_name, ": ISCO female weights do not sum to zero."))
  check(abs(sum(out$contribution) - residual_beta) < 1e-10,
        paste0(model_name, ": ISCO contributions do not reconstruct the residual component."))

  out <- out[order(-abs(out$contribution)), , drop = FALSE]
  out$abs_share <- abs(out$contribution) / sum(abs(out$contribution))
  out$cum_abs_share <- cumsum(out$abs_share)
  out$share_of_residual_net <- out$contribution / residual_beta
  out$cum_share_of_residual_net <- cumsum(out$contribution) / residual_beta
  attr(out, "residual_beta") <- residual_beta
  attr(out, "weight_sum") <- w_info$weight_sum
  attr(out, "max_matrix_fwl_difference") <- w_info$max_matrix_fwl_difference
  out
}

make_public_residual_top_07 <- function(d, model_name, n_top = 15L) {
  big <- d[d$N >= 5L, , drop = FALSE]
  small <- d[d$N < 5L, , drop = FALSE]

  z <- data.frame(
    model = model_name,
    group = "ISCO",
    isco_main = big$isco_main,
    N_reported = big$N,
    female_share_reported = big$female_share,
    syntax = big$syntax,
    overview = big$overview,
    compression_pred = big$compression_pred,
    residual_from_compression = big$residual_from_compression,
    female_weight_sum = big$female_weight_sum,
    contribution = big$contribution,
    stringsAsFactors = FALSE
  )

  if (nrow(small) > 0L) {
    pooled <- data.frame(
      model = model_name,
      group = "small_cells_pooled",
      isco_main = NA_real_,
      N_reported = sum(small$N),
      female_share_reported = NA_real_,
      syntax = NA_real_,
      overview = NA_real_,
      compression_pred = NA_real_,
      residual_from_compression = NA_real_,
      female_weight_sum = sum(small$female_weight_sum),
      contribution = sum(small$contribution),
      stringsAsFactors = FALSE
    )
    z <- rbind(z, pooled)
  }

  z <- z[order(-abs(z$contribution)), , drop = FALSE]
  z$abs_share <- abs(z$contribution) / sum(abs(z$contribution))
  z$cum_abs_share <- cumsum(z$abs_share)
  z$share_of_residual_net <- z$contribution / attr(d, "residual_beta")
  z$cum_share_of_residual_net <- cumsum(z$contribution) / attr(d, "residual_beta")
  head(z, n_top)
}

residual_codes_M1_07 <- make_residual_code_table_07("Model1")
residual_codes_M6_07 <- make_residual_code_table_07("Model6")
public_residual_top_07 <- rbind(
  make_public_residual_top_07(residual_codes_M1_07, "Model1"),
  make_public_residual_top_07(residual_codes_M6_07, "Model6")
)
rownames(public_residual_top_07) <- NULL

write.csv(
  public_residual_top_07,
  file.path(stage07_output, "07_female_isco_residual_top_contributions.csv"),
  row.names = FALSE,
  na = ""
)
cat("ISCO decomposition of the occupation-specific female component written.\n")


# -----------------------------------------------------------------------------
# 8. Raw crosswalk decomposition of the female coefficient
# -----------------------------------------------------------------------------

make_female_crosswalk_audit_07 <- function(model_name, from_version, to_version) {
  fit_from <- fits[[from_version]][[model_name]]
  fit_to <- fits[[to_version]][[model_name]]
  mf_from <- model.frame(fit_from)
  mf_to <- model.frame(fit_to)
  X_from <- model.matrix(fit_from)
  X_to <- model.matrix(fit_to)

  check(identical(rownames(mf_from), rownames(mf_to)) &&
          identical(colnames(X_from), colnames(X_to)) &&
          max(abs(X_from - X_to)) < 1e-12,
        paste0(model_name, ": crosswalk design matrices differ."))

  w_info <- female_weights_07(fit_from)
  w <- w_info$weights
  y_from <- as.numeric(model.response(mf_from))
  y_to <- as.numeric(model.response(mf_to))
  raw_difference <- y_to - y_from
  beta_change <- unname(coef(fit_to)[["female"]] - coef(fit_from)[["female"]])
  reconstructed_change <- sum(w * raw_difference)
  check(abs(beta_change - reconstructed_change) < 1e-10,
        paste0(model_name, ": raw crosswalk change does not reconstruct the female coefficient change."))

  d_from <- versions[[from_version]]
  k <- match(rownames(mf_from), rownames(d_from))
  check(!anyNA(k), paste0(model_name, ": crosswalk rows cannot be matched to version data."))
  isco <- as.numeric(d_from$isco_main[k])
  female <- as.numeric(mf_from$female)

  sp <- split(seq_along(isco), isco)
  code_table <- do.call(rbind, lapply(sp, function(ii) {
    check(diff(range(y_from[ii])) < 1e-12 &&
            diff(range(y_to[ii])) < 1e-12 &&
            diff(range(raw_difference[ii])) < 1e-12,
          paste0(model_name, ": non-deterministic raw crosswalk change within ISCO code."))

    W_k <- sum(w[ii])
    d_k <- raw_difference[ii[1]]
    data.frame(
      isco_main = isco[ii[1]],
      N = length(ii),
      female_share = mean(female[ii]),
      from_score = y_from[ii[1]],
      to_score = y_to[ii[1]],
      raw_difference = d_k,
      female_weight_sum = W_k,
      contribution = W_k * d_k,
      stringsAsFactors = FALSE
    )
  }))
  rownames(code_table) <- NULL
  check(abs(sum(code_table$female_weight_sum)) < 1e-10,
        paste0(model_name, ": code-level female weights do not sum to zero."))
  check(abs(sum(code_table$contribution) - beta_change) < 1e-10,
        paste0(model_name, ": code-level contributions do not reconstruct coefficient change."))

  code_table <- code_table[order(-abs(code_table$contribution)), , drop = FALSE]
  code_table$abs_share <- abs(code_table$contribution) / sum(abs(code_table$contribution))
  code_table$net_share <- code_table$contribution / beta_change

  list(
    model = model_name,
    from_version = from_version,
    to_version = to_version,
    beta_from = unname(coef(fit_from)[["female"]]),
    beta_to = unname(coef(fit_to)[["female"]]),
    beta_change = beta_change,
    reconstructed_change = reconstructed_change,
    weight_sum = w_info$weight_sum,
    max_matrix_fwl_difference = w_info$max_matrix_fwl_difference,
    mean_raw_difference_female = mean(raw_difference[female == 1]),
    mean_raw_difference_male = mean(raw_difference[female == 0]),
    female_minus_male_raw_difference =
      mean(raw_difference[female == 1]) - mean(raw_difference[female == 0]),
    code_table = code_table
  )
}

crosswalk_objects_07 <- list(
  M1_historical_to_overview = make_female_crosswalk_audit_07(
    "Model1", "historical_common", "overview_common"
  ),
  M6_historical_to_overview = make_female_crosswalk_audit_07(
    "Model6", "historical_common", "overview_common"
  ),
  M1_syntax_to_overview = make_female_crosswalk_audit_07(
    "Model1", "syntax_common", "overview_common"
  ),
  M6_syntax_to_overview = make_female_crosswalk_audit_07(
    "Model6", "syntax_common", "overview_common"
  )
)

crosswalk_summary_07 <- do.call(rbind, lapply(names(crosswalk_objects_07), function(label) {
  z <- crosswalk_objects_07[[label]]
  data.frame(
    contrast = label,
    model = z$model,
    from_version = z$from_version,
    to_version = z$to_version,
    beta_from = z$beta_from,
    beta_to = z$beta_to,
    beta_change = z$beta_change,
    reconstructed_change = z$reconstructed_change,
    weight_sum = z$weight_sum,
    max_matrix_fwl_difference = z$max_matrix_fwl_difference,
    mean_raw_difference_female = z$mean_raw_difference_female,
    mean_raw_difference_male = z$mean_raw_difference_male,
    female_minus_male_raw_difference = z$female_minus_male_raw_difference,
    stringsAsFactors = FALSE
  )
}))
rownames(crosswalk_summary_07) <- NULL

make_public_crosswalk_top_07 <- function(object, contrast, n_top = 15L) {
  d <- object$code_table
  big <- d[d$N >= 5L, , drop = FALSE]
  small <- d[d$N < 5L, , drop = FALSE]

  z <- data.frame(
    contrast = contrast,
    model = object$model,
    group = "ISCO",
    isco_main = big$isco_main,
    N_reported = big$N,
    female_share_reported = big$female_share,
    from_score = big$from_score,
    to_score = big$to_score,
    raw_difference = big$raw_difference,
    female_weight_sum = big$female_weight_sum,
    contribution = big$contribution,
    stringsAsFactors = FALSE
  )

  if (nrow(small) > 0L) {
    pooled <- data.frame(
      contrast = contrast,
      model = object$model,
      group = "small_cells_pooled",
      isco_main = NA_real_,
      N_reported = sum(small$N),
      female_share_reported = NA_real_,
      from_score = NA_real_,
      to_score = NA_real_,
      raw_difference = NA_real_,
      female_weight_sum = sum(small$female_weight_sum),
      contribution = sum(small$contribution),
      stringsAsFactors = FALSE
    )
    z <- rbind(z, pooled)
  }

  z <- z[order(-abs(z$contribution)), , drop = FALSE]
  z$abs_share <- abs(z$contribution) / sum(abs(z$contribution))
  z$net_share <- z$contribution / object$beta_change
  head(z, n_top)
}

crosswalk_top_07 <- do.call(rbind, lapply(names(crosswalk_objects_07), function(label) {
  make_public_crosswalk_top_07(crosswalk_objects_07[[label]], label)
}))
rownames(crosswalk_top_07) <- NULL

summarise_code_set_07 <- function(object, contrast, codes, set_name) {
  z <- object$code_table[object$code_table$isco_main %in% codes, , drop = FALSE]
  contribution <- sum(z$contribution)
  data.frame(
    contrast = contrast,
    model = object$model,
    set = set_name,
    n_codes = nrow(z),
    contribution = contribution,
    net_share_pct = 100 * contribution / object$beta_change,
    stringsAsFactors = FALSE
  )
}

code_set_rows_07 <- list()
k_set_07 <- 1L
teaching_five_candidate_07 <- c(2300, 2330, 2341, 2342, 2350)

for (label in names(crosswalk_objects_07)) {
  object <- crosswalk_objects_07[[label]]
  d <- object$code_table
  changed_23xx <- d$isco_main[
    d$isco_main >= 2300 & d$isco_main < 2400 & abs(d$raw_difference) > 1e-12
  ]

  sets <- list(
    nursing_2221 = 2221,
    shopkeeper_5221 = 5221,
    nursing_plus_primary_2341 = c(2221, 2341),
    teaching_five_candidate = teaching_five_candidate_07,
    nursing_plus_teaching_five_candidate = c(2221, teaching_five_candidate_07),
    all_changed_23xx = changed_23xx,
    nursing_plus_all_changed_23xx = c(2221, changed_23xx)
  )

  for (set_name in names(sets)) {
    code_set_rows_07[[k_set_07]] <- summarise_code_set_07(
      object, label, unique(sets[[set_name]]), set_name
    )
    k_set_07 <- k_set_07 + 1L
  }
}

crosswalk_code_sets_07 <- do.call(rbind, code_set_rows_07)
rownames(crosswalk_code_sets_07) <- NULL

write.csv(
  crosswalk_summary_07,
  file.path(stage07_output, "07_female_crosswalk_raw_difference_summary.csv"),
  row.names = FALSE
)
write.csv(
  crosswalk_top_07,
  file.path(stage07_output, "07_female_crosswalk_raw_difference_top_contributions.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  crosswalk_code_sets_07,
  file.path(stage07_output, "07_female_crosswalk_code_set_summary.csv"),
  row.names = FALSE
)

cat("Raw crosswalk decomposition of the female coefficient written.\n")


# -----------------------------------------------------------------------------
# 9. Output checks
# -----------------------------------------------------------------------------

expected_outputs_07 <- c(
  "07_model6_block_wald_tests.csv",
  "07_model6_se_ratio_audit.csv",
  "07_model6_sparse_reference_cell_audit.csv",
  "07_crosswalk_control_sensitivity.csv",
  "07_crosswalk_sign_sensitivity.csv",
  "07_syntax_overview_compression_benchmark.csv",
  "07_crosswalk_major_group_means.csv",
  "07_female_coefficient_decomposition.csv",
  "07_model3_coefficient_decomposition.csv",
  "07_female_isco_residual_top_contributions.csv",
  "07_female_crosswalk_raw_difference_summary.csv",
  "07_female_crosswalk_raw_difference_top_contributions.csv",
  "07_female_crosswalk_code_set_summary.csv"
)

output_paths_07 <- file.path(stage07_output, expected_outputs_07)
check(all(file.exists(output_paths_07)), "One or more Stage 07 outputs were not written.")
output_info_07 <- file.info(output_paths_07)
check(all(!is.na(output_info_07$mtime) & output_info_07$mtime >= run_started_07 - 2),
      "One or more Stage 07 outputs pre-date the current run.")

cat("\nStage 07 complete.\n")
cat("  Common-support N: 1646\n")
cat("  Common-support ISCO codes: 247\n")
cat("  Compression slope: ", format(compression_slope_07, digits = 10), "\n", sep = "")
cat("  Compression R2: ", format(compression_R2_07, digits = 10), "\n", sep = "")
cat("  Output files written: ", length(expected_outputs_07), "\n", sep = "")
cat("  No respondent identifiers or respondent-level records were exported.\n")
