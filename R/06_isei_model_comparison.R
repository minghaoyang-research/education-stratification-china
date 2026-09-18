# =============================================================================
# 06_isei_model_comparison.R
#
# Controlled model comparison after the Stage 05 ISEI measurement audit.
#
# Purpose
# -------
# Re-estimate the submitted Models 1-7 under five outcome/sample versions:
#
#   1. historical_common   N = 1646
#   2. syntax_common       N = 1646
#   3. overview_common     N = 1646
#   4. syntax_full         N = 1650
#   5. overview_full       N = 1650
#
# The common-support comparisons isolate outcome measurement because the
# respondents and model matrices are required to be identical.
#
# Stage 06 does not introduce a new substantive model specification.
#
# Classical OLS standard errors are retained for direct comparability with
# the submitted dissertation. HC3 standard errors are calculated separately
# as an inference diagnostic.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Helpers
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE)

stop_check <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

assert_close <- function(actual, expected, tol = 1e-8, label = "") {
  
  stop_check(
    length(actual) == length(expected),
    paste0(label, ": vector lengths differ.")
  )
  
  same_na <- identical(is.na(actual), is.na(expected))
  
  stop_check(
    same_na,
    paste0(label, ": NA patterns differ.")
  )
  
  keep <- !is.na(actual)
  
  if (any(keep)) {
    max_diff <- max(abs(actual[keep] - expected[keep]))
    
    stop_check(
      is.finite(max_diff) && max_diff <= tol,
      paste0(
        label,
        ": maximum absolute difference = ",
        format(max_diff, digits = 16),
        ", tolerance = ",
        format(tol, digits = 16)
      )
    )
  }
  
  invisible(TRUE)
}

assert_exact_names <- function(actual, expected, label = "") {
  
  stop_check(
    identical(as.character(actual), as.character(expected)),
    paste0(
      label,
      ": term names or ordering differ.\n",
      "Actual:   ",
      paste(actual, collapse = ", "),
      "\nExpected: ",
      paste(expected, collapse = ", ")
    )
  )
  
  invisible(TRUE)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  stop_check(length(x) > 1L, "Outcome SD cannot be calculated.")
  stats::sd(x)
}


# -----------------------------------------------------------------------------
# 1. Reconstruct the verified Stage 03 object
# -----------------------------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "Stage 06 - Controlled ISEI model comparison\n",
  "============================================================\n",
  sep = ""
)

source("R/03_original_isei.R")

stop_check(
  exists("stage3_dat", inherits = FALSE),
  "Stage 03 did not leave stage3_dat in memory."
)

stop_check(
  nrow(stage3_dat) == 1674L,
  "stage3_dat is not 1,674 rows."
)

required_vars <- c(
  "id",
  "isco_main",
  "isei",
  "edu_lvl",
  "edu_bachelor",
  "edu_postgrad",
  "female",
  "age",
  "par_edu_yrs",
  "rural_origin",
  "rural_origin_f",
  "cohort",
  "cohort2",
  "cohort3",
  "cohort4"
)

missing_vars <- setdiff(required_vars, names(stage3_dat))

stop_check(
  length(missing_vars) == 0L,
  paste(
    "Missing required Stage 06 variables:",
    paste(missing_vars, collapse = ", ")
  )
)

stage6 <- stage3_dat

stop_check(
  !(".stage3_row_id" %in% names(stage6)),
  ".stage3_row_id already exists in stage3_dat."
)

stage6$.stage3_row_id <- seq_len(nrow(stage6))


# -----------------------------------------------------------------------------
# 2. Validate existing dummy variables
# -----------------------------------------------------------------------------

stop_check(
  all(stage6$edu_lvl[!is.na(stage6$edu_lvl)] %in% 1:3),
  "Unexpected edu_lvl coding."
)

stop_check(
  all(stage6$cohort[!is.na(stage6$cohort)] %in% 1:4),
  "Unexpected cohort coding."
)

stop_check(
  identical(
    as.integer(stage6$edu_bachelor),
    as.integer(stage6$edu_lvl == 2)
  ),
  "edu_bachelor does not reproduce edu_lvl == 2."
)

stop_check(
  identical(
    as.integer(stage6$edu_postgrad),
    as.integer(stage6$edu_lvl == 3)
  ),
  "edu_postgrad does not reproduce edu_lvl == 3."
)

stop_check(
  identical(
    as.integer(stage6$cohort2),
    as.integer(stage6$cohort == 2)
  ),
  "cohort2 does not reproduce cohort == 2."
)

stop_check(
  identical(
    as.integer(stage6$cohort3),
    as.integer(stage6$cohort == 3)
  ),
  "cohort3 does not reproduce cohort == 3."
)

stop_check(
  identical(
    as.integer(stage6$cohort4),
    as.integer(stage6$cohort == 4)
  ),
  "cohort4 does not reproduce cohort == 4."
)

for (nm in c("female", "rural_origin", "rural_origin_f")) {
  vals <- unique(stage6[[nm]][!is.na(stage6[[nm]])])
  
  stop_check(
    all(vals %in% c(0, 1)),
    paste0(nm, " is not coded 0/1.")
  )
}


# -----------------------------------------------------------------------------
# 3. Construct interaction columns exactly once
# -----------------------------------------------------------------------------

stage6$bach_x_rural <-
  stage6$edu_bachelor * stage6$rural_origin

stage6$post_x_rural <-
  stage6$edu_postgrad * stage6$rural_origin

stage6$bach_x_c2 <-
  stage6$edu_bachelor * stage6$cohort2

stage6$bach_x_c3 <-
  stage6$edu_bachelor * stage6$cohort3

stage6$bach_x_c4 <-
  stage6$edu_bachelor * stage6$cohort4

stage6$post_x_c2 <-
  stage6$edu_postgrad * stage6$cohort2

stage6$post_x_c3 <-
  stage6$edu_postgrad * stage6$cohort3

stage6$post_x_c4 <-
  stage6$edu_postgrad * stage6$cohort4


# -----------------------------------------------------------------------------
# 4. Read the frozen Stage 05 mapping audit
# -----------------------------------------------------------------------------

mapping_file <-
  "output/revised_measurement/05_code_level_mapping_audit.csv"

stop_check(
  file.exists(mapping_file),
  paste0("Missing Stage 05 artifact: ", mapping_file)
)

mapping <- read.csv(
  mapping_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_mapping_vars <- c(
  "isco_numeric",
  "isei_historical",
  "isei_syntax",
  "isei_overview"
)

stop_check(
  all(required_mapping_vars %in% names(mapping)),
  "Stage 05 mapping file does not contain the required columns."
)

stop_check(
  !anyDuplicated(mapping$isco_numeric),
  "Stage 05 mapping file contains duplicate isco_numeric values."
)

map_index <- match(
  as.integer(stage6$isco_main),
  as.integer(mapping$isco_numeric)
)

stop_check(
  !anyNA(map_index),
  "At least one Stage 3 ISCO code is absent from the Stage 05 mapping artifact."
)

# Provenance guardrail: Stage 05 must reproduce the Stage 03 historical ISEI.
stage05_historical <-
  mapping$isei_historical[map_index]

stop_check(
  identical(is.na(stage6$isei), is.na(stage05_historical)),
  "Stage 03 and Stage 05 historical ISEI missingness do not match."
)

historical_mapping_max_abs_diff <- max(
  abs(
    stage6$isei[!is.na(stage6$isei)] -
      stage05_historical[!is.na(stage6$isei)]
  )
)

stop_check(
  historical_mapping_max_abs_diff < 1e-8,
  "Stage 03 historical ISEI does not match the Stage 05 historical mapping."
)

cat("Stage 03 historical ISEI vs Stage 05 historical mapping: PASSED\n")

stage6$isei_syntax <-
  mapping$isei_syntax[map_index]

stage6$isei_overview <-
  mapping$isei_overview[map_index]


# -----------------------------------------------------------------------------
# 5. Reconfirm sample boundaries
# -----------------------------------------------------------------------------

core_flag <-
  !is.na(stage6$rural_origin) &
  !is.na(stage6$par_edu_yrs)

historical_flag <-
  core_flag &
  !is.na(stage6$isei)

syntax_full_flag <-
  core_flag &
  !is.na(stage6$isei_syntax)

overview_full_flag <-
  core_flag &
  !is.na(stage6$isei_overview)

common_flag <-
  core_flag &
  !is.na(stage6$isei) &
  !is.na(stage6$isei_syntax) &
  !is.na(stage6$isei_overview)

stop_check(sum(core_flag) == 1650L, "Pre-ISEI core is not 1,650.")
stop_check(sum(historical_flag) == 1646L, "Historical N is not 1,646.")
stop_check(sum(syntax_full_flag) == 1650L, "Syntax full N is not 1,650.")
stop_check(sum(overview_full_flag) == 1650L, "Overview full N is not 1,650.")
stop_check(sum(common_flag) == 1646L, "Common-support N is not 1,646.")

stop_check(
  sum(historical_flag & is.na(stage6$isei_syntax)) == 0L,
  "A historically mapped case becomes unmapped under syntax mapping."
)

stop_check(
  sum(historical_flag & is.na(stage6$isei_overview)) == 0L,
  "A historically mapped case becomes unmapped under overview mapping."
)

recovered_flag <-
  syntax_full_flag &
  !historical_flag

stop_check(
  sum(recovered_flag) == 4L,
  "The syntax mapping does not recover exactly four historical exclusions."
)

stop_check(
  all(stage6$isco_main[recovered_flag] == 1000),
  "Recovered cases are not all ISCO 1000."
)

stop_check(
  all(stage6$rural_origin[recovered_flag] == 0),
  "Recovered cases are not all urban origin."
)

stop_check(
  all(stage6$edu_lvl[recovered_flag] == 2),
  "Recovered cases are not all bachelor graduates."
)

cat(
  "\n--- sample anchors ---\n",
  "Stage 3 N:            ", nrow(stage6), "\n",
  "Pre-ISEI core N:      ", sum(core_flag), "\n",
  "Historical N:         ", sum(historical_flag), "\n",
  "Common-support N:     ", sum(common_flag), "\n",
  "Syntax full N:        ", sum(syntax_full_flag), "\n",
  "Overview full N:      ", sum(overview_full_flag), "\n",
  "Recovered cases:      ", sum(recovered_flag), "\n",
  sep = ""
)


# -----------------------------------------------------------------------------
# 6. Construct the five analysis versions
# -----------------------------------------------------------------------------

make_version <- function(flag, outcome, label) {
  
  d <- stage6[flag, , drop = FALSE]
  
  d$.y <- outcome[flag]
  d$.version <- label
  
  stop_check(
    !anyNA(d$.y),
    paste0(label, ": outcome contains missing values.")
  )
  
  d
}

versions <- list(
  historical_common = make_version(
    common_flag,
    stage6$isei,
    "historical_common"
  ),
  
  syntax_common = make_version(
    common_flag,
    stage6$isei_syntax,
    "syntax_common"
  ),
  
  overview_common = make_version(
    common_flag,
    stage6$isei_overview,
    "overview_common"
  ),
  
  syntax_full = make_version(
    syntax_full_flag,
    stage6$isei_syntax,
    "syntax_full"
  ),
  
  overview_full = make_version(
    overview_full_flag,
    stage6$isei_overview,
    "overview_full"
  )
)


# -----------------------------------------------------------------------------
# 7. Respondent-identity checks
# -----------------------------------------------------------------------------

common_keys <- lapply(
  versions[c(
    "historical_common",
    "syntax_common",
    "overview_common"
  )],
  function(d) d$.stage3_row_id
)

stop_check(
  identical(common_keys$historical_common, common_keys$syntax_common),
  "Historical and syntax common-support row identities differ."
)

stop_check(
  identical(common_keys$historical_common, common_keys$overview_common),
  "Historical and overview common-support row identities differ."
)

id_nonmissing <-
  all(!is.na(stage6$id[common_flag]))

id_unique <-
  !anyDuplicated(stage6$id[common_flag])

id_identity_verified <- FALSE

if (id_nonmissing && id_unique) {
  
  id_hist <-
    as.character(versions$historical_common$id)
  
  id_syntax <-
    as.character(versions$syntax_common$id)
  
  id_overview <-
    as.character(versions$overview_common$id)
  
  stop_check(
    identical(id_hist, id_syntax),
    "Historical and syntax common-support respondent IDs differ."
  )
  
  stop_check(
    identical(id_hist, id_overview),
    "Historical and overview common-support respondent IDs differ."
  )
  
  id_identity_verified <- TRUE
}

cat(
  "\n--- respondent identity ---\n",
  "Internal Stage-3 row identities: PASSED\n",
  "CGSS id non-missing on common support: ",
  id_nonmissing,
  "\n",
  "CGSS id unique on common support:      ",
  id_unique,
  "\n",
  "CGSS id identity check used:           ",
  id_identity_verified,
  "\n",
  sep = ""
)


# -----------------------------------------------------------------------------
# 8. Original model specifications
# -----------------------------------------------------------------------------

model_formulas <- list(
  
  Model1 =
    .y ~
    edu_bachelor +
    edu_postgrad +
    female +
    age,
  
  Model2 =
    .y ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs,
  
  Model3 =
    .y ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs +
    rural_origin,
  
  Model4 =
    .y ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs +
    rural_origin +
    bach_x_rural +
    post_x_rural,
  
  Model5 =
    .y ~
    edu_bachelor +
    edu_postgrad +
    female +
    par_edu_yrs +
    rural_origin +
    cohort2 +
    cohort3 +
    cohort4,
  
  Model6 =
    .y ~
    edu_bachelor +
    edu_postgrad +
    female +
    par_edu_yrs +
    rural_origin +
    cohort2 +
    cohort3 +
    cohort4 +
    bach_x_c2 +
    bach_x_c3 +
    bach_x_c4 +
    post_x_c2 +
    post_x_c3 +
    post_x_c4,
  
  Model7 =
    .y ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs +
    rural_origin_f
)


# -----------------------------------------------------------------------------
# 9. Fit all 35 models
# -----------------------------------------------------------------------------

fit_one <- function(data, formula) {
  
  stats::lm(
    formula = formula,
    data = data,
    na.action = stats::na.fail,
    model = TRUE,
    x = TRUE,
    y = TRUE
  )
}

fits <- lapply(
  versions,
  function(d) {
    lapply(
      model_formulas,
      function(f) fit_one(d, f)
    )
  }
)

for (v in names(fits)) {
  for (m in names(fits[[v]])) {
    
    expected_n <-
      if (v %in% c(
        "historical_common",
        "syntax_common",
        "overview_common"
      )) {
        1646L
      } else {
        1650L
      }
    
    stop_check(
      stats::nobs(fits[[v]][[m]]) == expected_n,
      paste0(v, " / ", m, ": unexpected model N.")
    )
  }
}

cat("\n35 model fits completed.\n")


# -----------------------------------------------------------------------------
# 10. Design-matrix identity checks on common support
# -----------------------------------------------------------------------------

design_checks <- data.frame(
  model = character(),
  syntax_matches_historical = logical(),
  overview_matches_historical = logical(),
  stringsAsFactors = FALSE
)

for (m in names(model_formulas)) {
  
  x_hist <-
    stats::model.matrix(
      fits$historical_common[[m]]
    )
  
  x_syntax <-
    stats::model.matrix(
      fits$syntax_common[[m]]
    )
  
  x_overview <-
    stats::model.matrix(
      fits$overview_common[[m]]
    )
  
  syntax_same <-
    identical(x_hist, x_syntax)
  
  overview_same <-
    identical(x_hist, x_overview)
  
  stop_check(
    syntax_same,
    paste0(m, ": syntax common-support design matrix differs.")
  )
  
  stop_check(
    overview_same,
    paste0(m, ": overview common-support design matrix differs.")
  )
  
  design_checks <- rbind(
    design_checks,
    data.frame(
      model = m,
      syntax_matches_historical = syntax_same,
      overview_matches_historical = overview_same,
      stringsAsFactors = FALSE
    )
  )
}

cat("All common-support design matrices: IDENTICAL\n")


# -----------------------------------------------------------------------------
# 11. Classical coefficient extraction
# -----------------------------------------------------------------------------

classical_coef_table <- function(fit) {
  
  sm <- summary(fit)
  cm <- sm$coefficients
  
  data.frame(
    term = rownames(cm),
    estimate = cm[, 1],
    se_classical = cm[, 2],
    t_classical = cm[, 3],
    p_classical = cm[, 4],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# -----------------------------------------------------------------------------
# 12. HC3 covariance matrix
# -----------------------------------------------------------------------------

vcov_hc3 <- function(fit) {
  
  X <- stats::model.matrix(fit)
  e <- stats::residuals(fit)
  h <- stats::hatvalues(fit)
  
  stop_check(
    all(h < 1),
    "At least one leverage value is >= 1; HC3 cannot be computed."
  )
  
  XtX_inv <- solve(crossprod(X))
  
  adjusted_resid <-
    e / (1 - h)
  
  X_adj <-
    X * as.numeric(adjusted_resid)
  
  meat <-
    crossprod(X_adj)
  
  V <-
    XtX_inv %*%
    meat %*%
    XtX_inv
  
  dimnames(V) <- list(
    colnames(X),
    colnames(X)
  )
  
  V
}

hc3_coef_table <- function(fit) {
  
  b <- stats::coef(fit)
  V <- vcov_hc3(fit)
  
  se <- sqrt(diag(V))
  t_value <- b / se
  df <- stats::df.residual(fit)
  
  p_value <-
    2 * stats::pt(
      abs(t_value),
      df = df,
      lower.tail = FALSE
    )
  
  data.frame(
    term = names(b),
    se_hc3 = unname(se),
    t_hc3 = unname(t_value),
    p_hc3 = unname(p_value),
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 13. Full coefficient table
# -----------------------------------------------------------------------------

coefficient_results <- data.frame()

for (v in names(fits)) {
  
  for (m in names(fits[[v]])) {
    
    fit <- fits[[v]][[m]]
    
    classical <-
      classical_coef_table(fit)
    
    hc3 <-
      hc3_coef_table(fit)
    
    stop_check(
      identical(classical$term, hc3$term),
      paste0(v, " / ", m, ": classical and HC3 term orders differ.")
    )
    
    y_sd <-
      safe_sd(stats::model.response(stats::model.frame(fit)))
    
    z <- cbind(
      classical,
      hc3[, c(
        "se_hc3",
        "t_hc3",
        "p_hc3"
      )]
    )
    
    z$estimate_over_y_sd <-
      z$estimate / y_sd
    
    z$outcome_sd <-
      y_sd
    
    z$version <-
      v
    
    z$model <-
      m
    
    z$N <-
      stats::nobs(fit)
    
    z$residual_df <-
      stats::df.residual(fit)
    
    z <- z[, c(
      "version",
      "model",
      "term",
      "N",
      "residual_df",
      "estimate",
      "se_classical",
      "t_classical",
      "p_classical",
      "se_hc3",
      "t_hc3",
      "p_hc3",
      "outcome_sd",
      "estimate_over_y_sd"
    )]
    
    coefficient_results <-
      rbind(
        coefficient_results,
        z
      )
  }
}


# -----------------------------------------------------------------------------
# 14. Model fit statistics
# -----------------------------------------------------------------------------

fit_statistics_one <- function(fit) {
  
  sm <- summary(fit)
  
  fstat <- sm$fstatistic
  
  data.frame(
    N = stats::nobs(fit),
    residual_df = stats::df.residual(fit),
    R2 = unname(sm$r.squared),
    adjusted_R2 = unname(sm$adj.r.squared),
    residual_SE = unname(sm$sigma),
    RSS = sum(stats::residuals(fit)^2),
    F = unname(fstat[1]),
    model_df = unname(fstat[2]),
    denominator_df = unname(fstat[3]),
    stringsAsFactors = FALSE
  )
}

fit_statistics <- data.frame()

for (v in names(fits)) {
  for (m in names(fits[[v]])) {
    
    z <- fit_statistics_one(
      fits[[v]][[m]]
    )
    
    z$version <- v
    z$model <- m
    
    z <- z[, c(
      "version",
      "model",
      "N",
      "residual_df",
      "R2",
      "adjusted_R2",
      "residual_SE",
      "RSS",
      "F",
      "model_df",
      "denominator_df"
    )]
    
    fit_statistics <-
      rbind(
        fit_statistics,
        z
      )
  }
}


# -----------------------------------------------------------------------------
# 15. VIF from the model matrix
# -----------------------------------------------------------------------------

vif_from_fit <- function(fit) {
  
  X <- stats::model.matrix(fit)
  
  stop_check(
    "(Intercept)" %in% colnames(X),
    "Expected an intercept column in model matrix."
  )
  
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  
  out <- data.frame(
    term = colnames(X),
    VIF = NA_real_,
    tolerance = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (j in seq_len(ncol(X))) {
    
    yj <- X[, j]
    
    if (ncol(X) == 1L) {
      r2 <- 0
    } else {
      
      others <-
        X[, -j, drop = FALSE]
      
      aux <-
        stats::lm(
          yj ~ others
        )
      
      r2 <-
        summary(aux)$r.squared
    }
    
    vif_j <-
      1 / (1 - r2)
    
    out$VIF[j] <-
      vif_j
    
    out$tolerance[j] <-
      1 / vif_j
  }
  
  out
}

vif_results <- data.frame()

for (v in names(fits)) {
  for (m in names(fits[[v]])) {
    
    z <-
      vif_from_fit(
        fits[[v]][[m]]
      )
    
    z$version <- v
    z$model <- m
    
    z <- z[, c(
      "version",
      "model",
      "term",
      "VIF",
      "tolerance"
    )]
    
    vif_results <-
      rbind(
        vif_results,
        z
      )
  }
}


# -----------------------------------------------------------------------------
# 16. Verify frozen Stage 04 historical benchmarks
# -----------------------------------------------------------------------------

benchmark_coef_file <-
  "output/original_replication/original_models_coefficients.csv"

benchmark_fit_file <-
  "output/original_replication/original_models_fit_statistics.csv"

benchmark_vif_file <-
  "output/original_replication/original_models_vif.csv"

benchmark_joint_file <-
  "output/original_replication/original_joint_interaction_tests.csv"

for (f in c(
  benchmark_coef_file,
  benchmark_fit_file,
  benchmark_vif_file,
  benchmark_joint_file
)) {
  stop_check(
    file.exists(f),
    paste0("Missing frozen Stage 04 benchmark: ", f)
  )
}

benchmark_coef <- read.csv(
  benchmark_coef_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

benchmark_fit <- read.csv(
  benchmark_fit_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

benchmark_vif <- read.csv(
  benchmark_vif_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

benchmark_joint <- read.csv(
  benchmark_joint_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ---- coefficient benchmarks ----

for (m in names(model_formulas)) {
  
  actual <-
    coefficient_results[
      coefficient_results$version == "historical_common" &
        coefficient_results$model == m,
      ,
      drop = FALSE
    ]
  
  expected <-
    benchmark_coef[
      benchmark_coef$model == m,
      ,
      drop = FALSE
    ]
  
  stop_check(
    nrow(actual) == nrow(expected),
    paste0(m, ": coefficient benchmark row count differs.")
  )
  
  assert_exact_names(
    actual$term,
    expected$term,
    paste0(m, " historical coefficient terms")
  )
  
  assert_close(
    actual$estimate,
    expected$B,
    tol = 1e-8,
    label = paste0(m, " historical B")
  )
  
  assert_close(
    actual$se_classical,
    expected$SE,
    tol = 1e-8,
    label = paste0(m, " historical classical SE")
  )
}


# ---- fit-statistic benchmarks ----

for (m in names(model_formulas)) {
  
  actual <-
    fit_statistics[
      fit_statistics$version == "historical_common" &
        fit_statistics$model == m,
      ,
      drop = FALSE
    ]
  
  expected <-
    benchmark_fit[
      benchmark_fit$model == m,
      ,
      drop = FALSE
    ]
  
  stop_check(
    nrow(actual) == 1L &&
      nrow(expected) == 1L,
    paste0(m, ": fit benchmark row count is not one.")
  )
  
  stop_check(
    actual$N == expected$N,
    paste0(m, ": historical benchmark N differs.")
  )
  
  stop_check(
    actual$residual_df == expected$residual_df,
    paste0(m, ": historical residual df differs.")
  )
  
  assert_close(
    actual$R2,
    expected$R2,
    tol = 1e-10,
    label = paste0(m, " historical R2")
  )
  
  assert_close(
    actual$adjusted_R2,
    expected$adjusted_R2,
    tol = 1e-10,
    label = paste0(m, " historical adjusted R2")
  )
  
  assert_close(
    actual$RSS,
    expected$RSS,
    tol = 1e-6,
    label = paste0(m, " historical RSS")
  )
}


# ---- VIF benchmarks ----

for (m in names(model_formulas)) {
  
  actual <-
    vif_results[
      vif_results$version == "historical_common" &
        vif_results$model == m,
      ,
      drop = FALSE
    ]
  
  expected <-
    benchmark_vif[
      benchmark_vif$model == m,
      ,
      drop = FALSE
    ]
  
  stop_check(
    nrow(actual) == nrow(expected),
    paste0(m, ": historical VIF row count differs.")
  )
  
  assert_exact_names(
    actual$term,
    expected$term,
    paste0(m, " historical VIF terms")
  )
  
  assert_close(
    actual$VIF,
    expected$VIF,
    tol = 1e-8,
    label = paste0(m, " historical VIF")
  )
}

cat(
  "\nFrozen Stage 04 historical coefficients, classical SEs,\n",
  "fit statistics and VIFs: PASSED\n",
  sep = ""
)


# -----------------------------------------------------------------------------
# 17. Assert VIF identity on common support
# -----------------------------------------------------------------------------

for (m in names(model_formulas)) {
  
  vh <- vif_results[
    vif_results$version == "historical_common" &
      vif_results$model == m,
    ,
    drop = FALSE
  ]
  
  vs <- vif_results[
    vif_results$version == "syntax_common" &
      vif_results$model == m,
    ,
    drop = FALSE
  ]
  
  vo <- vif_results[
    vif_results$version == "overview_common" &
      vif_results$model == m,
    ,
    drop = FALSE
  ]
  
  assert_exact_names(
    vh$term,
    vs$term,
    paste0(m, " historical vs syntax VIF terms")
  )
  
  assert_exact_names(
    vh$term,
    vo$term,
    paste0(m, " historical vs overview VIF terms")
  )
  
  assert_close(
    vh$VIF,
    vs$VIF,
    tol = 1e-12,
    label = paste0(m, " common-support syntax VIF")
  )
  
  assert_close(
    vh$VIF,
    vo$VIF,
    tol = 1e-12,
    label = paste0(m, " common-support overview VIF")
  )
}

cat("Common-support VIF identity: PASSED\n")


# -----------------------------------------------------------------------------
# 18. Classical interaction-block tests
# -----------------------------------------------------------------------------

classical_nested_test <- function(reduced_fit, full_fit) {
  
  a <-
    stats::anova(
      reduced_fit,
      full_fit
    )
  
  data.frame(
    numerator_df = a$Df[2],
    denominator_df = stats::df.residual(full_fit),
    F = a$F[2],
    p = a$`Pr(>F)`[2],
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 19. HC3 Wald interaction-block test
# -----------------------------------------------------------------------------

hc3_wald_test <- function(fit, terms) {
  
  b <- stats::coef(fit)
  V <- vcov_hc3(fit)
  
  stop_check(
    all(terms %in% names(b)),
    paste(
      "HC3 Wald test could not find:",
      paste(setdiff(terms, names(b)), collapse = ", ")
    )
  )
  
  idx <- match(terms, names(b))
  
  b_sub <- b[idx]
  V_sub <- V[idx, idx, drop = FALSE]

  # STAGE06_FINAL_PATCH: conditioning diagnostic for the tested HC3 block
  vcov_sub_rcond <- rcond(V_sub)
  vcov_sub_near_singular <-
    !is.finite(vcov_sub_rcond) || vcov_sub_rcond < 1e-10

  if (vcov_sub_near_singular) {
    warning(
      "HC3 Wald covariance submatrix is near-singular; rcond = ",
      format(vcov_sub_rcond, scientific = TRUE)
    )
  }
  
  q <- length(idx)
  
  W <-
    as.numeric(
      t(b_sub) %*%
        solve(V_sub) %*%
        b_sub
    )
  
  F_value <- W / q
  denominator_df <- stats::df.residual(fit)
  
  p_F <-
    stats::pf(
      F_value,
      df1 = q,
      df2 = denominator_df,
      lower.tail = FALSE
    )
  
  p_chisq <-
    stats::pchisq(
      W,
      df = q,
      lower.tail = FALSE
    )
  
  data.frame(
    numerator_df = q,
    denominator_df = denominator_df,
    wald_chisq = W,
    vcov_sub_rcond = vcov_sub_rcond,
    vcov_sub_near_singular = vcov_sub_near_singular,
    p_chisq = p_chisq,
    F_hc3 = F_value,
    p_hc3_F = p_F,
    stringsAsFactors = FALSE
  )
}

interaction_tests <- data.frame()

for (v in names(fits)) {
  
  # Model 4 block
  c4 <-
    classical_nested_test(
      fits[[v]]$Model3,
      fits[[v]]$Model4
    )
  
  h4 <-
    hc3_wald_test(
      fits[[v]]$Model4,
      c(
        "bach_x_rural",
        "post_x_rural"
      )
    )
  
  z4 <- data.frame(
    version = v,
    comparison = "Model3_vs_Model4",
    block = "education_x_rural_origin",
    classical_num_df = c4$numerator_df,
    classical_den_df = c4$denominator_df,
    classical_F = c4$F,
    classical_p = c4$p,
    hc3_num_df = h4$numerator_df,
    hc3_den_df = h4$denominator_df,
    hc3_wald_chisq = h4$wald_chisq,
      hc3_vcov_sub_rcond = h4$vcov_sub_rcond,
      hc3_vcov_sub_near_singular = h4$vcov_sub_near_singular,
    hc3_p_chisq = h4$p_chisq,
    hc3_F = h4$F_hc3,
    hc3_p_F = h4$p_hc3_F,
    stringsAsFactors = FALSE
  )
  
  # Model 6 block
  c6 <-
    classical_nested_test(
      fits[[v]]$Model5,
      fits[[v]]$Model6
    )
  
  h6 <-
    hc3_wald_test(
      fits[[v]]$Model6,
      c(
        "bach_x_c2",
        "bach_x_c3",
        "bach_x_c4",
        "post_x_c2",
        "post_x_c3",
        "post_x_c4"
      )
    )
  
  z6 <- data.frame(
    version = v,
    comparison = "Model5_vs_Model6",
    block = "education_x_cohort",
    classical_num_df = c6$numerator_df,
    classical_den_df = c6$denominator_df,
    classical_F = c6$F,
    classical_p = c6$p,
    hc3_num_df = h6$numerator_df,
    hc3_den_df = h6$denominator_df,
    hc3_wald_chisq = h6$wald_chisq,
      hc3_vcov_sub_rcond = h6$vcov_sub_rcond,
      hc3_vcov_sub_near_singular = h6$vcov_sub_near_singular,
    hc3_p_chisq = h6$p_chisq,
    hc3_F = h6$F_hc3,
    hc3_p_F = h6$p_hc3_F,
    stringsAsFactors = FALSE
  )
  
  interaction_tests <-
    rbind(
      interaction_tests,
      z4,
      z6
    )
}


# -----------------------------------------------------------------------------
# 20. Verify frozen Stage 04 joint F tests
# -----------------------------------------------------------------------------

hist_joint <-
  interaction_tests[
    interaction_tests$version == "historical_common",
    ,
    drop = FALSE
  ]

stop_check(
  nrow(hist_joint) == 2L,
  "Historical interaction-test table does not contain two rows."
)

expected_f34 <-
  benchmark_joint$F[
    grepl(
      "Model 3 vs Model 4",
      benchmark_joint$comparison,
      fixed = TRUE
    )
  ]

expected_p34 <-
  benchmark_joint$p[
    grepl(
      "Model 3 vs Model 4",
      benchmark_joint$comparison,
      fixed = TRUE
    )
  ]

expected_f56 <-
  benchmark_joint$F[
    grepl(
      "Model 5 vs Model 6",
      benchmark_joint$comparison,
      fixed = TRUE
    )
  ]

expected_p56 <-
  benchmark_joint$p[
    grepl(
      "Model 5 vs Model 6",
      benchmark_joint$comparison,
      fixed = TRUE
    )
  ]

actual_f34 <-
  hist_joint$classical_F[
    hist_joint$comparison == "Model3_vs_Model4"
  ]

actual_p34 <-
  hist_joint$classical_p[
    hist_joint$comparison == "Model3_vs_Model4"
  ]

actual_f56 <-
  hist_joint$classical_F[
    hist_joint$comparison == "Model5_vs_Model6"
  ]

actual_p56 <-
  hist_joint$classical_p[
    hist_joint$comparison == "Model5_vs_Model6"
  ]

assert_close(
  actual_f34,
  expected_f34,
  tol = 1e-8,
  label = "Historical Model 3 vs 4 F"
)

assert_close(
  actual_p34,
  expected_p34,
  tol = 1e-8,
  label = "Historical Model 3 vs 4 p"
)

assert_close(
  actual_f56,
  expected_f56,
  tol = 1e-8,
  label = "Historical Model 5 vs 6 F"
)

assert_close(
  actual_p56,
  expected_p56,
  tol = 1e-8,
  label = "Historical Model 5 vs 6 p"
)

cat("Frozen Stage 04 joint interaction tests: PASSED\n")


# -----------------------------------------------------------------------------
# 21. Outcome descriptives
# -----------------------------------------------------------------------------

outcome_descriptives <- do.call(
  rbind,
  lapply(
    names(versions),
    function(v) {
      
      y <- versions[[v]]$.y
      
      data.frame(
        version = v,
        N = length(y),
        mean = mean(y),
        sd = stats::sd(y),
        min = min(y),
        max = max(y),
        distinct_values = length(unique(y)),
        stringsAsFactors = FALSE
      )
    }
  )
)


# -----------------------------------------------------------------------------
# 22. Sample composition
# -----------------------------------------------------------------------------

sample_composition <- do.call(
  rbind,
  lapply(
    names(versions),
    function(v) {
      
      d <- versions[[v]]
      
      data.frame(
        version = v,
        N = nrow(d),
        
        junior_college =
          sum(d$edu_lvl == 1),
        
        bachelor =
          sum(d$edu_lvl == 2),
        
        postgraduate =
          sum(d$edu_lvl == 3),
        
        urban_origin =
          sum(d$rural_origin == 0),
        
        rural_origin =
          sum(d$rural_origin == 1),
        
        male =
          sum(d$female == 0),
        
        female =
          sum(d$female == 1),
        
        cohort1 =
          sum(d$cohort == 1),
        
        cohort2 =
          sum(d$cohort == 2),
        
        cohort3 =
          sum(d$cohort == 3),
        
        cohort4 =
          sum(d$cohort == 4),
        
        stringsAsFactors = FALSE
      )
    }
  )
)


# -----------------------------------------------------------------------------
# 23. Recovered-case aggregate diagnostic
# -----------------------------------------------------------------------------

recovered_case_summary <- data.frame(
  recovered_n = sum(recovered_flag),
  urban_n = sum(stage6$rural_origin[recovered_flag] == 0),
  bachelor_n = sum(stage6$edu_lvl[recovered_flag] == 2),
  isco1000_n = sum(stage6$isco_main[recovered_flag] == 1000),
  syntax_mean = mean(stage6$isei_syntax[recovered_flag]),
  overview_mean = mean(stage6$isei_overview[recovered_flag]),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# 24. Coefficient contrasts
# -----------------------------------------------------------------------------

make_coefficient_contrast <- function(
    old_version,
    new_version,
    contrast_name
) {
  
  old <-
    coefficient_results[
      coefficient_results$version == old_version,
      ,
      drop = FALSE
    ]
  
  new <-
    coefficient_results[
      coefficient_results$version == new_version,
      ,
      drop = FALSE
    ]
  
  keep_old <- c(
    "model",
    "term",
    "N",
    "estimate",
    "se_classical",
    "p_classical",
    "se_hc3",
    "p_hc3",
    "outcome_sd",
    "estimate_over_y_sd"
  )
  
  keep_new <- keep_old
  
  old <- old[, keep_old]
  new <- new[, keep_new]
  
  names(old)[-(1:2)] <-
    paste0(
      names(old)[-(1:2)],
      "_old"
    )
  
  names(new)[-(1:2)] <-
    paste0(
      names(new)[-(1:2)],
      "_new"
    )
  
  z <- merge(
    old,
    new,
    by = c("model", "term"),
    all = TRUE,
    sort = FALSE
  )
  
  stop_check(
    !anyNA(z$estimate_old) &&
      !anyNA(z$estimate_new),
    paste0(contrast_name, ": coefficient merge produced missing estimates.")
  )
  
  z$contrast <- contrast_name
  z$old_version <- old_version
  z$new_version <- new_version
  
  z$delta_B <-
    z$estimate_new -
    z$estimate_old
  
  z$delta_B_over_y_sd <-
    z$estimate_over_y_sd_new -
    z$estimate_over_y_sd_old
  
  z$delta_se_classical <-
    z$se_classical_new -
    z$se_classical_old
  
  z$delta_se_hc3 <-
    z$se_hc3_new -
    z$se_hc3_old
  
  z[, c(
    "contrast",
    "old_version",
    "new_version",
    "model",
    "term",
    
    "N_old",
    "N_new",
    
    "estimate_old",
    "estimate_new",
    "delta_B",
    
    "outcome_sd_old",
    "outcome_sd_new",
    
    "estimate_over_y_sd_old",
    "estimate_over_y_sd_new",
    "delta_B_over_y_sd",
    
    "se_classical_old",
    "se_classical_new",
    "delta_se_classical",
    
    "p_classical_old",
    "p_classical_new",
    
    "se_hc3_old",
    "se_hc3_new",
    "delta_se_hc3",
    
    "p_hc3_old",
    "p_hc3_new"
  )]
}

coefficient_contrasts <- rbind(
  
  make_coefficient_contrast(
    "historical_common",
    "syntax_common",
    "measurement_effect"
  ),
  
  make_coefficient_contrast(
    "historical_common",
    "overview_common",
    "historical_to_overview_common"
  ),
  
  make_coefficient_contrast(
    "syntax_common",
    "overview_common",
    "crosswalk_sensitivity_common"
  ),
  
  make_coefficient_contrast(
    "syntax_common",
    "syntax_full",
    "coverage_effect_syntax"
  ),
  
  make_coefficient_contrast(
    "overview_common",
    "overview_full",
    "coverage_effect_overview"
  ),
  
  make_coefficient_contrast(
    "syntax_full",
    "overview_full",
    "crosswalk_sensitivity_full"
  ),
  
  make_coefficient_contrast(
    "historical_common",
    "syntax_full",
    "historical_to_syntax_full"
  )
)


# -----------------------------------------------------------------------------
# 25. Fit-statistic contrasts
# -----------------------------------------------------------------------------

make_fit_contrast <- function(
    old_version,
    new_version,
    contrast_name
) {
  
  old <-
    fit_statistics[
      fit_statistics$version == old_version,
      ,
      drop = FALSE
    ]
  
  new <-
    fit_statistics[
      fit_statistics$version == new_version,
      ,
      drop = FALSE
    ]
  
  old <- old[, c(
    "model",
    "N",
    "R2",
    "adjusted_R2",
    "residual_SE",
    "RSS"
  )]
  
  new <- new[, c(
    "model",
    "N",
    "R2",
    "adjusted_R2",
    "residual_SE",
    "RSS"
  )]
  
  names(old)[-1] <-
    paste0(names(old)[-1], "_old")
  
  names(new)[-1] <-
    paste0(names(new)[-1], "_new")
  
  z <- merge(
    old,
    new,
    by = "model",
    sort = FALSE
  )
  
  z$contrast <- contrast_name
  z$old_version <- old_version
  z$new_version <- new_version
  
  z$delta_R2 <-
    z$R2_new -
    z$R2_old
  
  z$delta_adjusted_R2 <-
    z$adjusted_R2_new -
    z$adjusted_R2_old
  
  
  
  z[, c(
    "contrast",
    "old_version",
    "new_version",
    "model",
    "N_old",
    "N_new",
    "R2_old",
    "R2_new",
    "delta_R2",
    "adjusted_R2_old",
    "adjusted_R2_new",
    "delta_adjusted_R2"
  )]
}

fit_contrasts <- rbind(
  
  make_fit_contrast(
    "historical_common",
    "syntax_common",
    "measurement_effect"
  ),
  
  make_fit_contrast(
    "syntax_common",
    "overview_common",
    "crosswalk_sensitivity_common"
  ),
  
  make_fit_contrast(
    "syntax_common",
    "syntax_full",
    "coverage_effect_syntax"
  ),
  
  make_fit_contrast(
    "overview_common",
    "overview_full",
    "coverage_effect_overview"
  ),
  
  make_fit_contrast(
    "syntax_full",
    "overview_full",
    "crosswalk_sensitivity_full"
  )
)


# -----------------------------------------------------------------------------
# 26. Identity-check output
# -----------------------------------------------------------------------------

identity_checks <- data.frame(
  check = c(
    "common_row_identity_historical_vs_syntax",
    "common_row_identity_historical_vs_overview",
    "common_id_nonmissing",
    "common_id_unique",
    "common_id_identity_verified"
  ),
  result = c(
    identical(
      common_keys$historical_common,
      common_keys$syntax_common
    ),
    identical(
      common_keys$historical_common,
      common_keys$overview_common
    ),
    id_nonmissing,
    id_unique,
    id_identity_verified
  ),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------

# ------------------------------------------------------------
# STAGE06_FINAL_PATCH: frozen Stage 05 input provenance
# ------------------------------------------------------------

expected_stage05_mapping_md5 <- "ecbcc94d75d0f074ca05b625a95e5913"
expected_stage05_provenance_md5 <- "7a85ebcb48bbd3565041e4f1d04d86be"

stage05_mapping_md5_now <-
  unname(as.character(tools::md5sum(mapping_file)))

stage05_provenance_md5_now <-
  unname(as.character(tools::md5sum(prov_file)))

stop_check(
  identical(
    stage05_mapping_md5_now,
    expected_stage05_mapping_md5
  ),
  "Stage 05 mapping artifact has changed since Stage 06 was frozen."
)

stop_check(
  identical(
    stage05_provenance_md5_now,
    expected_stage05_provenance_md5
  ),
  "Stage 05 provenance artifact has changed since Stage 06 was frozen."
)

stage05_prov <- read.csv(
  prov_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_stage05_prov_cols <- c(
  "package",
  "version",
  "primary_function",
  "sensitivity_function"
)

stop_check(
  all(required_stage05_prov_cols %in% names(stage05_prov)),
  "Stage 05 provenance file has an unexpected schema."
)

stop_check(
  nrow(stage05_prov) == 1L,
  "Stage 05 provenance file should contain exactly one row."
)

stage06_input_provenance <- data.frame(
  stage05_mapping_file = mapping_file,
  stage05_mapping_md5 = stage05_mapping_md5_now,
  stage05_provenance_file = prov_file,
  stage05_provenance_md5 = stage05_provenance_md5_now,
  package = as.character(stage05_prov$package[1]),
  version = as.character(stage05_prov$version[1]),
  primary_function = as.character(stage05_prov$primary_function[1]),
  sensitivity_function =
    as.character(stage05_prov$sensitivity_function[1]),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# STAGE06_FINAL_PATCH: cohort x education outcome descriptives
# ------------------------------------------------------------

stage06_edu_labels <- c(
  "junior_college",
  "bachelor",
  "postgraduate"
)

stage06_cohort_labels <- c(
  "1957-64",
  "1965-74",
  "1975-84",
  "1985-92"
)

cohort_education_descriptives <-
  do.call(
    rbind,
    lapply(names(versions), function(v) {

      d <- versions[[v]]

      do.call(
        rbind,
        lapply(1:4, function(cc) {

          do.call(
            rbind,
            lapply(1:3, function(ee) {

              ii <- d$cohort == cc & d$edu_lvl == ee
              yy <- d$.y[ii]
              n_i <- sum(ii)

              data.frame(
                version = v,
                cohort = cc,
                cohort_label = stage06_cohort_labels[cc],
                edu_lvl = ee,
                education = stage06_edu_labels[ee],
                n = n_i,
                mean_y =
                  if (n_i > 0L) mean(yy) else NA_real_,
                sd_y =
                  if (n_i > 1L) stats::sd(yy) else NA_real_,
                stringsAsFactors = FALSE
              )
            })
          )
        })
      )
    })
  )

row.names(cohort_education_descriptives) <- NULL

stop_check(
  nrow(cohort_education_descriptives) ==
    length(versions) * 12L,
  "Cohort-by-education descriptive table should contain 12 cells per version."
)

for (v in names(versions)) {

  observed_n <- sum(
    cohort_education_descriptives$n[
      cohort_education_descriptives$version == v
    ]
  )

  stop_check(
    observed_n == nrow(versions[[v]]),
    paste0(
      "Cohort-by-education cells do not sum to the analysis N for ",
      v,
      "."
    )
  )
}

# 27. Output directory
# -----------------------------------------------------------------------------

output_dir <-
  "output/isei_model_comparison"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# -----------------------------------------------------------------------------
# 28. Write aggregate/model-level outputs
# -----------------------------------------------------------------------------


write.csv(
  stage06_input_provenance,
  file.path(
    output_dir,
    "06_input_provenance.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  cohort_education_descriptives,
  file.path(
    output_dir,
    "06_cohort_education_descriptives.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  coefficient_results,
  file.path(
    output_dir,
    "06_all_model_coefficients.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  coefficient_contrasts,
  file.path(
    output_dir,
    "06_coefficient_contrasts.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  fit_statistics,
  file.path(
    output_dir,
    "06_model_fit_statistics.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  fit_contrasts,
  file.path(
    output_dir,
    "06_model_fit_contrasts.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  vif_results,
  file.path(
    output_dir,
    "06_vif_all_versions.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  interaction_tests,
  file.path(
    output_dir,
    "06_interaction_joint_tests.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  outcome_descriptives,
  file.path(
    output_dir,
    "06_outcome_descriptives.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  sample_composition,
  file.path(
    output_dir,
    "06_sample_composition.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  recovered_case_summary,
  file.path(
    output_dir,
    "06_recovered_case_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  identity_checks,
  file.path(
    output_dir,
    "06_common_support_identity_checks.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  design_checks,
  file.path(
    output_dir,
    "06_common_support_design_matrix_checks.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 29. Focused comparison table for substantive review
# -----------------------------------------------------------------------------

focus_terms <- c(
  "edu_bachelor",
  "edu_postgrad",
  "female",
  "par_edu_yrs",
  "rural_origin",
  "rural_origin_f",
  "cohort2",
  "cohort3",
  "cohort4",
  "bach_x_rural",
  "post_x_rural",
  "bach_x_c2",
  "bach_x_c3",
  "bach_x_c4",
  "post_x_c2",
  "post_x_c3",
  "post_x_c4"
)

focus_results <-
  coefficient_results[
    coefficient_results$term %in% focus_terms,
    ,
    drop = FALSE
  ]

write.csv(
  focus_results,
  file.path(
    output_dir,
    "06_focus_coefficients.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 30. Key Console summaries
# -----------------------------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "STAGE 06 KEY RESULTS\n",
  "============================================================\n",
  sep = ""
)

cat("\n--- Outcome descriptives ---\n")
print(
  outcome_descriptives,
  row.names = FALSE
)

cat("\n--- Sample composition ---\n")
print(
  sample_composition,
  row.names = FALSE
)

cat("\n--- Model 3 key coefficients ---\n")

model3_focus <-
  coefficient_results[
    coefficient_results$model == "Model3" &
      coefficient_results$term %in% c(
        "edu_bachelor",
        "edu_postgrad",
        "rural_origin"
      ),
    c(
      "version",
      "term",
      "N",
      "estimate",
      "se_classical",
      "p_classical",
      "se_hc3",
      "p_hc3",
      "estimate_over_y_sd"
    ),
    drop = FALSE
  ]

print(
  model3_focus,
  row.names = FALSE
)

cat("\n--- Model 4 interaction block ---\n")

print(
  interaction_tests[
    interaction_tests$comparison == "Model3_vs_Model4",
    ,
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n--- Model 6 interaction block ---\n")

print(
  interaction_tests[
    interaction_tests$comparison == "Model5_vs_Model6",
    ,
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n--- Measurement-effect coefficient deltas ---\n")

measurement_focus <-
  coefficient_contrasts[
    coefficient_contrasts$contrast == "measurement_effect" &
      coefficient_contrasts$term %in% c(
        "edu_bachelor",
        "edu_postgrad",
        "female",
        "par_edu_yrs",
        "rural_origin",
        "cohort2",
        "cohort3",
        "cohort4",
        "bach_x_rural",
        "post_x_rural",
        "post_x_c2",
        "post_x_c3",
        "post_x_c4"
      ),
    c(
      "model",
      "term",
      "estimate_old",
      "estimate_new",
      "delta_B",
      "estimate_over_y_sd_old",
      "estimate_over_y_sd_new",
      "delta_B_over_y_sd"
    ),
    drop = FALSE
  ]

print(
  measurement_focus,
  row.names = FALSE
)

cat("\n--- Coverage-effect coefficient deltas ---\n")

coverage_focus <-
  coefficient_contrasts[
    coefficient_contrasts$contrast == "coverage_effect_syntax" &
      coefficient_contrasts$term %in% c(
        "edu_bachelor",
        "edu_postgrad",
        "female",
        "par_edu_yrs",
        "rural_origin",
        "cohort2",
        "cohort3",
        "cohort4",
        "bach_x_rural",
        "post_x_rural"
      ),
    c(
      "model",
      "term",
      "estimate_old",
      "estimate_new",
      "delta_B"
    ),
    drop = FALSE
  ]

print(
  coverage_focus,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 31. Final checks
# -----------------------------------------------------------------------------

stop_check(
  nrow(coefficient_results) > 0L,
  "Coefficient output is empty."
)

stop_check(
  nrow(fit_statistics) == 35L,
  "Expected 35 model-fit rows."
)

stop_check(
  nrow(interaction_tests) == 10L,
  "Expected 10 interaction-test rows."
)

stop_check(
  all(design_checks$syntax_matches_historical),
  "At least one syntax common-support design matrix check failed."
)

stop_check(
  all(design_checks$overview_matches_historical),
  "At least one overview common-support design matrix check failed."
)


# -----------------------------------------------------------------------------
# 32. Completion message
# -----------------------------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "06_isei_model_comparison.R: ALL CHECKS PASSED\n",
  "============================================================\n",
  "Model fits completed:                35\n",
  "Common-support N:                    1646\n",
  "Revised full N:                      1650\n",
  "Historical Stage 04 benchmarks:      PASSED\n",
  "Respondent identity checks:          PASSED\n",
  "Common-support design matrices:      PASSED\n",
  "Common-support VIF identity:          PASSED\n",
  "Classical interaction benchmarks:    PASSED\n",
  "\n",
  "Outputs written to:\n",
  "output/isei_model_comparison\n",
  "\n",
  "No respondent-level CGSS data were exported.\n",
  sep = ""
)
