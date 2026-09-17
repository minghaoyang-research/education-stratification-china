# ============================================================
# 04_original_models.R
# Original dissertation reproduction
#
# Purpose:
#   Reproduce and validate the final historical SPSS v4:
#   - OLS Models 1-7
#   - model-fit statistics
#   - VIF diagnostics
#   - Table 6: rural origin x education
#   - Appendix A1: cohort x education
#   - Table 7: cohort x education ISEI means
#   - joint partial-F tests
#
# IMPORTANT:
#   This file reproduces the submitted historical analysis.
#   Do NOT introduce revised ISEI mappings, HC3 standard
#   errors, revised parental-education coding, survey weights,
#   or revised model specifications here.
#
# Historical validation authority:
#   1. Final SPSS v4 output
#   2. Final SPSS v4 syntax
#   3. Submitted dissertation tables
# ============================================================


# ------------------------------------------------------------
# 0. Rebuild historical analysis
# ------------------------------------------------------------

source("R/03_original_isei.R")


# Stage 03 should normally create analysis_original.
# This fallback reconstructs it if necessary.

if (!exists("analysis_original", inherits = FALSE)) {

  if (!exists("stage3_dat", inherits = FALSE)) {
    stop(
      "Neither analysis_original nor stage3_dat exists after Stage 03."
    )
  }

  required_reconstruction_vars <- c(
    "isei",
    "rural_origin",
    "par_edu_yrs"
  )

  missing_reconstruction_vars <- setdiff(
    required_reconstruction_vars,
    names(stage3_dat)
  )

  if (length(missing_reconstruction_vars) > 0) {
    stop(
      paste(
        "Cannot reconstruct analysis_original. Missing:",
        paste(missing_reconstruction_vars, collapse = ", ")
      )
    )
  }

  analysis_original <- stage3_dat[
    !is.na(stage3_dat$isei) &
      !is.na(stage3_dat$rural_origin) &
      !is.na(stage3_dat$par_edu_yrs),
    ,
    drop = FALSE
  ]
}


stopifnot(nrow(analysis_original) == 1646)

# Local working copy.
d <- analysis_original


# ------------------------------------------------------------
# 1. Helper functions
# ------------------------------------------------------------

to_numeric_safe <- function(x) {

  if (is.factor(x)) {
    return(as.numeric(as.character(x)))
  }

  as.numeric(x)
}


# Compare an unrounded R result with an SPSS value displayed
# to a fixed number of decimal places.

assert_display <- function(
    actual,
    target,
    digits = 3,
    label = ""
) {

  if (
    length(actual) != 1 ||
    is.na(actual) ||
    !is.finite(actual)
  ) {
    stop(
      paste0(
        "Validation failed for ",
        label,
        ": actual value is not a single finite number."
      )
    )
  }

  tolerance <- 0.5 * 10^(-digits) + 1e-10

  if (abs(actual - target) > tolerance) {

    stop(
      sprintf(
        paste0(
          "Validation failed for %s.\n",
          "Actual: %.12f\n",
          "SPSS displayed target: %.*f\n",
          "Allowed tolerance: %.12g"
        ),
        label,
        actual,
        digits,
        target,
        tolerance
      )
    )
  }

  invisible(TRUE)
}


assert_equal_integer <- function(
    actual,
    target,
    label = ""
) {

  if (
    length(actual) != 1 ||
    is.na(actual) ||
    actual != target
  ) {

    stop(
      sprintf(
        "Validation failed for %s. Actual = %s; expected = %s.",
        label,
        actual,
        target
      )
    )
  }

  invisible(TRUE)
}


extract_coefficients <- function(
    model,
    model_name
) {

  cf <- summary(model)$coefficients

  data.frame(
    model = model_name,
    term = rownames(cf),
    B = unname(cf[, 1]),
    SE = unname(cf[, 2]),
    t = unname(cf[, 3]),
    p = unname(cf[, 4]),
    row.names = NULL,
    check.names = FALSE
  )
}


extract_fit <- function(
    model,
    model_name
) {

  s <- summary(model)

  data.frame(
    model = model_name,
    N = nobs(model),
    residual_df = df.residual(model),
    R2 = unname(s$r.squared),
    adjusted_R2 = unname(s$adj.r.squared),
    residual_SE = unname(s$sigma),
    RSS = unname(deviance(model)),
    F = unname(s$fstatistic[1]),
    model_df = unname(s$fstatistic[2]),
    denominator_df = unname(s$fstatistic[3]),
    row.names = NULL
  )
}


# Conventional VIF:
# VIF_j = 1 / (1 - R_j^2)

vif_numeric <- function(model) {

  mm <- model.matrix(model)

  if ("(Intercept)" %in% colnames(mm)) {
    x <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  } else {
    x <- mm
  }

  if (ncol(x) < 2) {
    stop("VIF requires at least two predictors.")
  }

  out <- numeric(ncol(x))
  names(out) <- colnames(x)

  for (j in seq_len(ncol(x))) {

    y_j <- x[, j]

    other_x <- x[, -j, drop = FALSE]

    auxiliary_fit <- lm.fit(
      x = cbind("(Intercept)" = 1, other_x),
      y = y_j
    )

    rss <- sum(auxiliary_fit$residuals^2)

    tss <- sum(
      (y_j - mean(y_j))^2
    )

    r2_j <- 1 - rss / tss

    out[j] <- 1 / (1 - r2_j)
  }

  out
}


# ------------------------------------------------------------
# 2. Standardise variable names and types
# ------------------------------------------------------------

if (!"age" %in% names(d)) {

  if ("age_2017" %in% names(d)) {
    d$age <- to_numeric_safe(d$age_2017)
  } else {
    stop("Neither age nor age_2017 exists.")
  }
}


required_core_vars <- c(
  "isei",
  "edu_lvl",
  "female",
  "age",
  "par_edu_yrs",
  "rural_origin",
  "rural_origin_f",
  "cohort"
)


missing_core_vars <- setdiff(
  required_core_vars,
  names(d)
)


if (length(missing_core_vars) > 0) {

  stop(
    paste(
      "Missing required Stage-04 variables:",
      paste(missing_core_vars, collapse = ", ")
    )
  )
}


for (v in required_core_vars) {
  d[[v]] <- to_numeric_safe(d[[v]])
}


stopifnot(nrow(d) == 1646)

stopifnot(
  all(d$edu_lvl %in% 1:3),
  all(d$cohort %in% 1:4),
  all(d$female %in% 0:1),
  all(d$rural_origin %in% 0:1),
  all(d$rural_origin_f %in% 0:1)
)


# ------------------------------------------------------------
# 3. Historical dummy and interaction variables
# ------------------------------------------------------------

# Education reference:
# Junior college = category 1

d$edu_bachelor <- as.integer(
  d$edu_lvl == 2
)

d$edu_postgrad <- as.integer(
  d$edu_lvl == 3
)


# Cohort reference:
# 1957-64 = cohort 1

d$cohort2 <- as.integer(
  d$cohort == 2
)

d$cohort3 <- as.integer(
  d$cohort == 3
)

d$cohort4 <- as.integer(
  d$cohort == 4
)


# Education x rural origin

d$bach_x_rural <-
  d$edu_bachelor * d$rural_origin

d$post_x_rural <-
  d$edu_postgrad * d$rural_origin


# Education x cohort

d$bach_x_c2 <-
  d$edu_bachelor * d$cohort2

d$bach_x_c3 <-
  d$edu_bachelor * d$cohort3

d$bach_x_c4 <-
  d$edu_bachelor * d$cohort4

d$post_x_c2 <-
  d$edu_postgrad * d$cohort2

d$post_x_c3 <-
  d$edu_postgrad * d$cohort3

d$post_x_c4 <-
  d$edu_postgrad * d$cohort4


model_required_vars <- c(
  "isei",
  "edu_bachelor",
  "edu_postgrad",
  "female",
  "age",
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


stopifnot(
  sum(
    !complete.cases(
      d[, model_required_vars]
    )
  ) == 0
)


# ------------------------------------------------------------
# 4. Historical OLS Models 1-7
# ------------------------------------------------------------

# Model 1
model1 <- lm(
  isei ~
    edu_bachelor +
    edu_postgrad +
    female +
    age,
  data = d
)


# Model 2
model2 <- lm(
  isei ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs,
  data = d
)


# Model 3
model3 <- lm(
  isei ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs +
    rural_origin,
  data = d
)


# Model 4
model4 <- lm(
  isei ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs +
    rural_origin +
    bach_x_rural +
    post_x_rural,
  data = d
)


# Model 5
# Age is intentionally replaced by cohort dummies.

model5 <- lm(
  isei ~
    edu_bachelor +
    edu_postgrad +
    female +
    par_edu_yrs +
    rural_origin +
    cohort2 +
    cohort3 +
    cohort4,
  data = d
)


# Model 6
# Exploratory education x cohort interactions.

model6 <- lm(
  isei ~
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
  data = d
)


# Model 7
# Model 3 using the A27F residence-based origin measure.

model7 <- lm(
  isei ~
    edu_bachelor +
    edu_postgrad +
    female +
    age +
    par_edu_yrs +
    rural_origin_f,
  data = d
)


models <- list(
  Model1 = model1,
  Model2 = model2,
  Model3 = model3,
  Model4 = model4,
  Model5 = model5,
  Model6 = model6,
  Model7 = model7
)


stopifnot(
  all(
    vapply(
      models,
      nobs,
      numeric(1)
    ) == 1646
  )
)


# ------------------------------------------------------------
# 5. SPSS coefficient and SE benchmarks
# ------------------------------------------------------------

benchmark_coefficients <- list(

  Model1 = data.frame(
    term = c(
      "(Intercept)",
      "edu_bachelor",
      "edu_postgrad",
      "female",
      "age"
    ),
    B = c(
      38.447,
      6.274,
      16.199,
      0.943,
      0.230
    ),
    SE = c(
      1.814,
      0.817,
      1.494,
      0.776,
      0.041
    )
  ),

  Model2 = data.frame(
    term = c(
      "(Intercept)",
      "edu_bachelor",
      "edu_postgrad",
      "female",
      "age",
      "par_edu_yrs"
    ),
    B = c(
      36.637,
      6.072,
      15.761,
      0.918,
      0.243,
      0.149
    ),
    SE = c(
      2.188,
      0.828,
      1.523,
      0.775,
      0.042,
      0.101
    )
  ),

  Model3 = data.frame(
    term = c(
      "(Intercept)",
      "edu_bachelor",
      "edu_postgrad",
      "female",
      "age",
      "par_edu_yrs",
      "rural_origin"
    ),
    B = c(
      32.802,
      6.205,
      15.899,
      1.062,
      0.272,
      0.281,
      3.025
    ),
    SE = c(
      2.423,
      0.826,
      1.518,
      0.774,
      0.043,
      0.107,
      0.833
    )
  ),

  Model4 = data.frame(
    term = c(
      "(Intercept)",
      "edu_bachelor",
      "edu_postgrad",
      "female",
      "age",
      "par_edu_yrs",
      "rural_origin",
      "bach_x_rural",
      "post_x_rural"
    ),
    B = c(
      33.562,
      5.159,
      14.116,
      1.049,
      0.267,
      0.294,
      1.651,
      2.205,
      4.380
    ),
    SE = c(
      2.470,
      1.112,
      1.920,
      0.773,
      0.043,
      0.107,
      1.206,
      1.626,
      3.090
    )
  ),

  Model5 = data.frame(
    term = c(
      "(Intercept)",
      "edu_bachelor",
      "edu_postgrad",
      "female",
      "par_edu_yrs",
      "rural_origin",
      "cohort2",
      "cohort3",
      "cohort4"
    ),
    B = c(
      48.859,
      6.184,
      15.971,
      1.098,
      0.260,
      2.918,
      -3.542,
      -5.809,
      -7.885
    ),
    SE = c(
      1.556,
      0.829,
      1.523,
      0.777,
      0.107,
      0.834,
      1.492,
      1.433,
      1.422
    )
  ),

  Model6 = data.frame(
    term = c(
      "(Intercept)",
      "edu_bachelor",
      "edu_postgrad",
      "female",
      "par_edu_yrs",
      "rural_origin",
      "cohort2",
      "cohort3",
      "cohort4",
      "bach_x_c2",
      "bach_x_c3",
      "bach_x_c4",
      "post_x_c2",
      "post_x_c3",
      "post_x_c4"
    ),
    B = c(
      48.950,
      5.873,
      15.764,
      1.167,
      0.258,
      2.956,
      -2.252,
      -6.878,
      -8.354,
      -2.655,
      1.397,
      1.228,
      -2.684,
      4.479,
      -1.101
    ),
    SE = c(
      1.761,
      2.522,
      11.163,
      0.778,
      0.107,
      0.838,
      1.928,
      1.907,
      1.872,
      3.080,
      2.911,
      2.852,
      11.720,
      11.462,
      11.366
    )
  ),

  Model7 = data.frame(
    term = c(
      "(Intercept)",
      "edu_bachelor",
      "edu_postgrad",
      "female",
      "age",
      "par_edu_yrs",
      "rural_origin_f"
    ),
    B = c(
      32.884,
      6.179,
      15.859,
      1.067,
      0.269,
      0.283,
      3.089
    ),
    SE = c(
      2.403,
      0.825,
      1.517,
      0.774,
      0.043,
      0.107,
      0.832
    )
  )
)


# ------------------------------------------------------------
# 6. Validate coefficients and classical SEs
# ------------------------------------------------------------

for (model_name in names(models)) {

  actual <- extract_coefficients(
    models[[model_name]],
    model_name
  )

  expected <- benchmark_coefficients[[model_name]]

  stopifnot(
    all(expected$term %in% actual$term)
  )

  for (i in seq_len(nrow(expected))) {

    term_i <- expected$term[i]

    actual_row <- actual[
      actual$term == term_i,
      ,
      drop = FALSE
    ]

    assert_display(
      actual_row$B,
      expected$B[i],
      digits = 3,
      label = paste(
        model_name,
        term_i,
        "B"
      )
    )

    assert_display(
      actual_row$SE,
      expected$SE[i],
      digits = 3,
      label = paste(
        model_name,
        term_i,
        "SE"
      )
    )
  }
}


cat(
  "\nAll Model 1-7 coefficients and classical SEs: PASSED\n"
)


# ------------------------------------------------------------
# 7. Model-fit statistics
# ------------------------------------------------------------

fit_benchmarks <- data.frame(

  model = c(
    "Model1",
    "Model2",
    "Model3",
    "Model4",
    "Model5",
    "Model6",
    "Model7"
  ),

  R2 = c(
    0.085,
    0.086,
    0.093,
    0.095,
    0.091,
    0.095,
    0.093
  ),

  adjusted_R2 = c(
    0.082,
    0.083,
    0.090,
    0.090,
    0.087,
    0.087,
    0.090
  ),

  residual_df = c(
    1641,
    1640,
    1639,
    1637,
    1637,
    1631,
    1639
  )
)


fit_all <- do.call(
  rbind,
  lapply(
    names(models),
    function(nm) {
      extract_fit(
        models[[nm]],
        nm
      )
    }
  )
)

row.names(fit_all) <- NULL


for (i in seq_len(nrow(fit_benchmarks))) {

  nm <- fit_benchmarks$model[i]

  actual <- fit_all[
    fit_all$model == nm,
    ,
    drop = FALSE
  ]

  assert_display(
    actual$R2,
    fit_benchmarks$R2[i],
    digits = 3,
    label = paste(
      nm,
      "R-squared"
    )
  )

  assert_display(
    actual$adjusted_R2,
    fit_benchmarks$adjusted_R2[i],
    digits = 3,
    label = paste(
      nm,
      "adjusted R-squared"
    )
  )

  assert_equal_integer(
    actual$residual_df,
    fit_benchmarks$residual_df[i],
    label = paste(
      nm,
      "residual df"
    )
  )
}


rss_benchmarks <- c(
  Model1 = 402203.422,
  Model3 = 398465.254,
  Model4 = 397735.913,
  Model5 = 399396.001,
  Model6 = 397692.944,
  Model7 = 398317.118
)


for (nm in names(rss_benchmarks)) {

  actual_rss <- fit_all$RSS[
    fit_all$model == nm
  ]

  assert_display(
    actual_rss,
    rss_benchmarks[[nm]],
    digits = 3,
    label = paste(
      nm,
      "residual sum of squares"
    )
  )
}


cat(
  "Model fit statistics and SPSS RSS anchors: PASSED\n"
)


# ------------------------------------------------------------
# 8. VIF diagnostics
# ------------------------------------------------------------

vif_all <- do.call(
  rbind,
  lapply(
    names(models),
    function(nm) {

      v <- vif_numeric(
        models[[nm]]
      )

      data.frame(
        model = nm,
        term = names(v),
        VIF = as.numeric(v),
        tolerance = 1 / as.numeric(v),
        row.names = NULL
      )
    }
  )
)

row.names(vif_all) <- NULL


model6_vif_benchmark <- c(
  edu_bachelor = 10.729,
  edu_postgrad = 62.480,
  female = 1.021,
  par_edu_yrs = 1.258,
  rural_origin = 1.165,
  cohort2 = 4.057,
  cohort3 = 5.273,
  cohort4 = 5.585,
  bach_x_c2 = 4.951,
  bach_x_c3 = 8.008,
  bach_x_c4 = 8.801,
  post_x_c2 = 11.679,
  post_x_c3 = 22.563,
  post_x_c4 = 34.053
)


vif_model6 <- vif_numeric(
  model6
)


stopifnot(
  all(
    names(model6_vif_benchmark) %in%
      names(vif_model6)
  )
)


for (nm in names(model6_vif_benchmark)) {

  assert_display(
    vif_model6[[nm]],
    model6_vif_benchmark[[nm]],
    digits = 3,
    label = paste(
      "Model6 VIF",
      nm
    )
  )
}


cat(
  "Model 6 VIF diagnostics: PASSED\n"
)


# Validate dissertation statement:
# Models 1-5 and Model 7 maximum VIF = 3.218.

vif_1to5_7 <- vif_all[
  vif_all$model != "Model6",
  ,
  drop = FALSE
]


max_vif_1to5_7 <- max(
  vif_1to5_7$VIF
)


assert_display(
  max_vif_1to5_7,
  3.218,
  digits = 3,
  label = "Max VIF across Models 1-5 and 7"
)


cat(
  "Maximum VIF across Models 1-5 and 7:",
  sprintf(
    "%.6f",
    max_vif_1to5_7
  ),
  ": PASSED\n"
)


# ------------------------------------------------------------
# 9. Table 6: rural origin x education
# ------------------------------------------------------------

table6_origin_education <- table(
  d$rural_origin,
  d$edu_lvl
)


expected_table6 <- matrix(
  c(
    361, 480, 87,
    346, 326, 46
  ),
  nrow = 2,
  byrow = TRUE
)


stopifnot(
  all(
    dim(table6_origin_education) ==
      c(2, 3)
  ),
  all(
    table6_origin_education ==
      expected_table6
  ),
  sum(table6_origin_education) == 1646
)


chi_origin_education <- chisq.test(
  table6_origin_education,
  correct = FALSE
)


assert_display(
  unname(
    chi_origin_education$statistic
  ),
  15.847,
  digits = 3,
  label = "Table 6 Pearson chi-square"
)


assert_equal_integer(
  unname(
    chi_origin_education$parameter
  ),
  2,
  label = "Table 6 chi-square df"
)


cat(
  "Table 6 rural origin x education chi-square:",
  sprintf(
    "X2(%d) = %.6f, p = %.8f : PASSED\n",
    unname(
      chi_origin_education$parameter
    ),
    unname(
      chi_origin_education$statistic
    ),
    chi_origin_education$p.value
  )
)


# ------------------------------------------------------------
# 10. Appendix A1: cohort x education
# ------------------------------------------------------------

table_a1_cohort_education <- table(
  d$cohort,
  d$edu_lvl
)


expected_table_a1 <- matrix(
  c(
    108, 60, 2,
    174, 139, 21,
    194, 277, 43,
    231, 330, 67
  ),
  nrow = 4,
  byrow = TRUE
)


stopifnot(
  all(
    dim(table_a1_cohort_education) ==
      c(4, 3)
  ),
  all(
    table_a1_cohort_education ==
      expected_table_a1
  ),
  sum(table_a1_cohort_education) == 1646
)


chi_cohort_education <- chisq.test(
  table_a1_cohort_education,
  correct = FALSE
)


assert_display(
  unname(
    chi_cohort_education$statistic
  ),
  63.051,
  digits = 3,
  label = "Appendix A1 Pearson chi-square"
)


assert_equal_integer(
  unname(
    chi_cohort_education$parameter
  ),
  6,
  label = "Appendix A1 chi-square df"
)


cat(
  "Appendix A1 cohort x education chi-square:",
  sprintf(
    "X2(%d) = %.6f, p = %.8f : PASSED\n",
    unname(
      chi_cohort_education$parameter
    ),
    unname(
      chi_cohort_education$statistic
    ),
    chi_cohort_education$p.value
  )
)


# ------------------------------------------------------------
# 11. Table 7: ISEI by cohort x education
# ------------------------------------------------------------

table7_rows <- list()

counter <- 1


for (c_id in 1:4) {

  for (e_id in 1:3) {

    x <- d$isei[
      d$cohort == c_id &
        d$edu_lvl == e_id
    ]

    table7_rows[[counter]] <- data.frame(
      cohort = c_id,
      edu_lvl = e_id,
      n = length(x),
      mean_isei = mean(x),
      sd_isei = sd(x)
    )

    counter <- counter + 1
  }
}


table7 <- do.call(
  rbind,
  table7_rows
)

row.names(table7) <- NULL


expected_table7 <- data.frame(

  cohort = c(
    1, 1, 1,
    2, 2, 2,
    3, 3, 3,
    4, 4, 4
  ),

  edu_lvl = c(
    1, 2, 3,
    1, 2, 3,
    1, 2, 3,
    1, 2, 3
  ),

  n = c(
    108, 60, 2,
    174, 139, 21,
    194, 277, 43,
    231, 330, 67
  ),

  mean_isei = c(
    52.1204,
    58.2667,
    70.0000,

    50.7299,
    54.0719,
    64.5238,

    46.4742,
    53.8953,
    66.8605,

    45.4502,
    52.3182,
    59.9851
  )
)


stopifnot(
  nrow(table7) == 12,
  sum(table7$n) == 1646,
  all(
    table7$cohort ==
      expected_table7$cohort
  ),
  all(
    table7$edu_lvl ==
      expected_table7$edu_lvl
  ),
  all(
    table7$n ==
      expected_table7$n
  )
)


for (i in seq_len(nrow(table7))) {

  assert_display(
    table7$mean_isei[i],
    expected_table7$mean_isei[i],
    digits = 4,
    label = paste(
      "Table 7 cohort",
      table7$cohort[i],
      "education",
      table7$edu_lvl[i],
      "ISEI mean"
    )
  )
}


cohort_labels <- c(
  "1957-64",
  "1965-74",
  "1975-84",
  "1985-92"
)


education_labels <- c(
  "Junior college",
  "Bachelor",
  "Postgraduate"
)


table7$cohort_label <- cohort_labels[
  table7$cohort
]


table7$education_label <- education_labels[
  table7$edu_lvl
]


cat(
  "Table 7 cohort x education ISEI means: PASSED\n"
)


# ------------------------------------------------------------
# 12. Joint interaction test: Model 3 vs Model 4
# ------------------------------------------------------------

anova_34 <- anova(
  model3,
  model4
)


stopifnot(
  nrow(anova_34) == 2
)


f_34 <- anova_34$F[2]

p_34 <- anova_34$`Pr(>F)`[2]

num_df_34 <- anova_34$Df[2]

den_df_34 <- anova_34$Res.Df[2]


assert_equal_integer(
  num_df_34,
  2,
  label = "Model 3 vs Model 4 numerator df"
)


assert_equal_integer(
  den_df_34,
  1637,
  label = "Model 3 vs Model 4 denominator df"
)


assert_display(
  f_34,
  1.501,
  digits = 3,
  label = "Model 3 vs Model 4 partial F"
)


assert_display(
  p_34,
  0.223,
  digits = 3,
  label = "Model 3 vs Model 4 partial-F p value"
)


# ------------------------------------------------------------
# 13. Joint interaction test: Model 5 vs Model 6
# ------------------------------------------------------------

anova_56 <- anova(
  model5,
  model6
)


stopifnot(
  nrow(anova_56) == 2
)


f_56 <- anova_56$F[2]

p_56 <- anova_56$`Pr(>F)`[2]

num_df_56 <- anova_56$Df[2]

den_df_56 <- anova_56$Res.Df[2]


assert_equal_integer(
  num_df_56,
  6,
  label = "Model 5 vs Model 6 numerator df"
)


assert_equal_integer(
  den_df_56,
  1631,
  label = "Model 5 vs Model 6 denominator df"
)


assert_display(
  f_56,
  1.164,
  digits = 3,
  label = "Model 5 vs Model 6 partial F"
)


assert_display(
  p_56,
  0.323,
  digits = 3,
  label = "Model 5 vs Model 6 partial-F p value"
)


nested_tests <- data.frame(

  comparison = c(
    "Model 3 vs Model 4: education x rural-origin block",
    "Model 5 vs Model 6: education x cohort block"
  ),

  numerator_df = c(
    num_df_34,
    num_df_56
  ),

  denominator_df = c(
    den_df_34,
    den_df_56
  ),

  F = c(
    f_34,
    f_56
  ),

  p = c(
    p_34,
    p_56
  )
)


cat(
  sprintf(
    paste0(
      "Model 3 vs 4 joint interaction test: ",
      "F(%d, %d) = %.6f, p = %.6f : PASSED\n"
    ),
    num_df_34,
    den_df_34,
    f_34,
    p_34
  )
)


cat(
  sprintf(
    paste0(
      "Model 5 vs 6 joint interaction test: ",
      "F(%d, %d) = %.6f, p = %.6f : PASSED\n"
    ),
    num_df_56,
    den_df_56,
    f_56,
    p_56
  )
)


# ------------------------------------------------------------
# 14. Build coefficient output
# ------------------------------------------------------------

coefficients_all <- do.call(
  rbind,
  lapply(
    names(models),
    function(nm) {
      extract_coefficients(
        models[[nm]],
        nm
      )
    }
  )
)

row.names(coefficients_all) <- NULL


# ------------------------------------------------------------
# 15. Output directory
# ------------------------------------------------------------

output_dir <-
  "output/original_replication"


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 16. Save model outputs
# ------------------------------------------------------------

write.csv(
  coefficients_all,
  file.path(
    output_dir,
    "original_models_coefficients.csv"
  ),
  row.names = FALSE
)


write.csv(
  fit_all,
  file.path(
    output_dir,
    "original_models_fit_statistics.csv"
  ),
  row.names = FALSE
)


write.csv(
  vif_all,
  file.path(
    output_dir,
    "original_models_vif.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 17. Save categorical tables
# ------------------------------------------------------------

table6_export <- as.data.frame(
  table6_origin_education
)


names(table6_export) <- c(
  "rural_origin",
  "edu_lvl",
  "n"
)


write.csv(
  table6_export,
  file.path(
    output_dir,
    "table6_origin_by_education.csv"
  ),
  row.names = FALSE
)


table_a1_export <- as.data.frame(
  table_a1_cohort_education
)


names(table_a1_export) <- c(
  "cohort",
  "edu_lvl",
  "n"
)


write.csv(
  table_a1_export,
  file.path(
    output_dir,
    "appendix_a1_cohort_by_education.csv"
  ),
  row.names = FALSE
)


write.csv(
  table7,
  file.path(
    output_dir,
    "table7_isei_by_cohort_and_education.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 18. Save interaction tests
# ------------------------------------------------------------

write.csv(
  nested_tests,
  file.path(
    output_dir,
    "original_joint_interaction_tests.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 19. Final validation report
# ------------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  sep = ""
)


cat(
  "04_original_models.R: ALL CHECKS PASSED\n"
)


cat(
  "============================================================\n"
)


cat(
  "Analytic N:                         ",
  nrow(d),
  "\n"
)


cat(
  "Model 1 Bachelor B:                 ",
  sprintf(
    "%.6f",
    coef(model1)["edu_bachelor"]
  ),
  "\n"
)


cat(
  "Model 1 Postgraduate B:             ",
  sprintf(
    "%.6f",
    coef(model1)["edu_postgrad"]
  ),
  "\n"
)


cat(
  "Model 3 Rural-origin B:             ",
  sprintf(
    "%.8f",
    coef(model3)["rural_origin"]
  ),
  "\n"
)


cat(
  "Model 5 cohort2 B:                  ",
  sprintf(
    "%.6f",
    coef(model5)["cohort2"]
  ),
  "\n"
)


cat(
  "Model 5 cohort3 B:                  ",
  sprintf(
    "%.6f",
    coef(model5)["cohort3"]
  ),
  "\n"
)


cat(
  "Model 5 cohort4 B:                  ",
  sprintf(
    "%.6f",
    coef(model5)["cohort4"]
  ),
  "\n"
)


cat(
  "Max VIF Models 1-5 and 7:           ",
  sprintf(
    "%.6f",
    max_vif_1to5_7
  ),
  "\n"
)


cat(
  "Model 6 Postgraduate VIF:           ",
  sprintf(
    "%.6f",
    vif_model6["edu_postgrad"]
  ),
  "\n"
)


cat(
  "Model 6 post_x_c3 B / SE / VIF:     ",
  sprintf(
    "%.6f",
    coef(model6)["post_x_c3"]
  ),
  " / ",
  sprintf(
    "%.6f",
    summary(model6)$coefficients[
      "post_x_c3",
      "Std. Error"
    ]
  ),
  " / ",
  sprintf(
    "%.6f",
    vif_model6["post_x_c3"]
  ),
  "\n",
  sep = ""
)


cat(
  "Table 6 Pearson chi-square:         ",
  sprintf(
    "%.6f",
    unname(
      chi_origin_education$statistic
    )
  ),
  "\n"
)


cat(
  "Appendix A1 Pearson chi-square:     ",
  sprintf(
    "%.6f",
    unname(
      chi_cohort_education$statistic
    )
  ),
  "\n"
)


cat(
  "Model 3 vs 4 partial F:             ",
  sprintf(
    "%.6f",
    f_34
  ),
  "  p = ",
  sprintf(
    "%.6f",
    p_34
  ),
  "\n",
  sep = ""
)


cat(
  "Model 5 vs 6 partial F:             ",
  sprintf(
    "%.6f",
    f_56
  ),
  "  p = ",
  sprintf(
    "%.6f",
    p_56
  ),
  "\n",
  sep = ""
)


cat(
  "\nOutput written to:\n",
  output_dir,
  "\n",
  sep = ""
)


cat(
  "============================================================\n"
)


cat(
  "Historical Models 1-7 reproduction complete.\n"
)


cat(
  "Do NOT introduce revised specifications into this file.\n"
)


cat(
  "============================================================\n"
)