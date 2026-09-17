# =============================================================================
# 05_isei_crosswalk_audit.R
#
# ISEI measurement audit
#
# This stage compares the historical ISEI construction with two documented
# ISCO-08 to ISEI-08 mappings before any revised regression models are run.
#
# Historical baseline:
#   R/01_import_validate.R
#   R/02_construct_variables.R
#   R/03_original_isei.R
#
# Revised primary mapping:
#   ISCO08ConveRsions::isco08toisei08
#
# Sensitivity mapping:
#   ISCO08ConveRsions::isco08toisei08_2
#
# Stage 05 does not alter Stages 01-04 and does not estimate regression models.
# No respondent-level CGSS data are exported.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Dependency and verified historical baseline
# -----------------------------------------------------------------------------

if (!requireNamespace("ISCO08ConveRsions", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'ISCO08ConveRsions' is required.\n",
      "Install it once in the R Console with:\n",
      "install.packages(\"ISCO08ConveRsions\")"
    ),
    call. = FALSE
  )
}

source("R/03_original_isei.R")

cat(
  "\n",
  "============================================================\n",
  "Stage 05 - ISEI measurement audit\n",
  "============================================================\n",
  sep = ""
)

cat(
  "ISCO08ConveRsions version: ",
  as.character(packageVersion("ISCO08ConveRsions")),
  "\n",
  sep = ""
)


# -----------------------------------------------------------------------------
# 1. General helpers
# -----------------------------------------------------------------------------

assert_true <- function(condition, message) {

  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }

  invisible(TRUE)
}


strict_map_one <- function(code, fun, mapping_name) {

  out <- tryCatch(
    suppressWarnings(fun(code)),
    error = function(e) {
      stop(
        paste0(
          mapping_name,
          " failed for ISCO code ",
          code,
          ": ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  if (length(out) != 1L) {
    stop(
      paste0(
        mapping_name,
        " returned ",
        length(out),
        " values for ISCO code ",
        code,
        "; exactly one value was expected."
      ),
      call. = FALSE
    )
  }

  value <- suppressWarnings(as.numeric(out))

  if (length(value) != 1L) {
    stop(
      paste0(
        mapping_name,
        " could not be converted to one numeric value for ISCO code ",
        code,
        "."
      ),
      call. = FALSE
    )
  }

  if (!is.na(value) && !is.finite(value)) {
    stop(
      paste0(
        mapping_name,
        " returned a non-finite value for ISCO code ",
        code,
        "."
      ),
      call. = FALSE
    )
  }

  value
}




probe_map_one <- function(code, fun, mapping_name) {

  out <- tryCatch(
    suppressWarnings(fun(code)),
    error = function(e) NA_real_
  )

  if (length(out) != 1L) {
    return(NA_real_)
  }

  value <- suppressWarnings(as.numeric(out))

  if (length(value) != 1L) {
    return(NA_real_)
  }

  if (!is.na(value) && !is.finite(value)) {
    return(NA_real_)
  }

  value
}

coverage_row <- function(score,
                         population_flag,
                         mapping_name,
                         population_name) {

  assert_true(
    length(score) == length(population_flag),
    "coverage_row(): score and population_flag have different lengths."
  )

  population_flag[is.na(population_flag)] <- FALSE

  n_total <- sum(population_flag)

  n_mapped <- sum(
    population_flag &
      !is.na(score)
  )

  data.frame(
    population = population_name,
    mapping = mapping_name,
    n_total = n_total,
    n_mapped = n_mapped,
    n_missing = n_total - n_mapped,
    pct_mapped = if (n_total > 0L) {
      100 * n_mapped / n_total
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}


transition_table <- function(old_score,
                             new_score,
                             population_flag,
                             old_name,
                             new_name,
                             population_name) {

  assert_true(
    length(old_score) == length(new_score) &&
      length(old_score) == length(population_flag),
    "transition_table(): input lengths differ."
  )

  population_flag[is.na(population_flag)] <- FALSE

  old_state <- ifelse(
    is.na(old_score[population_flag]),
    "missing",
    "mapped"
  )

  new_state <- ifelse(
    is.na(new_score[population_flag]),
    "missing",
    "mapped"
  )

  tab <- as.data.frame(
    table(
      old_state = factor(
        old_state,
        levels = c("missing", "mapped")
      ),
      new_state = factor(
        new_state,
        levels = c("missing", "mapped")
      )
    ),
    stringsAsFactors = FALSE
  )

  names(tab)[3L] <- "n"

  tab$population <- population_name
  tab$old_mapping <- old_name
  tab$new_mapping <- new_name

  tab[
    ,
    c(
      "population",
      "old_mapping",
      "new_mapping",
      "old_state",
      "new_state",
      "n"
    )
  ]
}


comparison_row <- function(old_score,
                           new_score,
                           population_flag,
                           comparison_name,
                           level_name) {

  assert_true(
    length(old_score) == length(new_score) &&
      length(old_score) == length(population_flag),
    "comparison_row(): input lengths differ."
  )

  population_flag[is.na(population_flag)] <- FALSE

  keep <- population_flag &
    !is.na(old_score) &
    !is.na(new_score)

  old <- old_score[keep]
  new <- new_score[keep]

  if (length(old) == 0L) {
    return(
      data.frame(
        level = level_name,
        comparison = comparison_name,
        n_common = 0L,
        correlation = NA_real_,
        mean_old = NA_real_,
        mean_new = NA_real_,
        mean_change = NA_real_,
        median_change = NA_real_,
        sd_change = NA_real_,
        mean_abs_change = NA_real_,
        max_abs_change = NA_real_,
        pct_exactly_unchanged = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  change <- new - old

  correlation_value <- NA_real_

  if (
    length(change) > 1L &&
    sd(old) > 0 &&
    sd(new) > 0
  ) {
    correlation_value <- cor(old, new)
  }

  data.frame(
    level = level_name,
    comparison = comparison_name,
    n_common = length(change),
    correlation = correlation_value,
    mean_old = mean(old),
    mean_new = mean(new),
    mean_change = mean(change),
    median_change = median(change),
    sd_change = if (length(change) > 1L) {
      sd(change)
    } else {
      NA_real_
    },
    mean_abs_change = mean(abs(change)),
    max_abs_change = max(abs(change)),
    pct_exactly_unchanged = 100 * mean(change == 0),
    stringsAsFactors = FALSE
  )
}


major_group_comparison <- function(data,
                                   old_var,
                                   new_var,
                                   comparison_name) {

  keep <- (
    data$core_flag &
      !is.na(data[[old_var]]) &
      !is.na(data[[new_var]])
  )

  z <- data[
    keep,
    c(
      "major_group",
      old_var,
      new_var
    ),
    drop = FALSE
  ]

  if (nrow(z) == 0L) {
    return(
      data.frame(
        comparison = character(0),
        major_group = character(0),
        n = integer(0),
        mean_old = numeric(0),
        mean_new = numeric(0),
        mean_change = numeric(0),
        median_change = numeric(0),
        mean_abs_change = numeric(0),
        max_abs_change = numeric(0),
        stringsAsFactors = FALSE
      )
    )
  }

  names(z)[2:3] <- c(
    "old_score",
    "new_score"
  )

  z$change <- (
    z$new_score -
      z$old_score
  )

  groups <- sort(
    unique(z$major_group)
  )

  out <- do.call(
    rbind,
    lapply(
      groups,
      function(g) {

        x <- z[
          z$major_group == g,
          ,
          drop = FALSE
        ]

        data.frame(
          comparison = comparison_name,
          major_group = g,
          n = nrow(x),
          mean_old = mean(x$old_score),
          mean_new = mean(x$new_score),
          mean_change = mean(x$change),
          median_change = median(x$change),
          mean_abs_change = mean(abs(x$change)),
          max_abs_change = max(abs(x$change)),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  rownames(out) <- NULL

  out
}


# -----------------------------------------------------------------------------
# 2. Consume the verified Stage-03 object
# -----------------------------------------------------------------------------

assert_true(
  exists(
    "stage3_dat",
    inherits = FALSE
  ),
  paste0(
    "Stage 03 did not leave the expected object 'stage3_dat' ",
    "in memory."
  )
)

assert_true(
  nrow(stage3_dat) == 1674L,
  paste0(
    "stage3_dat should contain 1,674 occupation-valid cases. ",
    "Observed N = ",
    nrow(stage3_dat),
    "."
  )
)

required_stage3_vars <- c(
  "isco_main",
  "isei",
  "rural_origin",
  "par_edu_yrs",
  "edu_lvl",
  "cohort"
)

missing_stage3_vars <- setdiff(
  required_stage3_vars,
  names(stage3_dat)
)

assert_true(
  length(missing_stage3_vars) == 0L,
  paste0(
    "stage3_dat is missing required variable(s): ",
    paste(
      missing_stage3_vars,
      collapse = ", "
    )
  )
)

assert_true(
  !"isco4" %in% names(stage3_dat),
  paste0(
    "stage3_dat already contains a variable named 'isco4'. ",
    "Stage 05 will not overwrite an existing variable."
  )
)

stage3_audit <- stage3_dat


# -----------------------------------------------------------------------------
# 3. Validate and canonicalise ISCO codes
# -----------------------------------------------------------------------------

isco_numeric <- suppressWarnings(
  as.numeric(
    stage3_audit$isco_main
  )
)

assert_true(
  all(!is.na(isco_numeric)),
  "Missing or non-numeric isco_main values were found in stage3_dat."
)

assert_true(
  all(isco_numeric > 0),
  "All Stage-3 ISCO codes must be greater than zero."
)

assert_true(
  all(isco_numeric < 10000),
  "All Stage-3 ISCO codes must be below 10,000."
)

assert_true(
  all(
    abs(
      isco_numeric -
        round(isco_numeric)
    ) < 1e-8
  ),
  "Non-integer occupational codes were found."
)

stage3_audit$isco_numeric <- as.integer(
  round(isco_numeric)
)

stage3_audit$isco_raw_char <- as.character(
  stage3_audit$isco_numeric
)

stage3_audit$isco4 <- sprintf(
  "%04d",
  stage3_audit$isco_numeric
)

stage3_audit$major_group <- substr(
  stage3_audit$isco4,
  1L,
  1L
)

assert_true(
  all(
    nchar(stage3_audit$isco4) == 4L
  ),
  "Canonical ISCO codes are not all four characters long."
)


# -----------------------------------------------------------------------------
# 4. Historical sample anchors
# -----------------------------------------------------------------------------

stage3_audit$isei_historical <- suppressWarnings(
  as.numeric(
    stage3_audit$isei
  )
)

stage3_audit$core_flag <- (
  !is.na(stage3_audit$rural_origin) &
    !is.na(stage3_audit$par_edu_yrs)
)

n_core <- sum(
  stage3_audit$core_flag
)

assert_true(
  n_core == 1650L,
  paste0(
    "Pre-ISEI core should contain 1,650 cases. ",
    "Observed N = ",
    n_core,
    "."
  )
)

stage3_audit$historical_final_flag <- (
  stage3_audit$core_flag &
    !is.na(stage3_audit$isei_historical)
)

n_historical_final <- sum(
  stage3_audit$historical_final_flag
)

assert_true(
  n_historical_final == 1646L,
  paste0(
    "Historical final analytic sample should contain 1,646 cases. ",
    "Observed N = ",
    n_historical_final,
    "."
  )
)


# -----------------------------------------------------------------------------
# 5. Check historical ISEI uniqueness within ISCO code
# -----------------------------------------------------------------------------

historical_split <- split(
  stage3_audit$isei_historical,
  stage3_audit$isco4
)

historical_nonmissing_unique <- lapply(
  historical_split,
  function(x) {

    unique(
      x[
        !is.na(x)
      ]
    )
  }
)

bad_historical_codes <- names(
  historical_nonmissing_unique
)[
  lengths(
    historical_nonmissing_unique
  ) > 1L
]

assert_true(
  length(bad_historical_codes) == 0L,
  paste0(
    "Historical ISEI is not unique within ISCO code(s): ",
    paste(
      bad_historical_codes,
      collapse = ", "
    )
  )
)


# -----------------------------------------------------------------------------
# 6. Build one unique code-level mapping table
# -----------------------------------------------------------------------------

unique_codes <- sort(
  unique(
    stage3_audit$isco4
  )
)

code_map <- data.frame(
  isco4 = unique_codes,
  stringsAsFactors = FALSE
)

code_map$isco_numeric <- as.integer(
  code_map$isco4
)

code_map$major_group <- substr(
  code_map$isco4,
  1L,
  1L
)


# Historical mapping

code_map$isei_historical <- vapply(
  code_map$isco4,
  function(code) {

    values <- historical_nonmissing_unique[[code]]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    assert_true(
      length(values) == 1L,
      paste0(
        "Historical ISEI unexpectedly has more than one value for code ",
        code,
        "."
      )
    )

    values[1L]
  },
  numeric(1)
)


# Revised primary mapping

code_map$isei_syntax <- vapply(
  code_map$isco4,
  strict_map_one,
  numeric(1),
  fun = ISCO08ConveRsions::isco08toisei08,
  mapping_name = "syntax-primary mapping"
)


# Overview-table sensitivity mapping

code_map$isei_overview <- vapply(
  code_map$isco4,
  strict_map_one,
  numeric(1),
  fun = ISCO08ConveRsions::isco08toisei08_2,
  mapping_name = "overview-sensitivity mapping"
)


# -----------------------------------------------------------------------------
# 7. Code frequencies
# -----------------------------------------------------------------------------

stage3_counts <- table(
  stage3_audit$isco4
)

core_counts <- table(
  stage3_audit$isco4[
    stage3_audit$core_flag
  ]
)

code_map$n_stage3 <- as.integer(
  stage3_counts[
    code_map$isco4
  ]
)

core_match <- match(
  code_map$isco4,
  names(core_counts)
)

code_map$n_core <- 0L

has_core_count <- !is.na(
  core_match
)

code_map$n_core[
  has_core_count
] <- as.integer(
  core_counts[
    core_match[
      has_core_count
    ]
  ]
)

assert_true(
  sum(code_map$n_stage3) == 1674L,
  "Code-level Stage-3 frequencies do not sum to 1,674."
)

assert_true(
  sum(code_map$n_core) == 1650L,
  "Code-level pre-ISEI frequencies do not sum to 1,650."
)


# -----------------------------------------------------------------------------
# 8. Match code-level mappings back to respondents
# -----------------------------------------------------------------------------

code_index <- match(
  stage3_audit$isco4,
  code_map$isco4
)

assert_true(
  !anyNA(code_index),
  "At least one respondent ISCO code failed to match the code-level map."
)

stage3_audit$isei_syntax <- code_map$isei_syntax[
  code_index
]

stage3_audit$isei_overview <- code_map$isei_overview[
  code_index
]


# Exact consistency checks after matching

syntax_matched_again <- code_map$isei_syntax[
  code_index
]

overview_matched_again <- code_map$isei_overview[
  code_index
]

syntax_equal <- (
  (
    is.na(stage3_audit$isei_syntax) &
      is.na(syntax_matched_again)
  ) |
    (
      !is.na(stage3_audit$isei_syntax) &
        !is.na(syntax_matched_again) &
        stage3_audit$isei_syntax ==
        syntax_matched_again
    )
)

overview_equal <- (
  (
    is.na(stage3_audit$isei_overview) &
      is.na(overview_matched_again)
  ) |
    (
      !is.na(stage3_audit$isei_overview) &
        !is.na(overview_matched_again) &
        stage3_audit$isei_overview ==
        overview_matched_again
    )
)

assert_true(
  all(syntax_equal),
  "Syntax scores do not reproduce the code-level map after match()."
)

assert_true(
  all(overview_equal),
  "Overview scores do not reproduce the code-level map after match()."
)


# -----------------------------------------------------------------------------
# 9. Representation audit
# -----------------------------------------------------------------------------

representation_numeric <- c(
  100L,
  110L,
  310L,
  1000L,
  1200L,
  2300L,
  2350L,
  5200L,
  7000L,
  1111L,
  6111L,
  9313L
)

representation_test <- data.frame(
  numeric_code = representation_numeric,
  raw_char = as.character(
    representation_numeric
  ),
  padded_char = sprintf(
    "%04d",
    representation_numeric
  ),
  stringsAsFactors = FALSE
)

representation_test$syntax_raw <- vapply(
  representation_test$raw_char,
  probe_map_one,
  numeric(1),
  fun = ISCO08ConveRsions::isco08toisei08,
  mapping_name = "syntax-primary mapping"
)

representation_test$syntax_padded <- vapply(
  representation_test$padded_char,
  probe_map_one,
  numeric(1),
  fun = ISCO08ConveRsions::isco08toisei08,
  mapping_name = "syntax-primary mapping"
)

representation_test$overview_raw <- vapply(
  representation_test$raw_char,
  probe_map_one,
  numeric(1),
  fun = ISCO08ConveRsions::isco08toisei08_2,
  mapping_name = "overview-sensitivity mapping"
)

representation_test$overview_padded <- vapply(
  representation_test$padded_char,
  probe_map_one,
  numeric(1),
  fun = ISCO08ConveRsions::isco08toisei08_2,
  mapping_name = "overview-sensitivity mapping"
)


padding_rows <- representation_test$numeric_code %in% c(
  100L,
  110L,
  310L
)

assert_true(
  all(is.na(representation_test$syntax_raw[padding_rows])),
  "The syntax mapping unexpectedly accepted an unpadded leading-zero test code."
)

assert_true(
  all(!is.na(representation_test$syntax_padded[padding_rows])),
  "The syntax mapping failed after canonical four-character padding."
)

assert_true(
  all(is.na(representation_test$overview_raw[padding_rows])),
  "The overview mapping unexpectedly accepted an unpadded leading-zero test code."
)

assert_true(
  all(!is.na(representation_test$overview_padded[padding_rows])),
  "The overview mapping failed after canonical four-character padding."
)

cat(
  "\nRepresentation audit:\n"
)

print(
  representation_test,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 10. Person-level coverage
# -----------------------------------------------------------------------------

all_stage3_flag <- rep(
  TRUE,
  nrow(stage3_audit)
)

coverage_summary <- rbind(
  coverage_row(
    stage3_audit$isei_historical,
    all_stage3_flag,
    "historical",
    "Stage 3 occupation-valid"
  ),
  coverage_row(
    stage3_audit$isei_syntax,
    all_stage3_flag,
    "syntax_primary",
    "Stage 3 occupation-valid"
  ),
  coverage_row(
    stage3_audit$isei_overview,
    all_stage3_flag,
    "overview_sensitivity",
    "Stage 3 occupation-valid"
  ),
  coverage_row(
    stage3_audit$isei_historical,
    stage3_audit$core_flag,
    "historical",
    "Pre-ISEI core"
  ),
  coverage_row(
    stage3_audit$isei_syntax,
    stage3_audit$core_flag,
    "syntax_primary",
    "Pre-ISEI core"
  ),
  coverage_row(
    stage3_audit$isei_overview,
    stage3_audit$core_flag,
    "overview_sensitivity",
    "Pre-ISEI core"
  )
)

cat(
  "\nPerson-level coverage:\n"
)

print(
  coverage_summary,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 11. Code-level coverage
# -----------------------------------------------------------------------------

code_coverage_summary <- rbind(
  data.frame(
    population = "Stage 3 occupation-valid codes",
    mapping = "historical",
    n_total_codes = nrow(code_map),
    n_mapped_codes = sum(
      !is.na(code_map$isei_historical)
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    population = "Stage 3 occupation-valid codes",
    mapping = "syntax_primary",
    n_total_codes = nrow(code_map),
    n_mapped_codes = sum(
      !is.na(code_map$isei_syntax)
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    population = "Stage 3 occupation-valid codes",
    mapping = "overview_sensitivity",
    n_total_codes = nrow(code_map),
    n_mapped_codes = sum(
      !is.na(code_map$isei_overview)
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    population = "Pre-ISEI core codes",
    mapping = "historical",
    n_total_codes = sum(
      code_map$n_core > 0L
    ),
    n_mapped_codes = sum(
      code_map$n_core > 0L &
        !is.na(code_map$isei_historical)
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    population = "Pre-ISEI core codes",
    mapping = "syntax_primary",
    n_total_codes = sum(
      code_map$n_core > 0L
    ),
    n_mapped_codes = sum(
      code_map$n_core > 0L &
        !is.na(code_map$isei_syntax)
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    population = "Pre-ISEI core codes",
    mapping = "overview_sensitivity",
    n_total_codes = sum(
      code_map$n_core > 0L
    ),
    n_mapped_codes = sum(
      code_map$n_core > 0L &
        !is.na(code_map$isei_overview)
    ),
    stringsAsFactors = FALSE
  )
)

code_coverage_summary$n_missing_codes <- (
  code_coverage_summary$n_total_codes -
    code_coverage_summary$n_mapped_codes
)

code_coverage_summary$pct_codes_mapped <- (
  100 *
    code_coverage_summary$n_mapped_codes /
    code_coverage_summary$n_total_codes
)


# -----------------------------------------------------------------------------
# 12. Coverage transitions in both directions
# -----------------------------------------------------------------------------

coverage_transitions <- rbind(
  transition_table(
    stage3_audit$isei_historical,
    stage3_audit$isei_syntax,
    all_stage3_flag,
    "historical",
    "syntax_primary",
    "Stage 3 occupation-valid"
  ),
  transition_table(
    stage3_audit$isei_historical,
    stage3_audit$isei_syntax,
    stage3_audit$core_flag,
    "historical",
    "syntax_primary",
    "Pre-ISEI core"
  ),
  transition_table(
    stage3_audit$isei_syntax,
    stage3_audit$isei_overview,
    all_stage3_flag,
    "syntax_primary",
    "overview_sensitivity",
    "Stage 3 occupation-valid"
  ),
  transition_table(
    stage3_audit$isei_syntax,
    stage3_audit$isei_overview,
    stage3_audit$core_flag,
    "syntax_primary",
    "overview_sensitivity",
    "Pre-ISEI core"
  )
)

cat(
  "\nCoverage transitions:\n"
)

print(
  coverage_transitions,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 13. Mapping differences at code level
# -----------------------------------------------------------------------------

code_map$syntax_minus_historical <- (
  code_map$isei_syntax -
    code_map$isei_historical
)

code_map$overview_minus_syntax <- (
  code_map$isei_overview -
    code_map$isei_syntax
)

code_map$abs_syntax_minus_historical <- abs(
  code_map$syntax_minus_historical
)

code_map$abs_overview_minus_syntax <- abs(
  code_map$overview_minus_syntax
)

code_map$person_weighted_abs_hist_syntax <- (
  code_map$n_core *
    code_map$abs_syntax_minus_historical
)

code_map$person_weighted_abs_syntax_overview <- (
  code_map$n_core *
    code_map$abs_overview_minus_syntax
)


# -----------------------------------------------------------------------------
# 14. Common-support comparisons
# -----------------------------------------------------------------------------

person_comparisons <- rbind(
  comparison_row(
    stage3_audit$isei_historical,
    stage3_audit$isei_syntax,
    stage3_audit$core_flag,
    "historical_vs_syntax_primary",
    "person"
  ),
  comparison_row(
    stage3_audit$isei_syntax,
    stage3_audit$isei_overview,
    stage3_audit$core_flag,
    "syntax_primary_vs_overview_sensitivity",
    "person"
  )
)

code_population <- (
  code_map$n_core > 0L
)

code_comparisons <- rbind(
  comparison_row(
    code_map$isei_historical,
    code_map$isei_syntax,
    code_population,
    "historical_vs_syntax_primary",
    "code"
  ),
  comparison_row(
    code_map$isei_syntax,
    code_map$isei_overview,
    code_population,
    "syntax_primary_vs_overview_sensitivity",
    "code"
  )
)

common_support_summary <- rbind(
  person_comparisons,
  code_comparisons
)

cat(
  "\nCommon-support comparison:\n"
)

print(
  common_support_summary,
  row.names = FALSE
)


# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 14A. Complete pairwise and subgroup measurement audit
# -----------------------------------------------------------------------------

# Add the third pairwise comparison so that all three mappings are compared.

common_support_summary <- rbind(
  comparison_row(
    stage3_audit$isei_historical,
    stage3_audit$isei_syntax,
    stage3_audit$core_flag,
    "historical_vs_syntax_primary",
    "person"
  ),
  comparison_row(
    stage3_audit$isei_historical,
    stage3_audit$isei_overview,
    stage3_audit$core_flag,
    "historical_vs_overview_sensitivity",
    "person"
  ),
  comparison_row(
    stage3_audit$isei_syntax,
    stage3_audit$isei_overview,
    stage3_audit$core_flag,
    "syntax_primary_vs_overview_sensitivity",
    "person"
  ),
  comparison_row(
    code_map$isei_historical,
    code_map$isei_syntax,
    code_population,
    "historical_vs_syntax_primary",
    "code"
  ),
  comparison_row(
    code_map$isei_historical,
    code_map$isei_overview,
    code_population,
    "historical_vs_overview_sensitivity",
    "code"
  ),
  comparison_row(
    code_map$isei_syntax,
    code_map$isei_overview,
    code_population,
    "syntax_primary_vs_overview_sensitivity",
    "code"
  )
)

cat(
  "\nComplete pairwise common-support comparison:\n"
)

print(
  common_support_summary,
  row.names = FALSE
)


# Descriptive distribution of each mapping on its own analytic sample.

mapping_descriptive_row <- function(score, flag, mapping_name) {

  flag[is.na(flag)] <- FALSE

  values <- score[flag & !is.na(score)]

  assert_true(
    length(values) > 0L,
    paste0(
      "No valid observations found for mapping: ",
      mapping_name
    )
  )

  data.frame(
    mapping = mapping_name,
    n = length(values),
    mean = mean(values),
    sd = if (length(values) > 1L) sd(values) else NA_real_,
    min = min(values),
    max = max(values),
    stringsAsFactors = FALSE
  )
}

mapping_descriptives <- rbind(
  mapping_descriptive_row(
    stage3_audit$isei_historical,
    stage3_audit$core_flag & !is.na(stage3_audit$isei_historical),
    "historical"
  ),
  mapping_descriptive_row(
    stage3_audit$isei_syntax,
    stage3_audit$core_flag & !is.na(stage3_audit$isei_syntax),
    "syntax_primary"
  ),
  mapping_descriptive_row(
    stage3_audit$isei_overview,
    stage3_audit$core_flag & !is.na(stage3_audit$isei_overview),
    "overview_sensitivity"
  )
)

cat(
  "\nMapping descriptives:\n"
)

print(
  mapping_descriptives,
  row.names = FALSE
)


# Restrict subgroup comparisons to the same respondents observed under
# both the historical and syntax-primary mappings.

delta_flag <- (
  stage3_audit$core_flag &
    !is.na(stage3_audit$isei_historical) &
    !is.na(stage3_audit$isei_syntax)
)

measurement_delta <- stage3_audit[
  delta_flag,
  c(
    "edu_lvl",
    "rural_origin",
    "isei_historical",
    "isei_syntax"
  ),
  drop = FALSE
]

measurement_delta$syntax_minus_historical <- (
  measurement_delta$isei_syntax -
    measurement_delta$isei_historical
)

measurement_delta$abs_change <- abs(
  measurement_delta$syntax_minus_historical
)

assert_true(
  nrow(measurement_delta) == 1646L,
  paste0(
    "Historical-syntax common support should contain 1,646 respondents. ",
    "Observed N = ",
    nrow(measurement_delta),
    "."
  )
)

assert_true(
  !anyNA(measurement_delta$edu_lvl),
  "Missing education level found in the historical-syntax common support."
)

assert_true(
  !anyNA(measurement_delta$rural_origin),
  "Missing rural-origin value found in the historical-syntax common support."
)


summarise_delta_groups <- function(data, group_cols, grouping_name) {

  key <- interaction(
    data[group_cols],
    drop = TRUE,
    lex.order = TRUE,
    sep = "|"
  )

  pieces <- split(
    seq_len(nrow(data)),
    key,
    drop = TRUE
  )

  out <- do.call(
    rbind,
    lapply(
      pieces,
      function(idx) {

        d <- data[idx, , drop = FALSE]

        data.frame(
          grouping = grouping_name,
          edu_lvl = if ("edu_lvl" %in% group_cols) {
            as.character(d$edu_lvl[1L])
          } else {
            NA_character_
          },
          rural_origin = if ("rural_origin" %in% group_cols) {
            as.character(d$rural_origin[1L])
          } else {
            NA_character_
          },
          n = nrow(d),
          mean_historical = mean(d$isei_historical),
          mean_syntax = mean(d$isei_syntax),
          mean_change = mean(d$syntax_minus_historical),
          sd_change = if (nrow(d) > 1L) {
            sd(d$syntax_minus_historical)
          } else {
            NA_real_
          },
          mean_abs_change = mean(d$abs_change),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  rownames(out) <- NULL
  out
}


measurement_change_by_group <- rbind(
  summarise_delta_groups(
    measurement_delta,
    "edu_lvl",
    "education"
  ),
  summarise_delta_groups(
    measurement_delta,
    "rural_origin",
    "rural_origin"
  ),
  summarise_delta_groups(
    measurement_delta,
    c(
      "edu_lvl",
      "rural_origin"
    ),
    "education_by_rural_origin"
  )
)

assert_true(
  sum(
    measurement_change_by_group$n[
      measurement_change_by_group$grouping == "education"
    ]
  ) == 1646L,
  "Education-group measurement-change counts do not sum to 1,646."
)

assert_true(
  sum(
    measurement_change_by_group$n[
      measurement_change_by_group$grouping == "rural_origin"
    ]
  ) == 1646L,
  "Rural-origin measurement-change counts do not sum to 1,646."
)

assert_true(
  sum(
    measurement_change_by_group$n[
      measurement_change_by_group$grouping ==
        "education_by_rural_origin"
    ]
  ) == 1646L,
  "Education-by-origin measurement-change counts do not sum to 1,646."
)

cat(
  "\nMeasurement change by education and origin:\n"
)

print(
  measurement_change_by_group,
  row.names = FALSE
)


# The missing historical score for ISCO 1000 means historical-versus-
# revised comparisons have 1,646 respondents and 247 occupational codes.

hist_overview_person_n <- common_support_summary$n_common[
  common_support_summary$level == "person" &
    common_support_summary$comparison ==
      "historical_vs_overview_sensitivity"
]

hist_overview_code_n <- common_support_summary$n_common[
  common_support_summary$level == "code" &
    common_support_summary$comparison ==
      "historical_vs_overview_sensitivity"
]

assert_true(
  length(hist_overview_person_n) == 1L &&
    hist_overview_person_n == 1646L,
  "Historical-overview person-level common support should be 1,646."
)

assert_true(
  length(hist_overview_code_n) == 1L &&
    hist_overview_code_n == 247L,
  "Historical-overview code-level common support should contain 247 codes."
)

# 15. Score granularity
# -----------------------------------------------------------------------------

score_granularity <- data.frame(
  mapping = c(
    "historical",
    "syntax_primary",
    "overview_sensitivity"
  ),
  n_unique_scores_core = c(
    length(
      unique(
        stage3_audit$isei_historical[
          stage3_audit$core_flag &
            !is.na(stage3_audit$isei_historical)
        ]
      )
    ),
    length(
      unique(
        stage3_audit$isei_syntax[
          stage3_audit$core_flag &
            !is.na(stage3_audit$isei_syntax)
        ]
      )
    ),
    length(
      unique(
        stage3_audit$isei_overview[
          stage3_audit$core_flag &
            !is.na(stage3_audit$isei_overview)
        ]
      )
    )
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nScore granularity:\n"
)

print(
  score_granularity,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 16. Major-group comparisons
# -----------------------------------------------------------------------------

major_group_summary <- rbind(
  major_group_comparison(
    stage3_audit,
    "isei_historical",
    "isei_syntax",
    "historical_vs_syntax_primary"
  ),
  major_group_comparison(
    stage3_audit,
    "isei_syntax",
    "isei_overview",
    "syntax_primary_vs_overview_sensitivity"
  )
)


# -----------------------------------------------------------------------------
# 17. Largest code-level changes
# -----------------------------------------------------------------------------

top_absolute_hist_syntax <- code_map[
  code_map$n_core > 0L &
    !is.na(
      code_map$abs_syntax_minus_historical
    ),
  ,
  drop = FALSE
]

top_absolute_hist_syntax <- top_absolute_hist_syntax[
  order(
    -top_absolute_hist_syntax$abs_syntax_minus_historical,
    -top_absolute_hist_syntax$n_core,
    top_absolute_hist_syntax$isco4
  ),
  ,
  drop = FALSE
]

top_absolute_hist_syntax <- head(
  top_absolute_hist_syntax,
  20L
)


top_weighted_hist_syntax <- code_map[
  code_map$n_core > 0L &
    !is.na(
      code_map$person_weighted_abs_hist_syntax
    ),
  ,
  drop = FALSE
]

top_weighted_hist_syntax <- top_weighted_hist_syntax[
  order(
    -top_weighted_hist_syntax$person_weighted_abs_hist_syntax,
    -top_weighted_hist_syntax$abs_syntax_minus_historical,
    top_weighted_hist_syntax$isco4
  ),
  ,
  drop = FALSE
]

top_weighted_hist_syntax <- head(
  top_weighted_hist_syntax,
  20L
)


top_absolute_syntax_overview <- code_map[
  code_map$n_core > 0L &
    !is.na(
      code_map$abs_overview_minus_syntax
    ),
  ,
  drop = FALSE
]

top_absolute_syntax_overview <- top_absolute_syntax_overview[
  order(
    -top_absolute_syntax_overview$abs_overview_minus_syntax,
    -top_absolute_syntax_overview$n_core,
    top_absolute_syntax_overview$isco4
  ),
  ,
  drop = FALSE
]

top_absolute_syntax_overview <- head(
  top_absolute_syntax_overview,
  20L
)


# -----------------------------------------------------------------------------
# 18. Unmapped-code audit
# -----------------------------------------------------------------------------

historical_unmapped_codes <- code_map[
  code_map$n_core > 0L &
    is.na(code_map$isei_historical),
  ,
  drop = FALSE
]

syntax_unmapped_codes <- code_map[
  code_map$n_core > 0L &
    is.na(code_map$isei_syntax),
  ,
  drop = FALSE
]

overview_unmapped_codes <- code_map[
  code_map$n_core > 0L &
    is.na(code_map$isei_overview),
  ,
  drop = FALSE
]


# -----------------------------------------------------------------------------
# 19. Revised analytic sample boundaries
# -----------------------------------------------------------------------------

stage3_audit$syntax_final_flag <- (
  stage3_audit$core_flag &
    !is.na(stage3_audit$isei_syntax)
)

stage3_audit$overview_final_flag <- (
  stage3_audit$core_flag &
    !is.na(stage3_audit$isei_overview)
)

sample_boundary <- data.frame(
  mapping = c(
    "historical",
    "syntax_primary",
    "overview_sensitivity"
  ),
  analytic_n = c(
    sum(
      stage3_audit$historical_final_flag
    ),
    sum(
      stage3_audit$syntax_final_flag
    ),
    sum(
      stage3_audit$overview_final_flag
    )
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nAnalytic sample boundary:\n"
)

print(
  sample_boundary,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 20. Education and origin composition
# -----------------------------------------------------------------------------

composition_table <- function(flag, mapping_name) {

  flag[is.na(flag)] <- FALSE

  x <- stage3_audit[
    flag,
    c(
      "edu_lvl",
      "rural_origin"
    ),
    drop = FALSE
  ]

  tab <- as.data.frame(
    table(
      edu_lvl = x$edu_lvl,
      rural_origin = x$rural_origin
    ),
    stringsAsFactors = FALSE
  )

  names(tab)[3L] <- "n"

  tab$mapping <- mapping_name

  tab[
    ,
    c(
      "mapping",
      "edu_lvl",
      "rural_origin",
      "n"
    )
  ]
}

sample_composition <- rbind(
  composition_table(
    stage3_audit$historical_final_flag,
    "historical"
  ),
  composition_table(
    stage3_audit$syntax_final_flag,
    "syntax_primary"
  ),
  composition_table(
    stage3_audit$overview_final_flag,
    "overview_sensitivity"
  )
)


# -----------------------------------------------------------------------------
# 21. Conditional sample-boundary diagnostic
# -----------------------------------------------------------------------------

hist_missing_syntax_mapped_core <- (
  stage3_audit$core_flag &
    is.na(stage3_audit$isei_historical) &
    !is.na(stage3_audit$isei_syntax)
)

hist_mapped_syntax_missing_core <- (
  stage3_audit$core_flag &
    !is.na(stage3_audit$isei_historical) &
    is.na(stage3_audit$isei_syntax)
)

n_rescued <- sum(
  hist_missing_syntax_mapped_core
)

n_lost <- sum(
  hist_mapped_syntax_missing_core
)

historical_urban_n <- sum(
  stage3_audit$historical_final_flag &
    stage3_audit$rural_origin == 0,
  na.rm = TRUE
)

syntax_urban_n <- sum(
  stage3_audit$syntax_final_flag &
    stage3_audit$rural_origin == 0,
  na.rm = TRUE
)

historical_bachelor_n <- sum(
  stage3_audit$historical_final_flag &
    stage3_audit$edu_lvl == 2,
  na.rm = TRUE
)

syntax_bachelor_n <- sum(
  stage3_audit$syntax_final_flag &
    stage3_audit$edu_lvl == 2,
  na.rm = TRUE
)

conditional_diagnostic <- data.frame(
  quantity = c(
    "historical_analytic_n",
    "syntax_analytic_n",
    "historical_urban_n",
    "syntax_urban_n",
    "historical_bachelor_n",
    "syntax_bachelor_n",
    "historical_missing_to_syntax_mapped",
    "historical_mapped_to_syntax_missing"
  ),
  observed = c(
    sum(stage3_audit$historical_final_flag),
    sum(stage3_audit$syntax_final_flag),
    historical_urban_n,
    syntax_urban_n,
    historical_bachelor_n,
    syntax_bachelor_n,
    n_rescued,
    n_lost
  ),
  conditional_reference = c(
    1646,
    1650,
    928,
    932,
    806,
    810,
    4,
    0
  ),
  stringsAsFactors = FALSE
)

conditional_diagnostic$matches_conditional_reference <- (
  conditional_diagnostic$observed ==
    conditional_diagnostic$conditional_reference
)

cat(
  "\nConditional sample-boundary diagnostic:\n"
)

print(
  conditional_diagnostic,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 22. Focus codes
# -----------------------------------------------------------------------------

focus_codes <- c(
  "0100",
  "0110",
  "0310",
  "1000",
  "1200",
  "2300",
  "2350",
  "5200",
  "7000",
  "1111",
  "6111",
  "9313"
)

focus_index <- match(
  focus_codes,
  code_map$isco4
)

focus_code_audit <- code_map[
  focus_index[
    !is.na(focus_index)
  ],
  ,
  drop = FALSE
]

cat(
  "\nSelected occupational-code audit:\n"
)

print(
  focus_code_audit[
    ,
    c(
      "isco4",
      "n_stage3",
      "n_core",
      "isei_historical",
      "isei_syntax",
      "isei_overview",
      "syntax_minus_historical",
      "overview_minus_syntax"
    )
  ],
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 23. Package provenance
# -----------------------------------------------------------------------------

package_provenance <- data.frame(
  package = "ISCO08ConveRsions",
  version = as.character(
    packageVersion("ISCO08ConveRsions")
  ),
  primary_function = "isco08toisei08",
  sensitivity_function = "isco08toisei08_2",
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# 24. Write aggregate outputs only
# -----------------------------------------------------------------------------

output_dir_05 <- "output/revised_measurement"

dir.create(
  output_dir_05,
  recursive = TRUE,
  showWarnings = FALSE
)


write.csv(
  package_provenance,
  file.path(
    output_dir_05,
    "05_package_provenance.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  representation_test,
  file.path(
    output_dir_05,
    "05_code_representation_test.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  coverage_summary,
  file.path(
    output_dir_05,
    "05_person_coverage_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  code_coverage_summary,
  file.path(
    output_dir_05,
    "05_code_coverage_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  coverage_transitions,
  file.path(
    output_dir_05,
    "05_coverage_transitions.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  code_map,
  file.path(
    output_dir_05,
    "05_code_level_mapping_audit.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  common_support_summary,
  file.path(
    output_dir_05,
    "05_common_support_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  score_granularity,
  file.path(
    output_dir_05,
    "05_score_granularity.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  major_group_summary,
  file.path(
    output_dir_05,
    "05_major_group_change_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  top_absolute_hist_syntax,
  file.path(
    output_dir_05,
    "05_top_absolute_historical_to_syntax.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  top_weighted_hist_syntax,
  file.path(
    output_dir_05,
    "05_top_person_weighted_historical_to_syntax.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  top_absolute_syntax_overview,
  file.path(
    output_dir_05,
    "05_top_absolute_syntax_to_overview.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  historical_unmapped_codes,
  file.path(
    output_dir_05,
    "05_historical_unmapped_codes.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  syntax_unmapped_codes,
  file.path(
    output_dir_05,
    "05_syntax_unmapped_codes.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  overview_unmapped_codes,
  file.path(
    output_dir_05,
    "05_overview_unmapped_codes.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  sample_boundary,
  file.path(
    output_dir_05,
    "05_sample_boundary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  sample_composition,
  file.path(
    output_dir_05,
    "05_sample_composition.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  conditional_diagnostic,
  file.path(
    output_dir_05,
    "05_conditional_sample_diagnostic.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  focus_code_audit,
  file.path(
    output_dir_05,
    "05_focus_code_audit.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 24A. Additional measurement-audit outputs
# -----------------------------------------------------------------------------

write.csv(
  mapping_descriptives,
  file.path(
    output_dir_05,
    "05_mapping_descriptives.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  measurement_change_by_group,
  file.path(
    output_dir_05,
    "05_measurement_change_by_group.csv"
  ),
  row.names = FALSE,
  na = ""
)

# 25. Final integrity checks
# -----------------------------------------------------------------------------

assert_true(
  nrow(stage3_audit) == 1674L,
  "Stage-3 N changed unexpectedly during Stage 05."
)

assert_true(
  sum(stage3_audit$core_flag) == 1650L,
  "Pre-ISEI core N changed unexpectedly during Stage 05."
)

assert_true(
  sum(stage3_audit$historical_final_flag) == 1646L,
  "Historical final N changed unexpectedly during Stage 05."
)

assert_true(
  nrow(code_map) ==
    length(
      unique(
        stage3_audit$isco4
      )
    ),
  "Code-level mapping table does not contain exactly one row per ISCO code."
)

assert_true(
  all(
    stage3_audit$isco4 ==
      sprintf(
        "%04d",
        stage3_audit$isco_numeric
      )
  ),
  "Canonical four-character ISCO conversion failed."
)

assert_true(
  sum(code_map$n_stage3) == nrow(stage3_audit),
  "Code-level frequencies no longer reproduce the Stage-3 sample."
)

assert_true(
  sum(code_map$n_core) == sum(stage3_audit$core_flag),
  "Code-level frequencies no longer reproduce the pre-ISEI core."
)


# -----------------------------------------------------------------------------
# 26. Final console summary
# -----------------------------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "05_isei_crosswalk_audit.R: ALL CHECKS PASSED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "Unique Stage-3 ISCO codes: ",
  nrow(code_map),
  "\n",
  sep = ""
)

cat(
  "Stage-3 occupation-valid N: ",
  nrow(stage3_audit),
  "\n",
  sep = ""
)

cat(
  "Pre-ISEI core N: ",
  sum(stage3_audit$core_flag),
  "\n",
  sep = ""
)

cat(
  "Historical analytic N: ",
  sum(stage3_audit$historical_final_flag),
  "\n",
  sep = ""
)

cat(
  "Syntax-primary analytic N: ",
  sum(stage3_audit$syntax_final_flag),
  "\n",
  sep = ""
)

cat(
  "Overview-sensitivity analytic N: ",
  sum(stage3_audit$overview_final_flag),
  "\n",
  sep = ""
)

cat(
  "Historical missing -> syntax mapped: ",
  n_rescued,
  "\n",
  sep = ""
)

cat(
  "Historical mapped -> syntax missing: ",
  n_lost,
  "\n",
  sep = ""
)

cat(
  "\nOutputs written to:\n",
  output_dir_05,
  "\n",
  sep = ""
)

cat(
  "\nNo revised regression models were estimated in Stage 05.\n"
)
