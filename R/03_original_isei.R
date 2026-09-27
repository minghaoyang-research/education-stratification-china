# ============================================================
# 03_original_isei.R
# Original dissertation reproduction
#
# Purpose:
#   Faithfully reconstruct the historical custom ISEI mapping
#   used in the submitted dissertation.
#
# Historical specification:
#   archive/cgss2017_analysis_4.sps
#
# Workflow:
#   1. Rebuild Stage 01 and Stage 02 from source.
#   2. Start with ALL N = 1,674 Stage-3 occupation cases.
#   3. Read the final v4 SPSS syntax directly.
#   4. Extract the historical ISEI IF rules.
#   5. Execute them sequentially in original SPSS order.
#   6. Audit mapping coverage and boundary cases.
#   7. Reconstruct the final historical N = 1,646 sample.
#   8. Validate against final v4 SPSS output.
#
# IMPORTANT:
#   - This intentionally reproduces the historical mapping,
#     including its known limitations.
#   - It does NOT use the revised Ganzeboom crosswalk.
# ============================================================


# ------------------------------------------------------------
# 0. Rebuild validated Stage-02 objects
# ------------------------------------------------------------

source("R/02_construct_variables.R")

stopifnot(nrow(stage3_dat) == 1674)
stopifnot(n_stage3_core == 1650)


# ------------------------------------------------------------
# 0A. Helper for SPSS-display-precision validation
# ------------------------------------------------------------

# SPSS output prints different statistics to different numbers
# of decimal places.
#
# Rather than comparing floating-point numbers with exact ==,
# this helper checks whether an R result lies within one-half
# of the final displayed decimal unit.

near_display <- function(x, target, digits) {
  
  stopifnot(length(x) == 1)
  stopifnot(length(target) == 1)
  stopifnot(length(digits) == 1)
  
  stopifnot(is.finite(x))
  stopifnot(is.finite(target))
  
  tol <- 0.5 * 10^(-digits)
  
  stopifnot(
    abs(x - target) <
      tol + sqrt(.Machine$double.eps)
  )
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 1. Locate archived final v4 SPSS syntax
# ------------------------------------------------------------

v4_syntax_file <-
  "archive/cgss2017_analysis_4.sps"

stopifnot(
  file.exists(v4_syntax_file)
)

stopifnot(
  file.info(v4_syntax_file)$size > 0
)


# Record the historical syntax fingerprint.

v4_syntax_md5 <- unname(
  tools::md5sum(v4_syntax_file)
)


cat(
  "\n========================================\n"
)

cat(
  "Stage 03 - Historical ISEI reproduction\n"
)

cat(
  "========================================\n"
)

cat(
  "SPSS specification:",
  v4_syntax_file,
  "\n"
)

cat(
  "SPSS syntax MD5:",
  v4_syntax_md5,
  "\n"
)

cat(
  "Stage-3 cases:",
  nrow(stage3_dat),
  "\n\n"
)


# ------------------------------------------------------------
# 2. Read original SPSS syntax
# ------------------------------------------------------------

v4_lines <- readLines(
  v4_syntax_file,
  warn = FALSE,
  encoding = "UTF-8"
)

v4_lines_trimmed <- trimws(
  v4_lines
)

cat(
  "SPSS syntax lines read:",
  length(v4_lines_trimmed),
  "\n"
)


# ------------------------------------------------------------
# 3. Extract historical ISEI assignment rules
# ------------------------------------------------------------

# Historical v4 rules occur in forms such as:
#
# IF isco_main=1111 isei=70.
#
# or:
#
# IF (MISSING(isei) AND isco_main GE 1100
#     AND isco_main LE 1199) isei=70.
#
# The pattern accepts numeric assignments and $SYSMIS.

isei_rule_pattern <- paste0(
  "^IF\\s*",
  ".*?",
  "\\s+isei\\s*=\\s*",
  "(?:\\$SYSMIS|-?[0-9]+(?:\\.[0-9]+)?)",
  "\\s*\\.\\s*$"
)


isei_rule_lines <- v4_lines_trimmed[
  grepl(
    isei_rule_pattern,
    v4_lines_trimmed,
    ignore.case = TRUE,
    perl = TRUE
  )
]


cat(
  "ISEI assignment rules extracted:",
  length(isei_rule_lines),
  "\n\n"
)


# Historical v4 contains exactly 257 ISEI IF assignments.
# This also functions as a parser/syntax fingerprint.

stopifnot(
  length(isei_rule_lines) == 257
)


# ------------------------------------------------------------
# 4. Translate an SPSS IF condition into R syntax
# ------------------------------------------------------------

translate_spss_condition <- function(condition_text) {
  
  x <- trimws(
    condition_text
  )
  
  
  # ----------------------------------------------------------
  # 4A. Missing-value functions
  # ----------------------------------------------------------
  #
  # Historical v4 only uses MISSING(variable).
  # Nested expressions such as MISSING(f(x)) are not supported
  # by this deliberately narrow translator.
  
  x <- gsub(
    "NOT\\s+MISSING\\s*\\(([^\\)]+)\\)",
    "!is.na(\\1)",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "MISSING\\s*\\(([^\\)]+)\\)",
    "is.na(\\1)",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 4B. Logical operators
  # ----------------------------------------------------------
  
  x <- gsub(
    "\\bAND\\b",
    "&",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "\\bOR\\b",
    "|",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 4C. Comparison keywords
  # ----------------------------------------------------------
  
  x <- gsub(
    "\\bGE\\b",
    ">=",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "\\bLE\\b",
    "<=",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "\\bGT\\b",
    ">",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "\\bLT\\b",
    "<",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "\\bNE\\b",
    "!=",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "\\bEQ\\b",
    "==",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  
  # Convert remaining SPSS "=" comparisons into R "==".
  #
  # Do NOT modify:
  # >=
  # <=
  # !=
  # ==
  
  x <- gsub(
    "(?<![><!=])=(?!=)",
    "==",
    x,
    perl = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 4D. Standardise variable names
  # ----------------------------------------------------------
  
  x <- gsub(
    "\\bISCO_MAIN\\b",
    "isco_main",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  x <- gsub(
    "\\bISEI\\b",
    "isei",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  
  # Collapse excess whitespace.
  
  x <- gsub(
    "\\s+",
    " ",
    x,
    perl = TRUE
  )
  
  
  # NOTE:
  # The resulting expressions rely on R operator precedence:
  # comparison operators are evaluated before & and |.
  #
  # This translator is specific to R and should not be copied
  # mechanically into another programming language.
  
  trimws(x)
}


# ------------------------------------------------------------
# 5. Parse one historical ISEI assignment line
# ------------------------------------------------------------

parse_isei_rule <- function(line) {
  
  # Capture:
  #
  # group 1 = IF condition
  # group 2 = assigned value or $SYSMIS
  #
  # Lazy matching prevents MISSING(isei) inside the condition
  # from being mistaken for the assignment target.
  
  parser_pattern <- paste0(
    "^IF\\s*",
    "(.*?)",
    "\\s+isei\\s*=\\s*",
    "(\\$SYSMIS|-?[0-9]+(?:\\.[0-9]+)?)",
    "\\s*\\.\\s*$"
  )
  
  
  match_object <- regexec(
    parser_pattern,
    line,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  
  pieces <- regmatches(
    line,
    match_object
  )[[1]]
  
  
  if (length(pieces) != 3) {
    
    stop(
      paste0(
        "Unable to parse historical ISEI rule:\n",
        line
      )
    )
  }
  
  
  assignment_text <- pieces[3]
  
  
  assignment_value <-
    if (
      toupper(assignment_text) == "$SYSMIS"
    ) {
      
      NA_real_
      
    } else {
      
      as.numeric(
        assignment_text
      )
    }
  
  
  list(
    
    original_line = line,
    
    condition_spss = trimws(
      pieces[2]
    ),
    
    condition_r = translate_spss_condition(
      pieces[2]
    ),
    
    value = assignment_value
  )
}


isei_rules <- lapply(
  isei_rule_lines,
  parse_isei_rule
)


stopifnot(
  length(isei_rules) == 257
)


# ------------------------------------------------------------
# 6. Build historical-rule audit table
# ------------------------------------------------------------

rule_audit <- data.frame(
  
  rule = seq_along(
    isei_rules
  ),
  
  spss_condition = vapply(
    isei_rules,
    function(x) x$condition_spss,
    character(1)
  ),
  
  r_condition = vapply(
    isei_rules,
    function(x) x$condition_r,
    character(1)
  ),
  
  assigned_isei = vapply(
    isei_rules,
    function(x) x$value,
    numeric(1)
  ),
  
  spss_line = vapply(
    isei_rules,
    function(x) x$original_line,
    character(1)
  ),
  
  stringsAsFactors = FALSE
)


stopifnot(
  nrow(rule_audit) == 257
)


cat(
  "Historical mapping rule table built:",
  nrow(rule_audit),
  "rules\n\n"
)


# ------------------------------------------------------------
# 7. Initialise historical ISEI
# ------------------------------------------------------------

stage3_dat$isei <- rep(
  NA_real_,
  nrow(stage3_dat)
)


# ------------------------------------------------------------
# 8. Execute mapping sequentially
# ------------------------------------------------------------

# Sequential execution is essential.
#
# Historical v4 contains:
#   - exact assignments,
#   - broad-range assignments,
#   - missing-protected fallback assignments.
#
# Therefore the original rule order must be preserved.
# A one-shot case_when() would not faithfully reproduce v4.

mapping_env <- new.env(
  parent = baseenv()
)


mapping_env$isco_main <-
  stage3_dat$isco_main

mapping_env$isei <-
  stage3_dat$isei


for (i in seq_along(isei_rules)) {
  
  rule <- isei_rules[[i]]
  
  
  condition_expression <- tryCatch(
    
    parse(
      text = rule$condition_r
    ),
    
    error = function(e) {
      
      stop(
        paste0(
          "\nUnable to parse translated R condition.\n",
          "Rule number: ",
          i,
          "\n",
          "Original SPSS:\n",
          rule$original_line,
          "\n",
          "Translated R:\n",
          rule$condition_r,
          "\n",
          "R error:\n",
          conditionMessage(e)
        )
      )
    }
  )
  
  
  idx <- tryCatch(
    
    eval(
      condition_expression,
      envir = mapping_env
    ),
    
    error = function(e) {
      
      stop(
        paste0(
          "\nUnable to evaluate historical ISEI rule.\n",
          "Rule number: ",
          i,
          "\n",
          "Original SPSS:\n",
          rule$original_line,
          "\n",
          "Translated R:\n",
          rule$condition_r,
          "\n",
          "R error:\n",
          conditionMessage(e)
        )
      )
    }
  )
  
  
  # SPSS IF performs no assignment where its condition is
  # system-missing.
  #
  # In R, logical NA positions therefore become FALSE.
  
  idx[is.na(idx)] <- FALSE
  
  
  if (!is.logical(idx)) {
    
    stop(
      paste0(
        "Rule ",
        i,
        " did not evaluate to a logical vector.\n",
        rule$original_line
      )
    )
  }
  
  
  if (length(idx) != nrow(stage3_dat)) {
    
    stop(
      paste0(
        "Rule ",
        i,
        " returned ",
        length(idx),
        " values instead of ",
        nrow(stage3_dat),
        ".\n",
        rule$original_line
      )
    )
  }
  
  
  mapping_env$isei[idx] <-
    rule$value
}


stage3_dat$isei <-
  mapping_env$isei


# ------------------------------------------------------------
# 9. Historical mapping coverage
# ------------------------------------------------------------

n_isei_mapped_stage3 <- sum(
  !is.na(stage3_dat$isei)
)


n_isei_missing_stage3 <- sum(
  is.na(stage3_dat$isei)
)


cat(
  "ISEI mapped in Stage 3:",
  n_isei_mapped_stage3,
  "\n"
)

cat(
  "ISEI missing in Stage 3:",
  n_isei_missing_stage3,
  "\n\n"
)


stopifnot(
  n_isei_mapped_stage3 == 1670
)

stopifnot(
  n_isei_missing_stage3 == 4
)


# ------------------------------------------------------------
# 10. Audit historical unmapped occupation codes
# ------------------------------------------------------------

historical_unmapped <- stage3_dat[
  is.na(stage3_dat$isei),
  c(
    "isco_main",
    "rural_origin",
    "par_edu_yrs",
    "edu_lvl",
    "cohort"
  )
]


cat(
  "Historical unmapped cases:\n"
)


print(
  historical_unmapped,
  row.names = FALSE
)


unmapped_codes <- sort(
  unique(
    historical_unmapped$isco_main
  )
)


cat(
  "\nUnique historical unmapped ISCO codes:",
  paste(
    unmapped_codes,
    collapse = ", "
  ),
  "\n\n"
)


# Historical v4 benchmark:
# all four unmapped cases are ISCO = 1000.

stopifnot(
  nrow(historical_unmapped) == 4
)

stopifnot(
  length(unmapped_codes) == 1
)

stopifnot(
  unmapped_codes[1] == 1000
)

stopifnot(
  sum(
    stage3_dat$isco_main == 1000,
    na.rm = TRUE
  ) == 4
)

stopifnot(
  all(
    is.na(
      stage3_dat$isei[
        stage3_dat$isco_main == 1000
      ]
    )
  )
)


# ------------------------------------------------------------
# 11. Boundary validation:
#     ISCO 100 and ISCO 310
# ------------------------------------------------------------

n_isco100 <- sum(
  stage3_dat$isco_main == 100,
  na.rm = TRUE
)


n_isco310 <- sum(
  stage3_dat$isco_main == 310,
  na.rm = TRUE
)


cat(
  "ISCO 100 cases:",
  n_isco100,
  "\n"
)

cat(
  "ISCO 310 cases:",
  n_isco310,
  "\n"
)


stopifnot(
  n_isco100 > 0
)

stopifnot(
  n_isco310 > 0
)


isco100_scores <- unique(
  stage3_dat$isei[
    stage3_dat$isco_main == 100
  ]
)


isco310_scores <- unique(
  stage3_dat$isei[
    stage3_dat$isco_main == 310
  ]
)


cat(
  "ISEI for ISCO 100:",
  paste(
    isco100_scores,
    collapse = ", "
  ),
  "\n"
)

cat(
  "ISEI for ISCO 310:",
  paste(
    isco310_scores,
    collapse = ", "
  ),
  "\n\n"
)


stopifnot(
  length(isco100_scores) == 1
)

stopifnot(
  length(isco310_scores) == 1
)

stopifnot(
  isco100_scores[1] == 45
)

stopifnot(
  isco310_scores[1] == 45
)


# ------------------------------------------------------------
# 12. Verify final exclusion-source overlap
# ------------------------------------------------------------

origin_missing_flag <-
  is.na(
    stage3_dat$rural_origin
  )


parent_missing_flag <-
  is.na(
    stage3_dat$par_edu_yrs
  )


isei_missing_flag <-
  is.na(
    stage3_dat$isei
  )


origin_parent_overlap <- sum(
  origin_missing_flag &
    parent_missing_flag
)


origin_isei_overlap <- sum(
  origin_missing_flag &
    isei_missing_flag
)


parent_isei_overlap <- sum(
  parent_missing_flag &
    isei_missing_flag
)


triple_overlap <- sum(
  origin_missing_flag &
    parent_missing_flag &
    isei_missing_flag
)


cat(
  "Origin-parent overlap:",
  origin_parent_overlap,
  "\n"
)

cat(
  "Origin-ISEI overlap:",
  origin_isei_overlap,
  "\n"
)

cat(
  "Parent-ISEI overlap:",
  parent_isei_overlap,
  "\n"
)

cat(
  "Triple overlap:",
  triple_overlap,
  "\n\n"
)


stopifnot(
  origin_parent_overlap == 0
)

stopifnot(
  origin_isei_overlap == 0
)

stopifnot(
  parent_isei_overlap == 0
)

stopifnot(
  triple_overlap == 0
)


# ------------------------------------------------------------
# 13. Reconstruct final historical analytic sample
# ------------------------------------------------------------

stage3_dat$final_original_flag <-
  !origin_missing_flag &
  !parent_missing_flag &
  !isei_missing_flag


n_final_original <- sum(
  stage3_dat$final_original_flag
)


n_excluded_final_step <- sum(
  !stage3_dat$final_original_flag
)


cat(
  "Excluded at final historical step:",
  n_excluded_final_step,
  "\n"
)

cat(
  "Final historical analytic N:",
  n_final_original,
  "\n\n"
)


stopifnot(
  sum(origin_missing_flag) == 5
)

stopifnot(
  sum(parent_missing_flag) == 19
)

stopifnot(
  sum(isei_missing_flag) == 4
)

stopifnot(
  5 + 19 + 4 == 28
)

stopifnot(
  n_excluded_final_step == 28
)

stopifnot(
  1674 - 28 == 1646
)

stopifnot(
  n_final_original == 1646
)


# ------------------------------------------------------------
# 14. Create final original-analysis dataset
# ------------------------------------------------------------

analysis_original <- stage3_dat[
  stage3_dat$final_original_flag,
]


stopifnot(
  nrow(analysis_original) == 1646
)


# Model 7's alternative-origin measure should have no missing
# values within the historical final sample.

stopifnot(
  sum(
    is.na(
      analysis_original$rural_origin_f
    )
  ) == 0
)


# ------------------------------------------------------------
# 15. Validate historical ISEI distribution
# ------------------------------------------------------------

isei_n <- sum(
  !is.na(
    analysis_original$isei
  )
)


isei_mean <- mean(
  analysis_original$isei
)


isei_sd <- sd(
  analysis_original$isei
)


isei_min <- min(
  analysis_original$isei
)


isei_max <- max(
  analysis_original$isei
)


cat(
  "Historical final ISEI N:",
  isei_n,
  "\n"
)

cat(
  "Historical final ISEI mean:",
  format(
    isei_mean,
    digits = 12
  ),
  "\n"
)

cat(
  "Historical final ISEI SD:",
  format(
    isei_sd,
    digits = 12
  ),
  "\n"
)

cat(
  "Historical final ISEI minimum:",
  isei_min,
  "\n"
)

cat(
  "Historical final ISEI maximum:",
  isei_max,
  "\n\n"
)


stopifnot(
  isei_n == 1646
)


near_display(
  isei_mean,
  51.9842,
  4
)


near_display(
  isei_sd,
  16.34346,
  5
)


stopifnot(
  isei_min == 16
)

stopifnot(
  isei_max == 88
)


# ------------------------------------------------------------
# 16. Validate final education distribution
# ------------------------------------------------------------

edu_counts <- table(
  analysis_original$edu_lvl
)


cat(
  "Final education counts:\n"
)

print(
  edu_counts
)

cat("\n")


stopifnot(
  unname(
    edu_counts["1"]
  ) == 707
)

stopifnot(
  unname(
    edu_counts["2"]
  ) == 806
)

stopifnot(
  unname(
    edu_counts["3"]
  ) == 133
)


# ------------------------------------------------------------
# 17. Validate final origin distribution
# ------------------------------------------------------------

origin_counts <- table(
  analysis_original$rural_origin
)


cat(
  "Final origin counts:\n"
)

print(
  origin_counts
)

cat("\n")


# rural_origin:
# 0 = urban
# 1 = rural

stopifnot(
  unname(
    origin_counts["0"]
  ) == 928
)

stopifnot(
  unname(
    origin_counts["1"]
  ) == 718
)


# ------------------------------------------------------------
# 18. Validate final cohort distribution
# ------------------------------------------------------------

cohort_counts <- table(
  analysis_original$cohort
)


cat(
  "Final cohort counts:\n"
)

print(
  cohort_counts
)

cat("\n")


stopifnot(
  unname(
    cohort_counts["1"]
  ) == 170
)

stopifnot(
  unname(
    cohort_counts["2"]
  ) == 334
)

stopifnot(
  unname(
    cohort_counts["3"]
  ) == 514
)

stopifnot(
  unname(
    cohort_counts["4"]
  ) == 628
)


# ------------------------------------------------------------
# 19. Validate final gender distribution
# ------------------------------------------------------------

gender_counts <- table(
  analysis_original$female
)


cat(
  "Final gender counts:\n"
)

print(
  gender_counts
)

cat("\n")


# female:
# 0 = male
# 1 = female

stopifnot(
  unname(
    gender_counts["0"]
  ) == 837
)

stopifnot(
  unname(
    gender_counts["1"]
  ) == 809
)


# ------------------------------------------------------------
# 20. Additional final-sample fingerprints
# ------------------------------------------------------------

age_mean <- mean(
  analysis_original$age
)


age_sd <- sd(
  analysis_original$age
)


parent_edu_mean <- mean(
  analysis_original$par_edu_yrs
)


parent_edu_sd <- sd(
  analysis_original$par_edu_yrs
)


cat(
  "Final age mean:",
  sprintf(
    "%.4f",
    age_mean
  ),
  "\n"
)

cat(
  "Final age SD:",
  sprintf(
    "%.5f",
    age_sd
  ),
  "\n"
)

cat(
  "Final parental-education mean:",
  sprintf(
    "%.4f",
    parent_edu_mean
  ),
  "\n"
)

cat(
  "Final parental-education SD:",
  sprintf(
    "%.5f",
    parent_edu_sd
  ),
  "\n\n"
)


near_display(
  age_mean,
  37.7947,
  4
)


near_display(
  age_sd,
  9.57887,
  5
)


near_display(
  parent_edu_mean,
  9.7631,
  4
)


near_display(
  parent_edu_sd,
  4.04440,
  5
)


# ------------------------------------------------------------
# 21. Strong joint validation:
#     education x rural-origin ISEI cells
# ------------------------------------------------------------

# These six cells jointly validate:
#
#   - final sample membership
#   - edu_lvl
#   - rural_origin
#   - historical ISEI construction
#
# against the final SPSS v4 output.

stopifnot(
  !anyNA(
    analysis_original[
      c(
        "isei",
        "edu_lvl",
        "rural_origin"
      )
    ]
  )
)


cell_audit <- data.frame(
  
  edu_lvl = c(
    1,
    1,
    2,
    2,
    3,
    3
  ),
  
  rural_origin = c(
    0,
    1,
    0,
    1,
    0,
    1
  ),
  
  expected_n = c(
    361,
    346,
    480,
    326,
    87,
    46
  ),
  
  expected_mean_isei = c(
    48.0028,
    48.0983,
    52.4937,
    55.2423,
    61.2874,
    66.4565
  )
)


cell_audit$n <- mapply(
  
  function(edu, rural) {
    
    sum(
      analysis_original$edu_lvl == edu &
        analysis_original$rural_origin == rural
    )
  },
  
  cell_audit$edu_lvl,
  cell_audit$rural_origin
)


cell_audit$mean_isei <- mapply(
  
  function(edu, rural) {
    
    idx <-
      analysis_original$edu_lvl == edu &
      analysis_original$rural_origin == rural
    
    mean(
      analysis_original$isei[idx]
    )
  },
  
  cell_audit$edu_lvl,
  cell_audit$rural_origin
)


cat(
  "Education x rural-origin cell audit:\n"
)

print(
  cell_audit,
  row.names = FALSE
)

cat("\n")


# Every final case must appear in exactly one of the six cells.

stopifnot(
  sum(
    cell_audit$n
  ) == 1646
)


# Cell Ns must match SPSS.

stopifnot(
  all(
    cell_audit$n ==
      cell_audit$expected_n
  )
)


# Cell means must match SPSS to four displayed decimals.

for (
  i in seq_len(
    nrow(cell_audit)
  )
) {
  
  near_display(
    cell_audit$mean_isei[i],
    cell_audit$expected_mean_isei[i],
    4
  )
}


# ------------------------------------------------------------
# 22. Build resolved historical ISCO -> ISEI map
# ------------------------------------------------------------

# This is different from rule_audit:
#
# rule_audit:
#   shows all 257 historical SPSS rules.
#
# resolved_map:
#   shows the FINAL score actually produced for each unique
#   ISCO code occurring in the Stage-3 CGSS sample.

resolved_map <- unique(
  data.frame(
    
    isco_main =
      stage3_dat$isco_main,
    
    isei_original =
      stage3_dat$isei
  )
)


resolved_map <- resolved_map[
  order(
    resolved_map$isco_main
  ),
]


# A single ISCO code must not resolve to two different final
# ISEI values.

stopifnot(
  !anyDuplicated(
    resolved_map$isco_main
  )
)


# Add Stage-3 frequency for each occupational code.

isco_stage3_counts <- table(
  stage3_dat$isco_main
)


resolved_map$n_stage3 <- as.integer(
  isco_stage3_counts[
    as.character(
      resolved_map$isco_main
    )
  ]
)


stopifnot(
  sum(
    resolved_map$n_stage3
  ) == 1674
)


# Explicitly check the historically unmapped code.

stopifnot(
  nrow(
    resolved_map[
      resolved_map$isco_main == 1000 &
        is.na(resolved_map$isei_original),
    ]
  ) == 1
)


# ------------------------------------------------------------
# 23. Save non-identifying audit outputs
# ------------------------------------------------------------

dir.create(
  "output/original_replication",
  recursive = TRUE,
  showWarnings = FALSE
)


# 257 rules extracted directly from v4 SPSS syntax.

write.csv(
  rule_audit,
  "output/original_replication/historical_isei_rules_extracted.csv",
  row.names = FALSE
)


# Resolved code-level historical map.

write.csv(
  resolved_map,
  "output/original_replication/historical_isei_resolved_map.csv",
  row.names = FALSE
)


# Historical unmapped code summary.

unmapped_code_summary <- data.frame(
  
  isco_main =
    unmapped_codes,
  
  n_stage3 =
    vapply(
      
      unmapped_codes,
      
      function(code) {
        
        sum(
          stage3_dat$isco_main == code,
          na.rm = TRUE
        )
      },
      
      numeric(1)
    )
)


write.csv(
  unmapped_code_summary,
  "output/original_replication/historical_unmapped_isco_codes.csv",
  row.names = FALSE
)


# Six-cell validation table.

write.csv(
  cell_audit,
  "output/original_replication/education_origin_isei_cell_audit.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 24. Final Stage-03 validation
# ------------------------------------------------------------

cat(
  "========================================\n"
)

cat(
  "03_original_isei.R: ALL CHECKS PASSED\n"
)

cat(
  "========================================\n"
)


cat(
  "Historical ISEI rules extracted: ",
  length(isei_rule_lines),
  "\n"
)


cat(
  "Stage-3 occupation sample:        ",
  nrow(stage3_dat),
  "\n"
)


cat(
  "ISEI mapped in Stage 3:           ",
  n_isei_mapped_stage3,
  "\n"
)


cat(
  "Missing origin:                   ",
  sum(origin_missing_flag),
  "\n"
)


cat(
  "Missing parental education:       ",
  sum(parent_missing_flag),
  "\n"
)


cat(
  "Historical ISEI unmapped:         ",
  sum(isei_missing_flag),
  "\n"
)


cat(
  "Final exclusions:                 ",
  n_excluded_final_step,
  "\n"
)


cat(
  "Final original analytic sample:   ",
  n_final_original,
  "\n"
)


cat(
  "ISEI mean:                        ",
  sprintf(
    "%.4f",
    isei_mean
  ),
  "\n"
)


cat(
  "ISEI SD:                          ",
  sprintf(
    "%.5f",
    isei_sd
  ),
  "\n"
)


cat(
  "ISEI range:                       ",
  paste0(
    isei_min,
    "-",
    isei_max
  ),
  "\n"
)


cat(
  "Historical syntax MD5:            ",
  v4_syntax_md5,
  "\n"
)


cat(
  "========================================\n"
)

cat(
  "Historical ISEI reproduction complete.\n"
)

cat(
  "Next stage: reproduce original tables and Models 1-7.\n"
)

cat(
  "========================================\n"
)