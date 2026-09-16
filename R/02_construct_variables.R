# ============================================================
# 02_construct_variables.R
# Original dissertation reproduction
#
# Purpose:
#   Reconstruct all NON-ISEI variables used in the submitted
#   dissertation and reproduce the first three sequential
#   sample restrictions.
#
# Stage 02 MUST stop at N = 1,674.
#
# IMPORTANT:
#   - This script does NOT construct ISEI.
#   - This script does NOT construct the final N = 1,646 sample.
#   - Original dissertation coding is reproduced faithfully.
#   - Parental-education category 14 ("Other") is intentionally
#     retained as 9 years because this was the historical v4
#     coding used in the submitted dissertation.
#
#   N = 1,650 is an INTERNAL reproduction checkpoint only.
#   It was not a separate step in the submitted dissertation's
#   published sample-flow table.
#
#   Stage 03 will apply the historical ISEI mapping to ALL
#   N = 1,674 Stage-3 cases, not only the N = 1,650 core.
# ============================================================


# ------------------------------------------------------------
# 0. Start from the validated source data
# ------------------------------------------------------------

source("R/01_import_validate.R")

# 01_import_validate.R creates:
#
# raw = untouched archived SAV object
# dat = working dataset after removing 12 legacy diagnostic vars
#
# Expected clean dimensions:
# 12,582 cases x 786 source variables

stopifnot(nrow(dat) == 12582)
stopifnot(ncol(dat) == 786)


# ------------------------------------------------------------
# 1. Create plain numeric copies of required source variables
# ------------------------------------------------------------

# haven imports SPSS variables as labelled vectors.
# Plain numeric copies make the historical recodes explicit.

dat$a2_num       <- as.numeric(dat$a2)
dat$a31_num      <- as.numeric(dat$a31)
dat$a7a_num      <- as.numeric(dat$a7a)
dat$a27f_num     <- as.numeric(dat$a27f)
dat$a27h_num     <- as.numeric(dat$a27h)
dat$a89b_num     <- as.numeric(dat$a89b)
dat$a90b_num     <- as.numeric(dat$a90b)
dat$isco_a59_num <- as.numeric(dat$isco08_a59)
dat$isco_a60_num <- as.numeric(dat$isco08_a60)


# Birth year must be integer-valued.
# This explicitly verifies the assumption used in cohort coding.

stopifnot(
  all(
    dat$a31_num == round(dat$a31_num),
    na.rm = TRUE
  )
)


# ------------------------------------------------------------
# 2. Age
# ------------------------------------------------------------

# Historical dissertation definition:
# age = 2017 - birth year (A31)

dat$age <- 2017 - dat$a31_num


# ------------------------------------------------------------
# 3. Stage 1: Higher-education sample
# ------------------------------------------------------------

# A7A = 9 through 13:
# junior college or above.

dat$sample_stage1 <- dat$a7a_num %in% 9:13

n_stage1 <- sum(dat$sample_stage1)

cat(
  "\nStage 1 - Higher education (A7A 9-13):",
  n_stage1,
  "\n"
)

stopifnot(n_stage1 == 2470)


# ------------------------------------------------------------
# 4. Stage 2: Age 25-60
# ------------------------------------------------------------

dat$sample_stage2 <-
  dat$sample_stage1 &
  !is.na(dat$age) &
  dat$age >= 25 &
  dat$age <= 60

n_stage2 <- sum(dat$sample_stage2)

cat(
  "Stage 2 - Age 25-60:",
  n_stage2,
  "\n"
)

stopifnot(n_stage2 == 1766)


# Equivalent birth-year range.

stopifnot(
  all(
    dat$a31_num[dat$sample_stage2] >= 1957 &
      dat$a31_num[dat$sample_stage2] <= 1992
  )
)


# ------------------------------------------------------------
# 5. Current / recent non-farm occupation
# ------------------------------------------------------------

# Historical v4 logic:
#
# 1. Use current non-farm occupation (isco08_a59) if valid.
# 2. Otherwise use most recent non-farm occupation (isco08_a60).
# 3. A valid occupational code is:
#
#       > 0 and < 10000
#
# IMPORTANT:
# There is deliberately NO lower bound of 1000.
# Codes such as 100 and 310 therefore remain valid.

valid_a59 <-
  !is.na(dat$isco_a59_num) &
  dat$isco_a59_num > 0 &
  dat$isco_a59_num < 10000

valid_a60 <-
  !is.na(dat$isco_a60_num) &
  dat$isco_a60_num > 0 &
  dat$isco_a60_num < 10000


# Initialise final occupational-code variable.

dat$isco_main <- rep(
  NA_real_,
  nrow(dat)
)


# A59 has priority.

dat$isco_main[valid_a59] <-
  dat$isco_a59_num[valid_a59]


# Use A60 only if A59 was not valid.

use_a60 <- !valid_a59 & valid_a60

dat$isco_main[use_a60] <-
  dat$isco_a60_num[use_a60]


# ------------------------------------------------------------
# 6. Stage 3: Valid current/recent occupation
# ------------------------------------------------------------

dat$sample_stage3 <-
  dat$sample_stage2 &
  !is.na(dat$isco_main)

n_stage3 <- sum(dat$sample_stage3)

cat(
  "Stage 3 - Valid current/recent occupation:",
  n_stage3,
  "\n\n"
)

stopifnot(n_stage3 == 1674)


# ------------------------------------------------------------
# 7. Education level
# ------------------------------------------------------------

# Historical coding:
#
# 1 = Junior college (A7A 9-10)
# 2 = Bachelor       (A7A 11-12)
# 3 = Postgraduate   (A7A 13)

dat$edu_lvl <- rep(
  NA_real_,
  nrow(dat)
)

dat$edu_lvl[
  dat$a7a_num %in% c(9, 10)
] <- 1

dat$edu_lvl[
  dat$a7a_num %in% c(11, 12)
] <- 2

dat$edu_lvl[
  dat$a7a_num == 13
] <- 3


# Education dummies used in regression models.

dat$edu_bachelor <- ifelse(
  is.na(dat$edu_lvl),
  NA_real_,
  as.numeric(dat$edu_lvl == 2)
)

dat$edu_postgrad <- ifelse(
  is.na(dat$edu_lvl),
  NA_real_,
  as.numeric(dat$edu_lvl == 3)
)


# ------------------------------------------------------------
# 8. Gender
# ------------------------------------------------------------

# CGSS A2:
# 1 = male
# 2 = female
#
# Historical regression coding:
# male   = 0
# female = 1

dat$female <- rep(
  NA_real_,
  nrow(dat)
)

dat$female[
  dat$a2_num == 1
] <- 0

dat$female[
  dat$a2_num == 2
] <- 1


# ------------------------------------------------------------
# 9. Birth cohort
# ------------------------------------------------------------

# Historical cohort definitions:
#
# 1 = 1957-1964 (reference)
# 2 = 1965-1974
# 3 = 1975-1984
# 4 = 1985-1992
#
# The integer-valued birth-year assumption was explicitly
# verified above before using %in%.

dat$cohort <- rep(
  NA_real_,
  nrow(dat)
)

dat$cohort[
  dat$a31_num %in% 1957:1964
] <- 1

dat$cohort[
  dat$a31_num %in% 1965:1974
] <- 2

dat$cohort[
  dat$a31_num %in% 1975:1984
] <- 3

dat$cohort[
  dat$a31_num %in% 1985:1992
] <- 4


# Cohort dummy variables used in Models 5-6.

dat$cohort2 <- ifelse(
  is.na(dat$cohort),
  NA_real_,
  as.numeric(dat$cohort == 2)
)

dat$cohort3 <- ifelse(
  is.na(dat$cohort),
  NA_real_,
  as.numeric(dat$cohort == 3)
)

dat$cohort4 <- ifelse(
  is.na(dat$cohort),
  NA_real_,
  as.numeric(dat$cohort == 4)
)


# ------------------------------------------------------------
# 10. Rural origin: main measure (A27H)
# ------------------------------------------------------------

# Historical v4 coding:
#
# A27H 1-2 = rural origin = 1
# A27H 3-5 = urban origin = 0
# Everything else = missing.

dat$rural_origin <- rep(
  NA_real_,
  nrow(dat)
)

dat$rural_origin[
  dat$a27h_num %in% c(1, 2)
] <- 1

dat$rural_origin[
  dat$a27h_num %in% c(3, 4, 5)
] <- 0


# ------------------------------------------------------------
# 11. Rural origin: robustness measure (A27F)
# ------------------------------------------------------------

# Same historical binary coding used for Model 7.

dat$rural_origin_f <- rep(
  NA_real_,
  nrow(dat)
)

dat$rural_origin_f[
  dat$a27f_num %in% c(1, 2)
] <- 1

dat$rural_origin_f[
  dat$a27f_num %in% c(3, 4, 5)
] <- 0


# ------------------------------------------------------------
# 12. Parental education:
#     original dissertation coding
# ------------------------------------------------------------

# Approximate years of schooling used in the submitted analysis:
#
# 1      No formal education                    -> 0
# 2      Sishu / traditional private schooling -> 2
# 3      Primary                                -> 6
# 4      Junior secondary                       -> 9
# 5-8    Senior/vocational/technical secondary -> 12
# 9-10   Junior college                         -> 15
# 11-12  Bachelor                               -> 16
# 13     Postgraduate                           -> 19
# 14     Other                                  -> 9
#
# IMPORTANT:
# Category 14 -> 9 is a historical coding choice.
# It is retained ONLY for faithful reproduction.
# It will be reconsidered in the revised analysis.

recode_parent_education <- function(x) {
  
  out <- rep(
    NA_real_,
    length(x)
  )
  
  out[x == 1] <- 0
  out[x == 2] <- 2
  out[x == 3] <- 6
  out[x == 4] <- 9
  
  out[
    x %in% c(5, 6, 7, 8)
  ] <- 12
  
  out[
    x %in% c(9, 10)
  ] <- 15
  
  out[
    x %in% c(11, 12)
  ] <- 16
  
  out[
    x == 13
  ] <- 19
  
  # Historical v4 assignment.
  
  out[
    x == 14
  ] <- 9
  
  return(out)
}


dat$fa_edu_yrs <-
  recode_parent_education(
    dat$a89b_num
  )

dat$mo_edu_yrs <-
  recode_parent_education(
    dat$a90b_num
  )


# ------------------------------------------------------------
# 13. Highest parental education
# ------------------------------------------------------------

# Reproduce SPSS MAX behaviour explicitly:
#
# Father valid + Mother valid   -> maximum
# Father valid + Mother missing -> Father
# Father missing + Mother valid -> Mother
# Both missing                  -> missing

dat$par_edu_yrs <- rep(
  NA_real_,
  nrow(dat)
)


both_valid <-
  !is.na(dat$fa_edu_yrs) &
  !is.na(dat$mo_edu_yrs)


father_only <-
  !is.na(dat$fa_edu_yrs) &
  is.na(dat$mo_edu_yrs)


mother_only <-
  is.na(dat$fa_edu_yrs) &
  !is.na(dat$mo_edu_yrs)


dat$par_edu_yrs[both_valid] <-
  pmax(
    dat$fa_edu_yrs[both_valid],
    dat$mo_edu_yrs[both_valid]
  )


dat$par_edu_yrs[father_only] <-
  dat$fa_edu_yrs[father_only]


dat$par_edu_yrs[mother_only] <-
  dat$mo_edu_yrs[mother_only]


# Both missing remain NA.


# ------------------------------------------------------------
# 14. Create fully constructed Stage-3 object
# ------------------------------------------------------------

# This is the first and only construction of stage3_dat.

stage3_dat <- dat[
  dat$sample_stage3,
]

stopifnot(
  nrow(stage3_dat) == 1674
)


# ------------------------------------------------------------
# 15. Boundary validation:
#     A27H main origin measure
# ------------------------------------------------------------

n_origin_6_7 <- sum(
  stage3_dat$a27h_num %in% c(6, 7)
)

n_origin_98_99 <- sum(
  stage3_dat$a27h_num %in% c(98, 99)
)

n_origin_missing <- sum(
  is.na(stage3_dat$rural_origin)
)


cat(
  "A27H 6/7 cases in Stage 3:",
  n_origin_6_7,
  "\n"
)

cat(
  "A27H 98/99 cases in Stage 3:",
  n_origin_98_99,
  "\n"
)

cat(
  "Missing rural_origin in Stage 3:",
  n_origin_missing,
  "\n\n"
)


stopifnot(
  n_origin_6_7 == 3
)

stopifnot(
  n_origin_98_99 == 2
)

stopifnot(
  n_origin_missing == 5
)


stopifnot(
  all(
    is.na(
      stage3_dat$rural_origin[
        stage3_dat$a27h_num %in% c(6, 7)
      ]
    )
  )
)

stopifnot(
  all(
    is.na(
      stage3_dat$rural_origin[
        stage3_dat$a27h_num %in% c(98, 99)
      ]
    )
  )
)


# ------------------------------------------------------------
# 16. Boundary validation:
#     parental education
# ------------------------------------------------------------

n_parent_missing <- sum(
  is.na(stage3_dat$par_edu_yrs)
)

cat(
  "Both-parent education unavailable in Stage 3:",
  n_parent_missing,
  "\n\n"
)

stopifnot(
  n_parent_missing == 19
)


# Stronger validation:
# par_edu_yrs is missing if and only if BOTH parents'
# recoded education values are missing.

both_parent_missing_stage3 <-
  is.na(stage3_dat$fa_edu_yrs) &
  is.na(stage3_dat$mo_edu_yrs)


stopifnot(
  all(
    is.na(stage3_dat$par_edu_yrs) ==
      both_parent_missing_stage3
  )
)


# ------------------------------------------------------------
# 17. Pre-ISEI eligibility audit
# ------------------------------------------------------------

# The five missing-origin cases and nineteen missing-parental-
# education cases must not overlap.

n_origin_parent_overlap <- sum(
  is.na(stage3_dat$rural_origin) &
    is.na(stage3_dat$par_edu_yrs)
)

cat(
  "Origin/parent missing overlap in Stage 3:",
  n_origin_parent_overlap,
  "\n"
)

stopifnot(
  n_origin_parent_overlap == 0
)


# Internal reproduction checkpoint:
# cases valid on origin and parental education BEFORE ISEI.

stage3_core_flag <-
  !is.na(stage3_dat$rural_origin) &
  !is.na(stage3_dat$par_edu_yrs)


n_stage3_core <- sum(
  stage3_core_flag
)

cat(
  "Pre-ISEI core sample:",
  n_stage3_core,
  "\n\n"
)

stopifnot(
  n_stage3_core == 1650
)


# ------------------------------------------------------------
# 18. Alternative origin measure audit (A27F)
# ------------------------------------------------------------

n_rural_origin_f_missing <- sum(
  is.na(stage3_dat$rural_origin_f)
)

cat(
  "Missing rural_origin_f in Stage 3:",
  n_rural_origin_f_missing,
  "\n"
)


core_dat <- stage3_dat[
  stage3_core_flag,
]

stopifnot(
  nrow(core_dat) == 1650
)


n_rural_origin_f_missing_core <- sum(
  is.na(core_dat$rural_origin_f)
)

cat(
  "Missing rural_origin_f in pre-ISEI core:",
  n_rural_origin_f_missing_core,
  "\n\n"
)


stopifnot(
  n_rural_origin_f_missing == 3
)

stopifnot(
  n_rural_origin_f_missing_core == 0
)


# ------------------------------------------------------------
# 19. Parental-education category 14 audit
# ------------------------------------------------------------

# Print counts first.
# Earlier Python/Claude counts are only debugging references,
# not historical ground truth.

fa14_full <- sum(
  dat$a89b_num == 14,
  na.rm = TRUE
)

mo14_full <- sum(
  dat$a90b_num == 14,
  na.rm = TRUE
)

fa14_stage3 <- sum(
  stage3_dat$a89b_num == 14,
  na.rm = TRUE
)

mo14_stage3 <- sum(
  stage3_dat$a90b_num == 14,
  na.rm = TRUE
)


cat(
  "Father category 14, full sample:",
  fa14_full,
  "\n"
)

cat(
  "Mother category 14, full sample:",
  mo14_full,
  "\n"
)

cat(
  "Father category 14, Stage 3:",
  fa14_stage3,
  "\n"
)

cat(
  "Mother category 14, Stage 3:",
  mo14_stage3,
  "\n\n"
)


# Prevent vacuous category-14 mapping checks.

stopifnot(
  fa14_full > 0
)

stopifnot(
  mo14_full > 0
)


father14_index <-
  !is.na(dat$a89b_num) &
  dat$a89b_num == 14

mother14_index <-
  !is.na(dat$a90b_num) &
  dat$a90b_num == 14


stopifnot(
  all(
    dat$fa_edu_yrs[
      father14_index
    ] == 9
  )
)

stopifnot(
  all(
    dat$mo_edu_yrs[
      mother14_index
    ] == 9
  )
)


# ------------------------------------------------------------
# 20. Boundary validation:
#     occupation
# ------------------------------------------------------------

# Codes 100 and 310 satisfy the historical occupation-validity
# rule (> 0 and < 10000), so they must survive Stage 3.
#
# ISEI is NOT assigned here.
# The historical ISEI = 45 treatment belongs to Stage 03.

stage3_isco_values <- unique(
  stage3_dat$isco_main
)

stopifnot(
  100 %in% stage3_isco_values
)

stopifnot(
  310 %in% stage3_isco_values
)


stopifnot(
  all(
    stage3_dat$isco_main > 0 &
      stage3_dat$isco_main < 10000
  )
)


# ------------------------------------------------------------
# 21. Constructed-variable integrity checks
# ------------------------------------------------------------

stopifnot(
  all(
    stage3_dat$edu_lvl %in% c(1, 2, 3)
  )
)

stopifnot(
  all(
    stage3_dat$cohort %in% c(1, 2, 3, 4)
  )
)

stopifnot(
  all(
    stage3_dat$female %in% c(0, 1)
  )
)


# ------------------------------------------------------------
# 22. Sample-flow table
# ------------------------------------------------------------

sample_flow <- data.frame(
  
  Step = c(
    "Full CGSS 2017",
    "Higher education (A7A 9-13)",
    "Age 25-60",
    "Valid current/recent non-farm occupation",
    "Valid origin + parental education (pre-ISEI)"
  ),
  
  N = c(
    nrow(dat),
    n_stage1,
    n_stage2,
    n_stage3,
    n_stage3_core
  ),
  
  Excluded_from_previous_step = c(
    NA,
    nrow(dat) - n_stage1,
    n_stage1 - n_stage2,
    n_stage2 - n_stage3,
    n_stage3 - n_stage3_core
  )
)


cat(
  "========================================\n"
)

cat(
  "Original reproduction flow through Stage 02\n"
)

cat(
  "========================================\n"
)

print(
  sample_flow,
  row.names = FALSE
)


cat(
  "\nNOTE: N = 1,650 is an internal reproduction checkpoint.\n"
)

cat(
  "It was not a separate row in the submitted dissertation's sample-flow table.\n\n"
)


# ------------------------------------------------------------
# 23. Final Stage-02 validation
# ------------------------------------------------------------

stopifnot(
  nrow(dat) == 12582
)

stopifnot(
  n_stage1 == 2470
)

stopifnot(
  n_stage2 == 1766
)

stopifnot(
  n_stage3 == 1674
)

stopifnot(
  n_stage3_core == 1650
)


cat(
  "\n========================================\n"
)

cat(
  "02_construct_variables.R: ALL CHECKS PASSED\n"
)

cat(
  "========================================\n"
)


cat(
  "Full CGSS sample:                     ",
  nrow(dat),
  "\n"
)

cat(
  "Higher-education sample:              ",
  n_stage1,
  "\n"
)

cat(
  "Age 25-60 sample:                     ",
  n_stage2,
  "\n"
)

cat(
  "Valid occupation sample:              ",
  n_stage3,
  "\n"
)

cat(
  "Missing A27H origin at Stage 3:       ",
  n_origin_missing,
  "\n"
)

cat(
  "Missing parental education at Stage 3:",
  n_parent_missing,
  "\n"
)

cat(
  "Origin/parent missing overlap:        ",
  n_origin_parent_overlap,
  "\n"
)

cat(
  "Pre-ISEI core sample:                 ",
  n_stage3_core,
  "\n"
)

cat(
  "Missing A27F origin in Stage 3:       ",
  n_rural_origin_f_missing,
  "\n"
)

cat(
  "Missing A27F origin in pre-ISEI core: ",
  n_rural_origin_f_missing_core,
  "\n"
)

cat(
  "========================================\n"
)

cat(
  "Stage 02 stops here. ISEI is NOT constructed yet.\n"
)

cat(
  "Stage 03 will apply ISEI to all 1,674 Stage-3 cases.\n"
)

cat(
  "========================================\n"
)