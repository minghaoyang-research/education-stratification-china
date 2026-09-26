# 02 Construct variables and first three sample restrictions
# same coding as the dissertation SPSS file
# ISEI comes in 03

source("R/01_import_validate.R")

stopifnot(nrow(dat) == 12582, ncol(dat) == 786)

# numeric copies so the original codes stay visible
dat$a2_num       <- as.numeric(dat$a2)
dat$a31_num      <- as.numeric(dat$a31)
dat$a7a_num      <- as.numeric(dat$a7a)
dat$a27f_num     <- as.numeric(dat$a27f)
dat$a27h_num     <- as.numeric(dat$a27h)
dat$a89b_num     <- as.numeric(dat$a89b)
dat$a90b_num     <- as.numeric(dat$a90b)
dat$isco_a59_num <- as.numeric(dat$isco08_a59)
dat$isco_a60_num <- as.numeric(dat$isco08_a60)


# Sample restrictions and occupation --------------------------------------

dat$age <- 2017 - dat$a31_num

dat$sample_stage1 <- dat$a7a_num %in% 9:13
n_stage1 <- sum(dat$sample_stage1)
stopifnot(n_stage1 == 2470)

dat$sample_stage2 <-
  dat$sample_stage1 &
  !is.na(dat$age) &
  dat$age >= 25 &
  dat$age <= 60

n_stage2 <- sum(dat$sample_stage2)
stopifnot(n_stage2 == 1766)

# A59 current occupation, A60 last non-farm job if A59 is not usable
valid_a59 <-
  !is.na(dat$isco_a59_num) &
  dat$isco_a59_num > 0 &
  dat$isco_a59_num < 10000

valid_a60 <-
  !is.na(dat$isco_a60_num) &
  dat$isco_a60_num > 0 &
  dat$isco_a60_num < 10000

dat$isco_main <- rep(NA_real_, nrow(dat))
dat$isco_main[valid_a59] <- dat$isco_a59_num[valid_a59]

use_a60 <- !valid_a59 & valid_a60
dat$isco_main[use_a60] <- dat$isco_a60_num[use_a60]

dat$sample_stage3 <-
  dat$sample_stage2 &
  !is.na(dat$isco_main)

n_stage3 <- sum(dat$sample_stage3)
stopifnot(n_stage3 == 1674)


# Education, sex and cohort -----------------------------------------------

# A7A: 9-10 junior college, 11-12 bachelor, 13 postgraduate
dat$edu_lvl <- rep(NA_real_, nrow(dat))
dat$edu_lvl[dat$a7a_num %in% c(9, 10)] <- 1
dat$edu_lvl[dat$a7a_num %in% c(11, 12)] <- 2
dat$edu_lvl[dat$a7a_num == 13] <- 3

# junior college is the reference group
dat$edu_bachelor <- as.numeric(dat$edu_lvl == 2)
dat$edu_postgrad <- as.numeric(dat$edu_lvl == 3)

# A2: 1 male, 2 female
dat$female <- rep(NA_real_, nrow(dat))
dat$female[dat$a2_num == 1] <- 0
dat$female[dat$a2_num == 2] <- 1

# dissertation cohorts (1957-64 is the reference)
dat$cohort <- rep(NA_real_, nrow(dat))
dat$cohort[dat$a31_num %in% 1957:1964] <- 1
dat$cohort[dat$a31_num %in% 1965:1974] <- 2
dat$cohort[dat$a31_num %in% 1975:1984] <- 3
dat$cohort[dat$a31_num %in% 1985:1992] <- 4

dat$cohort2 <- as.numeric(dat$cohort == 2)
dat$cohort3 <- as.numeric(dat$cohort == 3)
dat$cohort4 <- as.numeric(dat$cohort == 4)


# Origin ------------------------------------------------------------------

# A27H age-14 hukou: 1-2 rural, 3-5 urban
dat$rural_origin <- rep(NA_real_, nrow(dat))
dat$rural_origin[dat$a27h_num %in% c(1, 2)] <- 1
dat$rural_origin[dat$a27h_num %in% c(3, 4, 5)] <- 0

# A27F age-14 residence, same split for Model 7
dat$rural_origin_f <- rep(NA_real_, nrow(dat))
dat$rural_origin_f[dat$a27f_num %in% c(1, 2)] <- 1
dat$rural_origin_f[dat$a27f_num %in% c(3, 4, 5)] <- 0


# Parental education -------------------------------------------------------

recode_parent_education <- function(x) {
  out <- rep(NA_real_, length(x))

  out[x == 1] <- 0                   # none
  out[x == 2] <- 2                   # sishu
  out[x == 3] <- 6                   # primary
  out[x == 4] <- 9                   # junior secondary
  out[x %in% c(5, 6, 7, 8)] <- 12   # senior / vocational / technical
  out[x %in% c(9, 10)] <- 15         # junior college
  out[x %in% c(11, 12)] <- 16        # bachelor
  out[x == 13] <- 19                 # postgraduate
  out[x == 14] <- 9                  # "other", kept as in dissertation

  out
}

dat$fa_edu_yrs <- recode_parent_education(dat$a89b_num)
dat$mo_edu_yrs <- recode_parent_education(dat$a90b_num)

# highest parent education, same rule as SPSS MAX
dat$par_edu_yrs <- pmax(
  dat$fa_edu_yrs,
  dat$mo_edu_yrs,
  na.rm = TRUE
)


# Stage-3 data and checks --------------------------------------------------

stage3_dat <- dat[dat$sample_stage3, ]

n_origin_6_7 <- sum(stage3_dat$a27h_num %in% c(6, 7))
n_origin_98_99 <- sum(stage3_dat$a27h_num %in% c(98, 99))
n_origin_missing <- sum(is.na(stage3_dat$rural_origin))

stopifnot(
  n_origin_6_7 == 3,
  n_origin_98_99 == 2,
  n_origin_missing == 5
)

n_parent_missing <- sum(is.na(stage3_dat$par_edu_yrs))
stopifnot(n_parent_missing == 19)

stage3_core_flag <-
  !is.na(stage3_dat$rural_origin) &
  !is.na(stage3_dat$par_edu_yrs)

n_stage3_core <- sum(stage3_core_flag)
stopifnot(n_stage3_core == 1650)

core_dat <- stage3_dat[stage3_core_flag, ]

n_rural_origin_f_missing <- sum(is.na(stage3_dat$rural_origin_f))
n_rural_origin_f_missing_core <- sum(is.na(core_dat$rural_origin_f))

stopifnot(
  n_rural_origin_f_missing == 3,
  n_rural_origin_f_missing_core == 0
)

stopifnot(
  !anyNA(stage3_dat$cohort),
  !anyNA(stage3_dat$female)
)
