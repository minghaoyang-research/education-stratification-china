# 01 Import CGSS 2017 data
#
# archived diagnostic fields are removed before rebuilding the analysis

library(haven)

raw_file <- "data/raw/CGSS2017.sav"
archive_md5 <- "4933ebaa85a5d2e38c1a15aa5ae6c04e"

if (!file.exists(raw_file)) {
  stop("Cannot find ", raw_file)
}

if (!identical(unname(tools::md5sum(raw_file)), archive_md5)) {
  stop("CGSS2017.sav does not match the archived copy")
}

raw <- read_sav(raw_file, user_na = TRUE)

# Stages 02-03 rebuild the analytic sample from the survey items
diagnostic_vars <- c(
  "step1_pass", "step2_pass", "step3_pass", "age_2017",
  "has_occ", "occ_code", "isei_unmappable", "origin_miss",
  "father_miss", "mother_miss", "pared_miss", "analytic_sample"
)

dat <- raw[, !(names(raw) %in% diagnostic_vars)]