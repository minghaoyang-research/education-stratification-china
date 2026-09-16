# ============================================================
# 01_import_validate.R
# Original dissertation reproduction
#
# Purpose:
#   1. Import the archived CGSS 2017 SPSS file
#   2. Verify the exact source-file fingerprint
#   3. Verify dimensions as read by haven
#   4. Identify legacy diagnostic variables saved on 19 Apr 2026
#   5. Remove those legacy diagnostics from the analysis object
#
# IMPORTANT:
#   - The raw SAV file is never modified.
#   - analytic_sample == 1649 is a legacy diagnostic artefact,
#     NOT the dissertation's final analytic sample.
#   - The final dissertation sample (N = 1646) will be rebuilt
#     later from the original survey variables.
# ============================================================


# ------------------------------------------------------------
# 0. Package
# ------------------------------------------------------------

library(haven)


# ------------------------------------------------------------
# 1. Raw data path
# ------------------------------------------------------------

raw_file <- "data/raw/CGSS2017.sav"

# The raw data file must exist
stopifnot(file.exists(raw_file))


# ------------------------------------------------------------
# 2. Verify source-file fingerprint
# ------------------------------------------------------------

raw_size <- file.info(raw_file)$size
raw_md5  <- unname(tools::md5sum(raw_file))

cat("========================================\n")
cat("CGSS 2017 source-file validation\n")
cat("========================================\n\n")

cat("Raw file:", raw_file, "\n")
cat("File size:", raw_size, "bytes\n")
cat("MD5:", raw_md5, "\n\n")

# Exact archived source file used for this reproduction
expected_size <- 26231277
expected_md5  <- "4933ebaa85a5d2e38c1a15aa5ae6c04e"

stopifnot(raw_size == expected_size)
stopifnot(raw_md5 == expected_md5)

cat("File fingerprint: PASSED\n\n")


# ------------------------------------------------------------
# 3. Import SPSS SAV file
# ------------------------------------------------------------

# user_na = TRUE preserves SPSS user-defined missing metadata
# if any exists.
raw <- read_sav(
  raw_file,
  user_na = TRUE
)

cat("Rows read by haven:", nrow(raw), "\n")
cat("Variables read by haven:", ncol(raw), "\n\n")

# Empirically verified dimensions of this SAV when read by haven
stopifnot(nrow(raw) == 12582)
stopifnot(ncol(raw) == 798)

cat("SAV dimensions: PASSED\n\n")


# ------------------------------------------------------------
# 4. Check core source variables
# ------------------------------------------------------------

core_variables <- c(
  "a2",
  "a31",
  "a7a",
  "a27f",
  "a27h",
  "a89b",
  "a90b",
  "isco08_a59",
  "isco08_a60",
  "isco08_fa",
  "isco08_mo",
  "weight"
)

missing_core_variables <- setdiff(core_variables, names(raw))

if (length(missing_core_variables) > 0) {
  stop(
    "Missing expected core variables: ",
    paste(missing_core_variables, collapse = ", ")
  )
}

cat("Core source variables: PASSED\n\n")


# ------------------------------------------------------------
# 5. Identify legacy diagnostic variables
# ------------------------------------------------------------

legacy_diagnostic_vars <- c(
  "step1_pass",
  "step2_pass",
  "step3_pass",
  "age_2017",
  "has_occ",
  "occ_code",
  "isei_unmappable",
  "origin_miss",
  "father_miss",
  "mother_miss",
  "pared_miss",
  "analytic_sample"
)

missing_legacy_vars <- setdiff(
  legacy_diagnostic_vars,
  names(raw)
)

if (length(missing_legacy_vars) > 0) {
  stop(
    "Missing expected legacy diagnostic variables: ",
    paste(missing_legacy_vars, collapse = ", ")
  )
}

cat(
  "Legacy diagnostic variables found:",
  length(legacy_diagnostic_vars),
  "\n"
)

stopifnot(length(legacy_diagnostic_vars) == 12)

cat("Legacy-variable check: PASSED\n\n")


# ------------------------------------------------------------
# 6. Verify archived diagnostic fingerprints
# ------------------------------------------------------------

# analytic_sample is NOT the dissertation sample.
# It comes from a superseded post-analysis diagnostic run.
legacy_analytic_n <- sum(
  raw$analytic_sample == 1,
  na.rm = TRUE
)

cat(
  "Legacy analytic_sample == 1:",
  legacy_analytic_n,
  "\n"
)

stopifnot(legacy_analytic_n == 1649)


# step3_pass is a historical residual variable.
# In this archived SAV it is zero for all 12,582 cases.
step3_nonzero_n <- sum(
  raw$step3_pass != 0,
  na.rm = TRUE
)

cat(
  "Non-zero step3_pass cases:",
  step3_nonzero_n,
  "\n"
)

stopifnot(step3_nonzero_n == 0)

cat("Legacy diagnostic fingerprints: PASSED\n\n")


# ------------------------------------------------------------
# 7. Create clean analysis object
# ------------------------------------------------------------

# IMPORTANT:
# raw remains untouched.
# dat is the working object used by later scripts.
dat <- raw[
  ,
  !(names(raw) %in% legacy_diagnostic_vars)
]

cat("Raw dataset dimensions:\n")
cat(
  nrow(raw),
  "rows x",
  ncol(raw),
  "variables\n\n"
)

cat("Clean analysis object dimensions:\n")
cat(
  nrow(dat),
  "rows x",
  ncol(dat),
  "variables\n\n"
)

# 798 SAV variables minus 12 legacy diagnostics = 786
stopifnot(nrow(dat) == 12582)
stopifnot(ncol(dat) == 786)


# ------------------------------------------------------------
# 8. Final Stage 01 validation
# ------------------------------------------------------------

cat("========================================\n")
cat("01_import_validate.R: ALL CHECKS PASSED\n")
cat("========================================\n")
cat("Source cases:                 ", nrow(raw), "\n")
cat("Source variables (haven):     ", ncol(raw), "\n")
cat("Legacy diagnostics removed:   ", length(legacy_diagnostic_vars), "\n")
cat("Clean analysis variables:     ", ncol(dat), "\n")
cat("Legacy analytic_sample count: ", legacy_analytic_n, "\n")
cat("MD5:                          ", raw_md5, "\n")
cat("========================================\n")