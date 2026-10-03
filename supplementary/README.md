# Supplementary analyses

This directory contains follow-up work completed after the dissertation reproduction.

The main reproduction is in `R/01_import_validate.R` through `R/04_original_models.R`. Those four scripts rebuild the submitted SPSS analysis and are sufficient for the project described in the root README.

The material here records later sensitivity checks and diagnostics, including alternative occupational-status mappings, model sensitivity checks and additional diagnostics. It is kept separate so the submitted analysis and the later exploratory work remain easy to distinguish.

To rerun the supplementary work from the repository root, run the scripts in order:

```r
source("supplementary/R/05_isei_sensitivity.R")
source("supplementary/R/06_model_sensitivity.R")
source("supplementary/R/07_model_diagnostics.R")
```

Nothing in this directory is required to reproduce the dissertation results.
