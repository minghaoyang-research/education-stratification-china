# Supplementary measurement sensitivity and model diagnostics

These analyses were completed after the dissertation reproduction. They compare the dissertation's historical occupational-status recode with two documented alternative ISCO-08-to-ISEI-08 mappings, rerun Models 1–7 under controlled sample definitions, and add model diagnostics. They are kept separate so the submitted analysis and the later methodological checks remain easy to distinguish.

Stage 05 compares the three occupational-status mappings. Stage 06 holds the original 1,646 respondents fixed in the common-support versions and also reports full-coverage versions (N = 1,650), which include four ISCO 1000 cases scored by the alternative mappings. In Model 3, the bachelor, postgraduate and rural-origin coefficients remain positive and statistically significant across mappings; the female coefficient is more sensitive to mapping choice. Stage 07 examines that sensitivity and additional model diagnostics.

To rerun the supplementary work from the repository root, run the scripts in order:

```r
source("supplementary/R/05_isei_sensitivity.R")
source("supplementary/R/06_model_sensitivity.R")
source("supplementary/R/07_model_diagnostics.R")
```

Nothing in this directory is required to reproduce the dissertation results.
