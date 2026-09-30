# Education, Social Origin and Occupational Status in China

This repository rebuilds my undergraduate sociology dissertation in R and then audits it. The dissertation used the 2017 wave of the Chinese General Social Survey to examine whether urban-rural inequality among higher-education graduates shows up mainly in the qualifications people hold or in their occupational positions after education and family background are taken into account. Stages 01 to 04 reproduce the SPSS analysis I submitted at the University of Edinburgh. Stages 05 to 07 were added afterwards to check the ISEI coding, sample coverage and statistical inference.

The dissertation sample contains 1,646 graduates aged 25 to 60 with a valid current or recent non-farm occupation. Occupational status is measured with ISEI-08 and the submitted analysis uses a sequence of unweighted OLS models with classical standard errors.

Respondent-level CGSS records are not redistributed here. See [Getting the data](#getting-the-data) before running the scripts.

## Repository layout

```text
R/
  # reproduction of the submitted SPSS analysis
  01_import_validate.R
  02_construct_variables.R
  03_original_isei.R
  04_original_models.R

  # post-submission audit
  05_isei_crosswalk_audit.R
  06_isei_model_comparison.R
  07_model_diagnostics.R

archive/
  cgss2017_analysis_v4.sps
  isei_map_4.csv

data/raw/
  CGSS2017.sav                  # not in Git

output/
  original_replication/         # stages 03-04
  revised_measurement/          # stage 05
  isei_model_comparison/        # stage 06
  model_diagnostics/            # stage 07, with three figures under plots/

docs/
  replication_audit.md
  session_info.txt

education-stratification-china.Rproj
```

`cgss2017_analysis_v4.sps` is the final dissertation syntax. Stage 03 reads `archive/isei_map_4.csv`, a lookup derived from that syntax, rather than parsing the SPSS file on every run. Stages 05 to 07 do not feed revised choices back into the submitted reproduction.

## Getting the data

CGSS is run by the National Survey Research Center at Renmin University of China.

Place the archived SAV used for this project at `data/raw/CGSS2017.sav`. Stage 01 checks it against the fingerprint below.

```text
Cases       12,582
Variables   798
File size   26,231,277 bytes
MD5         4933ebaa85a5d2e38c1a15aa5ae6c04e
```

This fingerprint belongs to the exact SAV archived with the dissertation workflow. That file contains 12 variables left by a diagnostic run on 19 April 2026; Stage 01 removes them and works with the remaining 786 variables. The old `analytic_sample` flag is one of those discarded variables and is not used to define the dissertation sample.

A copy without those 12 diagnostic variables cannot match this MD5, so Stage 01 will stop on it.

## Running the analysis

The verified reproduction was run under R 4.4.3. The required packages are `haven`, `ISCO08ConveRsions`, `sandwich`, `lmtest` and `car`.

```r
install.packages(c("haven", "ISCO08ConveRsions", "sandwich", "lmtest", "car"))
```

The revised audit was run with `haven` 2.5.5, `ISCO08ConveRsions` 0.2.0, `sandwich` 3.1.1, `lmtest` 0.9.40 and `car` 3.1.5.

Open `education-stratification-china.Rproj` and put the SAV in place. Stage 04 rebuilds the submitted analysis on its own.

```r
source("R/04_original_models.R")
```

Stage 04 sources 03, which sources 02 and then 01.

Rebuilding the audit as well takes three calls in order.

```r
source("R/04_original_models.R")
source("R/05_isei_crosswalk_audit.R")
source("R/07_model_diagnostics.R")
```

Stage 05 sources Stage 03. Stage 06 also sources Stage 03, but it reads the Stage 04 reproduction outputs and the Stage 05 mapping and provenance files from disk, so 04 and 05 need to have been run first. Stage 07 sources Stage 06.

No respondent-level analytic dataset is written out by these scripts. The audit outputs are aggregate summaries, model results, code-level mapping tables and diagnostic plots.

## Models and tables

The seven model specifications are fixed throughout the repository. Model 1 contains education, sex and age. Model 2 adds parental education. Model 3 adds the main origin measure. Model 4 adds education-by-origin interactions. Model 5 replaces age with birth-cohort indicators. Model 6 adds education-by-cohort interactions. Model 7 reruns Model 3 with the alternative age-14 residence measure.

Table 6 is education by urban-rural origin. Table 7 reports mean ISEI by birth cohort and education level. Appendix Table A1 is education by birth cohort.

## Summary of results

Education remains strongly associated with occupational position. In Model 3, bachelor graduates are 6.205 ISEI points above junior-college graduates and postgraduates 15.899 points above, holding the other covariates constant. The rural-origin coefficient is positive at 3.025 points (SE = 0.833, p < .001), the opposite sign from the rural penalty expected at the start of the project. I do not read that coefficient as evidence that rural origin itself produces an occupational advantage. The sample contains only people who reached higher education, and rural-origin respondents may have passed a stronger selection process on the way in.

The education-by-origin interaction block is not statistically significant in the submitted model, while Table 6 shows clear sorting across education tiers. Rural-origin graduates are more concentrated in junior college and less concentrated in the higher tiers.

The cohort results are consistent with credential inflation, but the cross-sectional design cannot separate cohort change from ordinary career progression. Model 6 stays in the appendix as exploratory because the postgraduate-by-1957-64 cell contains only two respondents and produces severe multicollinearity.

## What the reproduction checks

Stages 01 to 04 are checked against the final SPSS output. Sample counts, residual degrees of freedom and table cell counts have to match exactly. Coefficients, classical standard errors, fit statistics and VIFs are checked at the precision printed by SPSS. Models 1 to 7, Tables 6, 7 and A1, and the classical interaction-block tests all reproduce the submitted analysis.

The estimate-by-estimate comparison also found two reporting discrepancies in the submitted dissertation. Both are recorded in [`docs/replication_audit.md`](docs/replication_audit.md).

## What the audit found

Stage 05 compares the dissertation ISEI coding with two mappings from `ISCO08ConveRsions`. `isco08toisei08()` follows the SPSS syntax modules and `isco08toisei08_2()` uses the overview table. I use the first as the main revised mapping and the second as a sensitivity check. Three-digit ISCO codes are zero-padded before the package functions are called.

Valid origin and parental education are available for 1,650 respondents. The historical mapping leaves four ISCO 1000 cases without an ISEI score, so the submitted N is 1,646. Both revised mappings score those four cases. They are all urban-origin bachelor graduates.

Stage 06 fits the same seven models under five fixed versions.

| Version | N | ISEI mapping |
|---|---:|---|
| `historical_common` | 1,646 | dissertation SPSS recode |
| `syntax_common` | 1,646 | `isco08toisei08()` |
| `overview_common` | 1,646 | `isco08toisei08_2()` |
| `syntax_full` | 1,650 | `isco08toisei08()` |
| `overview_full` | 1,650 | `isco08toisei08_2()` |

The three `_common` versions contain the same respondents in the same order, so those comparisons isolate changes in the outcome measure. The two `_full` versions add the four recovered ISCO 1000 cases. Stage 06 keeps the dissertation-style classical standard errors and adds HC3 alongside them. Its `historical_common` fits are checked back against the Stage 04 coefficients, fit statistics and VIFs before the revised versions are compared.

Under the `syntax` and `overview` mappings, the female coefficient stands out. It changes sign in all seven models, so Stage 07 traces where that shift comes from.

On the 1,646-person common sample, `overview` is approximately a compressed version of `syntax`, with a slope of 0.725 and R² of 0.841. After removing that common rescaling, the occupation-specific part of the female-coefficient shift stays close to -2.79 in all seven models. Stage 07 attributes that remainder back to ISCO codes. The ranked contributions are in `07_female_isco_residual_top_contributions.csv`. Cells with fewer than five respondents are pooled in the public contribution tables.

For the two-person postgraduate-by-1957-64 cell in Model 6, Stage 07 also compares classical and HC3 inference. Both respondents have ISCO 1120.

## Data and measures

The reproduction sample covers respondents aged 25 to 60 who completed junior college or above and reported a valid current or recent non-farm occupation.

The main origin measure is CGSS variable `A27H`, the respondent's hukou at age 14. Codes 1 to 2 are treated as rural origin and 3 to 5 as urban origin. Model 7 uses `A27F`, place of residence at age 14, with the same rural-urban split.

Parental education uses `A89B` and `A90B`. The analysis takes the higher available parental education value after converting categories to approximate years of schooling. The reproduction keeps category 14 coded as 9 years because that is what the submitted workflow used.


## Status

Stages 01 to 07 are frozen. `docs/session_info.txt` records the original reproduction environment, and `docs/session_info_revised.txt` records the environment used for Stages 05 to 07.

## Author

**Minghao Yang**

MA (Hons) Sociology, University of Edinburgh

GitHub [@minghaoyang-research](https://github.com/minghaoyang-research)
