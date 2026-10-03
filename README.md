# Education, Social Origin and Occupational Status in China

Reproducible R reconstruction of an undergraduate quantitative sociology dissertation using the 2017 China General Social Survey (CGSS).

The project examines how higher-education attainment, social origin and birth cohort are associated with occupational status in China. The central question is whether urban-rural inequality among respondents who reached higher education is concentrated mainly in the qualifications they obtain or remains visible in occupational position after education and family background are taken into account.

The submitted analysis was originally conducted in SPSS. This repository rebuilds the analytic sample from the archived CGSS source file, reconstructs the variables used in the dissertation, reproduces the seven OLS models in R, and validates the results against the retained final SPSS output.

Respondent-level CGSS records are not redistributed here.

## Research at a glance

| Item | Design |
|---|---|
| Data | China General Social Survey 2017 |
| Starting sample | 12,582 respondents |
| Analytic sample | 1,646 respondents |
| Age range | 25–60 |
| Education | Junior college or above |
| Outcome | ISEI-08 occupational status as coded in the submitted SPSS workflow |
| Main predictors | Education level, rural origin, parental education, age / birth cohort, sex |
| Method | Seven unweighted OLS models |
| Original software | IBM SPSS |
| Reproduction | R 4.4.3 |

The analytic sample is rebuilt from the survey items rather than loaded from a saved sample flag. The main sample flow is:

```text
12,582  CGSS 2017 respondents
 2,470  junior college or above
 1,766  aged 25–60
 1,674  valid current or most recent non-farm occupation
 1,646  final submitted analytic sample
```

## Main findings

Education is strongly associated with occupational position. In Model 3, with junior college as the reference category, the bachelor coefficient is **6.205 ISEI points** and the postgraduate coefficient is **15.899 points**.

Within this higher-education sample, rural origin does not appear as a negative occupational-status gap after controls. The Model 3 rural-origin coefficient is **3.025 points** (SE = 0.833, p < .001). I do not interpret that estimate as a causal rural advantage: the analysis conditions on having reached higher education, so selection into the higher-education sample remains important.

The education-by-origin interaction block is not jointly significant. At the same time, the education-by-origin cross-tabulation shows clear sorting across qualification levels: rural-origin respondents are more concentrated in junior college and less concentrated in the higher tiers.

Later-born cohorts have lower occupational-status estimates in the cohort specification. Because all respondents are observed in the same 2017 cross-section, the analysis cannot cleanly separate cohort change from career-stage differences, so the cohort pattern is interpreted cautiously.

Model 6, which adds education-by-cohort interactions, is retained as exploratory because the oldest-cohort postgraduate cell contains only two respondents and produces substantial multicollinearity. The substantive conclusions do not rely on that model.

## Reproduction and validation

Stages 01 to 04 form the core reproduction.

The scripts rebuild the analysis from the archived SAV file and compare the R results directly with the retained final SPSS output. Structural quantities such as sample counts, residual degrees of freedom and table cell counts are checked exactly. Coefficients and classical standard errors are checked against the precision printed by SPSS, together with the available fit and VIF benchmarks.

The reproduction validates:

- the full sample-construction sequence;
- the education, origin, cohort, parental-education and occupation variables;
- the ISEI-08 scores used in the submitted workflow;
- Models 1 to 7;
- Table 6: education by urban-rural origin;
- Table 7: mean ISEI by birth cohort and education level;
- Appendix Table A1: education by birth cohort;
- model fit and multicollinearity benchmarks available in the archived output.

The R reconstruction also adds two joint interaction-block tests as transparent diagnostics:

- education × rural origin: F(2, 1637) = 1.501, p = .223;
- education × cohort: F(6, 1631) = 1.164, p = .323.

These tests are calculated from the reproduced models and are not substituted for any statistic in the submitted dissertation.

## Repository structure

```text
R/
  01_import_validate.R
  02_construct_variables.R
  03_original_isei.R
  04_original_models.R

archive/
  cgss2017_analysis_4.sps
  isei_map_4.csv

tools/
  make_isei_map.R

data/raw/
  CGSS2017.sav                  # not tracked by Git

output/
  original_replication/

docs/
  session_info.txt

education-stratification-china.Rproj
```

`cgss2017_analysis_4.sps` is the final dissertation syntax retained with the project. `tools/make_isei_map.R` extracts the occupational-status lookup used by that syntax into `archive/isei_map_4.csv`, which Stage 03 reads.

The portfolio reproduction is contained in Stages 01 to 04. Other scripts retained in the repository are supplementary project work and are not required to rebuild the submitted analysis.

## Data and measures

The sample includes respondents aged 25 to 60 whose highest reported education in A7A is junior college or above and who have a valid current or most recent non-farm occupation.

Education is represented by junior college, bachelor and postgraduate categories, with junior college as the reference group.

The main origin measure is based on A27H, the type of place where the respondent's hukou was registered at age 14. Codes 1–2 are grouped as rural origin and codes 3–5 as urban origin. Model 7 uses A27F, place of residence at age 14, with the same rural-urban grouping.

Parental education uses the higher available education level of either parent after conversion to approximate years of schooling.

Occupational status is the ISEI-08 outcome used in the submitted SPSS analysis. Stage 03 reconstructs those scores from the archived final syntax through the project lookup table, so the R reproduction targets the analysis that was actually submitted rather than substituting a different outcome definition.

The submitted models use unweighted OLS with classical model-based standard errors, and the reproduction keeps that specification unchanged.

## Getting the data

CGSS is conducted by the National Survey Research Center at Renmin University of China.

The respondent-level CGSS 2017 file is not redistributed in this repository. Place the archived SAV used for the reproduction at:

```text
data/raw/CGSS2017.sav
```

Stage 01 validates the archived source against the following fingerprint:

```text
Cases       12,582
Variables   798
File size   26,231,277 bytes
MD5         4933ebaa85a5d2e38c1a15aa5ae6c04e
```

The archived working SAV contains 12 diagnostic variables created during the original workflow. Stage 01 removes those fields before analysis, leaving the 786 survey variables used to rebuild the sample.

The raw-data directory is excluded from Git.

## Running the reproduction

The verified reproduction was run under R 4.4.3. The core reproduction requires `haven` to read the SPSS file; model estimation and validation otherwise rely mainly on base R.

```r
install.packages("haven")
```

Open `education-stratification-china.Rproj`, place the SAV file in `data/raw/`, and run:

```r
source("R/04_original_models.R")
```

Stage 04 sources Stages 03, 02 and 01 in sequence, so the complete analysis is rebuilt from the source SAV file.

A successful run ends with:

```text
04_original_models.R: ALL CHECKS PASSED
```

Aggregate reproduction outputs are written to:

```text
output/original_replication/
```

No respondent-level analytic dataset is written by the reproduction scripts.

## Reproducibility status

The submitted SPSS analysis has been independently reconstructed in R and checked against the retained final output. The original reproduction baseline is preserved at the Git tag `v1.0-original-reproduction`.

## Author

**Minghao Yang**

MA (Hons) Sociology — First Class Honours  
University of Edinburgh

GitHub [@minghaoyang-research](https://github.com/minghaoyang-research)
