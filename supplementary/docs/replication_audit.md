# Replication Audit

## Why I rebuilt the analysis

My undergraduate dissertation used CGSS 2017 and was originally analysed in SPSS. Before revising the paper, I decided to rebuild the analysis in R from the original data and the final SPSS syntax.

I did this for two reasons. First, I wanted to check whether the submitted results could actually be recovered from the archived files. Second, I needed a clean baseline before changing the ISEI conversion or any of the modelling choices.

Stages 01 to 04 reproduce the historical analysis. Stages 05 to 07 were added afterwards to audit the ISEI conversion, sample coverage and statistical inference. The later stages are kept separate rather than being used to rewrite the submitted analysis.

## Source data

The project uses the China General Social Survey 2017.

The respondent-level data are not stored in this repository. My local copy is expected at:

`data/raw/CGSS2017.sav`

Stage 01 checks that file before constructing any variables.

The archived file used for this reproduction has:

- 12,582 cases
- 798 variables as read by `haven`
- a file size of 26,231,277 bytes
- MD5 `4933ebaa85a5d2e38c1a15aa5ae6c04e`

The raw SAV file is excluded from Git.

This fingerprint is specific to the copy archived with the dissertation workflow. It contains 12 variables left by a later diagnostic run. Stage 01 removes those variables before rebuilding the analysis, leaving 786 variables in the working object. A separately obtained CGSS 2017 file will therefore not be byte-identical to this archive even if the underlying released survey data are otherwise the same.

## Final SPSS syntax and the historical ISEI lookup

The historical syntax used here is:

`archive/cgss2017_analysis_4.sps`

Its recorded MD5 is:

`6c631aa6c17f95932a96deb6d28f90a7`

The final syntax contains 257 `IF` assignments used in the historical ISEI recode. Rather than re-parsing the SPSS file every time Stage 03 runs, `tools/make_isei_map.R` extracts the historical lookup into:

`archive/isei_map_4.csv`

Stage 03 reads that CSV directly. The SPSS syntax is kept in the repository so the derivation of the lookup can still be checked.

The historical mapping is reproduced because it was the one used for the submitted dissertation. It is not treated as the preferred ISCO-08 to ISEI-08 conversion in the later audit.

## An old verification variable that should not be used

The SAV file contains 12 variables left over from an earlier checking exercise:

`step1_pass`

`step2_pass`

`step3_pass`

`age_2017`

`has_occ`

`occ_code`

`isei_unmappable`

`origin_miss`

`father_miss`

`mother_miss`

`pared_miss`

`analytic_sample`

Stage 01 does not use these variables to construct the sample. It removes them from the clean working object and rebuilds the sample from the original CGSS variables.

The old `analytic_sample` variable identifies 1,649 cases, while the final dissertation analysis uses 1,646. The April diagnostic output makes the source of that difference clearer. Among the same 1,674 occupation-valid cases, the older routine marked 2 cases as missing on origin, 19 on parental education and 4 as ISEI-unmappable:

`1,674 - 2 - 19 - 4 = 1,649`

The final historical coding identifies 5 invalid origin cases instead:

`1,674 - 5 - 19 - 4 = 1,646`

The occupation step is the same in both counts. I therefore treat the 1,649 flag as an archived diagnostic rather than a definition of the submitted sample.

## Reconstructing the sample

The sample flow reproduced in R is:

| Step | N |
|---|---:|
| Full CGSS 2017 sample | 12,582 |
| Higher education, A7A 9 to 13 | 2,470 |
| Age 25 to 60 | 1,766 |
| Valid current or recent nonfarm occupation | 1,674 |
| Valid origin and parental education | 1,650 |
| Final historical analytic sample | 1,646 |

The 1,650 row is useful for checking the reconstruction, although it did not appear as a separate row in the submitted dissertation.

Among the 1,674 cases with a valid current or recent occupation, five do not have a valid A27H origin under the historical coding. Nineteen more have no usable education information for either parent. These groups do not overlap, leaving 1,650 cases before ISEI is applied.

Four of those cases cannot be assigned an ISEI score under the historical mapping. The final sample is therefore:

`1,674 - 5 - 19 - 4 = 1,646`

I also checked the overlaps directly rather than relying only on the arithmetic.

### A7A and graduation status

The submitted higher-education restriction uses A7A, the reported highest education level. It does not additionally require A7B to indicate graduation.

In the final historical sample, A7B is distributed as follows:

| A7B status | N |
|---|---:|
| Currently studying | 26 |
| Withdrawn / left before graduation | 5 |
| Incomplete | 4 |
| Graduated | 1,610 |
| Unknown | 1 |

So 1,610 of the 1,646 cases are explicitly coded as graduated in A7B. I have not changed the historical sample to impose an A7B graduation filter, because that would no longer reproduce the submitted analysis.

The two postgraduate respondents in the 1957 to 1964 Model 6 reference cell are both coded as graduated in A7B, so the sparse-cell problem in that model is not caused by current students being placed in the postgraduate category.

## Origin measure

The main origin variable comes from A27H. This item records the type of place where the respondent's hukou was registered at age 14. It is not an agricultural versus non-agricultural hukou-status measure.

The historical coding groups A27H codes 1 to 2 as rural origin and codes 3 to 5 as urban origin. Model 7 uses A27F, place of residence at age 14, with the same split.

I keep those groupings because they reproduce the submitted models. They should be read as a rural-urban classification of the place recorded by the survey item, not as a direct measure of hukou legal status.

## Occupational code

The historical occupation variable first uses the respondent's current main nonfarm occupation in `isco08_a59`.

If that is unavailable, it falls back to the most recent nonfarm occupation in `isco08_a60`.

The historical validity rule keeps codes greater than 0 and below 10,000. There is no lower bound of 100 or 1,000.

That detail is easy to miss. ISCO 100 and 310 are both retained, and both receive ISEI 45 under the historical mapping.

## Rebuilding the historical ISEI variable

Stage 03 applies the lookup derived from the final SPSS syntax.

Among the 1,674 occupation-valid cases:

- 1,670 receive an ISEI value
- 4 remain unmapped

All four unmapped cases have `isco_main = 1000`.

They are also all in the urban-origin bachelor category under the historical coding. After their exclusion, the final sample contains 1,646 respondents.

The reproduced ISEI distribution is:

| Statistic | Value |
|---|---:|
| Mean | 51.9842041312 |
| Standard deviation | 16.3434563336 |
| Minimum | 16 |
| Maximum | 88 |

The final SPSS output displays the mean as 51.9842 and the standard deviation as 16.34346. The R values reproduce both.

## Final sample checks

The education counts are:

| Education | N |
|---|---:|
| Junior college | 707 |
| Bachelor | 806 |
| Postgraduate | 133 |

Origin is split into 928 urban respondents and 718 rural respondents.

The four cohort counts are:

| Cohort | N |
|---|---:|
| 1957 to 1964 | 170 |
| 1965 to 1974 | 334 |
| 1975 to 1984 | 514 |
| 1985 to 1992 | 628 |

There are 837 men and 809 women.

The mean age is 37.7947 and the standard deviation is 9.57887. Mean parental education is 9.7631 years, with a standard deviation of 4.04440.

## A stronger joint check

I also checked ISEI within each combination of education and origin.

| Education | Origin | N | Mean ISEI |
|---|---|---:|---:|
| Junior college | Urban | 361 | 48.0028 |
| Junior college | Rural | 346 | 48.0983 |
| Bachelor | Urban | 480 | 52.4937 |
| Bachelor | Rural | 326 | 55.2423 |
| Postgraduate | Urban | 87 | 61.2874 |
| Postgraduate | Rural | 46 | 66.4565 |

These six cells sum to 1,646.

I kept this check because it is harder to pass by accident than separate marginal totals. The sample definition, education coding, origin coding and ISEI assignment all have to agree for the six counts and six means to reproduce the SPSS output.

## Models 1 to 7

Stage 04 rebuilds the seven OLS models from the submitted analysis.

At this stage I keep the original conventional standard errors. Robust standard errors belong to the later audit and are not inserted into the historical reproduction.

The R models are checked against the final SPSS output for every coefficient and standard error. I also compare the available fit statistics, residual degrees of freedom, selected residual sums of squares and VIF benchmarks.

The historical cross tabulations and grouped means are checked in the same script.

All programmed checks pass.

## Cross tabulations

The rural origin by education table gives:

`χ²(2) = 15.847389`

The final SPSS output reports 15.847.

For cohort by education:

`χ²(6) = 63.051020`

SPSS reports 63.051.

The 12 cohort by education ISEI means also match the final SPSS output. The smallest cell is the postgraduate group born between 1957 and 1964, which contains only two respondents and has a mean ISEI of 70.00.

## Additional interaction tests

The original dissertation reported the individual interaction coefficients, but it did not report joint tests for the interaction blocks.

I added those tests during the R reproduction because they provide a clearer way to assess whether the interaction terms improve the model as a group.

For the education by rural origin terms, comparing Model 3 with Model 4 gives:

`F(2, 1637) = 1.500909, p = 0.223234`

For the education by cohort terms, comparing Model 5 with Model 6 gives:

`F(6, 1631) = 1.164083, p = 0.322875`

Neither interaction block is jointly significant.

These are new diagnostic results calculated from the reproduced models. They were not statistics reported in the submitted dissertation.

## Multicollinearity

The dissertation stated that Models 1 to 5 and Model 7 did not show problematic VIF values. The R reproduction calculates VIFs for those models and the largest is:

`3.218004`

Rounded to two decimal places, this is 3.22.

Model 7 needs one qualification here. The final SPSS output does not print collinearity statistics for that model, so its VIF is an R diagnostic rather than an estimate-by-estimate reproduction of a printed Model 7 SPSS table.

Model 6 is the exception. Once the education by cohort interactions are added, several VIFs become very large.

The postgraduate main effect has a VIF of about 62.480. The postgraduate by 1975 to 1984 term is about 22.563, while the postgraduate by 1985 to 1992 term is higher still.

This fits the structure of the data. The earlier cohorts contain very few postgraduate respondents, including only two in the oldest cohort. I therefore regard Model 6 as an exploratory historical model rather than a stable basis for interpreting cohort differences.

## A reporting issue in Model 3

This is a rounding issue in the written dissertation, not an error in the model calculation.

The unrounded rural origin coefficient from Model 3 is:

`3.02452982`

SPSS displays this to three decimal places as:

`3.025`

Rounded directly from the unrounded estimate to two decimal places, it is:

`3.02`

The submitted dissertation reports 3.03.

The difference came from rounding the already rounded SPSS display value again. The fitted model itself is unchanged, as are the significance test and substantive conclusion.

## A transcription error in Appendix A2

I found a different problem in Appendix A2.

For the postgraduate by 1975 to 1984 interaction in Model 6, the values copied into the submitted appendix do not match the final SPSS output.

The R reproduction gives:

- coefficient 4.478815
- standard error 11.462080
- VIF 22.563430

To three decimal places, these are 4.479, 11.462 and 22.563.

The final SPSS output contains the same values.

Unlike the Model 3 issue, this is not a rounding convention. The numbers were transcribed incorrectly into the appendix. The underlying model was still estimated correctly.

## Parental education category 14

The historical coding assigns nine years of schooling to parental education category 14.

The CGSS label for category 14 is `Other`. It does not correspond to a defined education level.

I could not find a note in my original files explaining why I assigned nine years to this category. For the historical reproduction I have left the coding unchanged, because changing it here would make the R analysis different from the submitted one.

Stages 05 to 07 did not revisit this coding. An alternative treatment of category 14 would be a separate sensitivity analysis rather than a correction to the historical reproduction.

## Post-submission ISEI audit

Stage 05 compares the historical ISEI coding with two mappings from `ISCO08ConveRsions` 0.2.0. `isco08toisei08()` is used as the main revised mapping and `isco08toisei08_2()` as a sensitivity mapping. Three-digit ISCO codes are zero-padded before those functions are called.

The revised mappings score the four ISCO 1000 cases that the historical recode left unmapped. This produces two full revised samples of 1,650 cases, alongside three common-support versions containing the same 1,646 historical respondents.

Stage 06 refits Models 1 to 7 under those five fixed versions. Common-support comparisons change the ISEI measurement while holding respondents and design matrices fixed; the full versions additionally show what happens when the four recovered cases are included.

Stage 07 follows up the sparse Model 6 cell, the compression between the two revised ISEI scales and the sensitivity of the female coefficient to the crosswalk. HC3 standard errors are used here as a later diagnostic; they do not replace the classical standard errors in the submitted models. In Model 6 the oldest-cohort postgraduate cell has very high leverage, and classical and HC3 inference diverge sharply. I therefore treat inference from that model as unstable. Outcome summaries for cells with fewer than five respondents are suppressed in the public Stage 06 and 07 CSV outputs.

Nothing in Stages 05 to 07 is fed back into Stages 01 to 04.

## Output

Aggregate reproduction results are written to:

`output/original_replication/`

The later audit writes aggregate or code-level results to:

`supplementary/output/revised_measurement/`

`supplementary/output/isei_model_comparison/`

`supplementary/output/model_diagnostics/`

No respondent-level CGSS data are exported by these stages.

## Current status

Stages 01 to 07 are complete and frozen. Stages 01 to 04 remain the historical reproduction; Stages 05 to 07 are the separate post-submission audit.

`docs/session_info.txt` records the original reproduction environment. `supplementary/docs/session_info_revised.txt` records the environment used for Stages 05 to 07.
