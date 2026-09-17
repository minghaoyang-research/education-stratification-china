# Replication Audit

## Why I rebuilt the analysis

My undergraduate dissertation used CGSS 2017 and was originally analysed in SPSS. Before revising the paper, I decided to rebuild the analysis in R from the original data and the final SPSS syntax.

I did this for two reasons. First, I wanted to check whether the submitted results could actually be recovered from the archived files. Second, I needed a clean baseline before changing the ISEI conversion or any of the modelling choices.

Stages 01 to 04 therefore reproduce the historical analysis. Later scripts will contain the revised analysis. I have kept the two separate rather than correcting the old analysis retrospectively.

## Source data

The project uses the China General Social Survey 2017.

The respondent level data are not stored in this repository. My local copy is expected at:

`data/raw/CGSS2017.sav`

Stage 01 checks that file before constructing any variables.

The file used for this reproduction has:

- 12,582 cases
- 798 variables as read by `haven`
- a file size of 26,231,277 bytes
- MD5 `4933ebaa85a5d2e38c1a15aa5ae6c04e`

The raw SAV file is excluded from Git.

I have not used the raw variable count alone as a provenance check because software can represent SPSS metadata differently. The file size, MD5, row count, required variables, and later sample checks provide a stronger combined check.

## Final SPSS syntax

The historical syntax used here is:

`archive/cgss2017_analysis_v4.sps`

Its MD5 is:

`6c631aa6c17f95932a96deb6d28f90a7`

The file contains 515 lines. Stage 03 finds 257 `IF` statements that assign ISEI values.

Those assignments are applied in the same order in R. Order matters because some later SPSS statements can overwrite values assigned earlier.

The archived SPSS syntax also contains Chinese comments. It is read explicitly as UTF-8 in the R reproduction. The ISEI rule extraction operates on the `IF` statements themselves rather than on the comment text.

This historical mapping is reproduced because it was the one used for the submitted dissertation. It is not assumed to be the preferred ISCO 08 to ISEI 08 conversion for the revised paper.

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

This turned out to matter. The old `analytic_sample` variable identifies 1,649 cases, while the final dissertation analysis uses 1,646.

The 1,649 count came from an earlier verification routine. That routine did not implement the same origin and occupation checks as the final v4 syntax, so I treat it as an archived diagnostic rather than a definition of the submitted sample.

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

The 1,650 row is useful for debugging the reconstruction, although it did not appear as a separate row in the submitted dissertation.

Among the 1,674 cases with a valid current or recent occupation, five do not have a valid A27H origin under the historical coding. Nineteen more have no usable education information for either parent.

These groups do not overlap, leaving 1,650 cases before ISEI is applied.

Four of those cases cannot be assigned an ISEI score under the historical mapping. The final sample is therefore:

`1,674 - 5 - 19 - 4 = 1,646`

I also checked the overlaps directly rather than relying only on the arithmetic.

## Occupational code

The historical occupation variable first uses the respondent's current main nonfarm occupation in `isco08_a59`.

If that is unavailable, it falls back to the most recent nonfarm occupation in `isco08_a60`.

The historical validity rule keeps codes greater than 0 and below 10,000. There is no lower bound of 100 or 1,000.

That detail is easy to miss. ISCO 100 and 310 are both retained, and both receive ISEI 45 under the historical v4 syntax.

## Rebuilding the historical ISEI variable

Stage 03 reconstructs ISEI directly from the final SPSS syntax.

Among the 1,674 occupation valid cases:

- 1,670 receive an ISEI value
- 4 remain unmapped

All four unmapped cases have `isco_main = 1000`.

They are also all urban origin bachelor graduates under the historical coding. After their exclusion, the final sample contains 1,646 respondents.

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

I kept this check because it is harder to pass by accident than separate marginal totals. The sample definition, education coding, origin coding, and ISEI assignment all have to agree for the six counts and six means to reproduce the SPSS output.

## Models 1 to 7

Stage 04 rebuilds the seven OLS models from the submitted analysis.

At this stage I keep the original conventional standard errors. Robust standard errors belong to the revised analysis and are not inserted into the reproduction.

The R models are checked against the final SPSS output for every coefficient and standard error. I also compare the reported fit statistics, residual degrees of freedom, selected residual sums of squares, and VIF values.

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

The dissertation stated that Models 1 to 5 and Model 7 did not show problematic VIF values. The R reproduction supports that statement.

The largest VIF across those models is:

`3.218004`

Rounded to two decimal places, this is 3.22.

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

For the revised paper, reported values will be rounded directly from the unrounded R output.

## A transcription error in Appendix A2

I found a different problem in Appendix A2.

For the postgraduate by 1975 to 1984 interaction in Model 6, the values copied into the submitted appendix do not match the final SPSS output.

The R reproduction gives:

- coefficient 4.478815
- standard error 11.462080
- VIF 22.563430

To three decimal places, these are 4.479, 11.462, and 22.563.

The final SPSS output contains the same values.

Unlike the Model 3 issue, this is not a rounding convention. The numbers were transcribed incorrectly into the appendix. The underlying model was still estimated correctly.

## Parental education category 14

The historical coding assigns nine years of schooling to parental education category 14.

The CGSS label for category 14 is `Other`. It does not correspond to a defined education level.

I could not find a note in my original files explaining why I assigned nine years to this category. For the historical reproduction I have left the coding unchanged, because changing it here would make the R analysis different from the submitted one.

The revised analysis will treat category 14 separately and check whether changing its treatment affects the results.

## Output

Aggregate reproduction results are written to:

`output/original_replication/`

The folder contains the extracted historical ISEI rules, the resolved historical mapping, unmapped codes, model coefficients, fit statistics, VIF results, cross tabulations, grouped means, and the two additional interaction tests.

No respondent level CGSS data are exported there.

## Current status

Stages 01 to 04 reproduce the final historical analysis in R and pass all programmed checks.

I now treat this as the frozen historical baseline. Any changes to the ISEI conversion, sample definition, standard errors, or model specification will be made in separate revised scripts rather than by altering Stages 01 to 04.
