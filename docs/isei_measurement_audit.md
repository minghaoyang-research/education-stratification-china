# ISEI Measurement Audit

## Why I checked the conversion

The submitted dissertation used ISEI-08 as the measure of occupational status. When I returned to the project, I found that the conversion implemented in the archived SPSS workflow did not reproduce either of the documented ISCO-08 to ISEI-08 mappings examined here.

I did not want to replace the old scores and immediately rerun the models. Stage 05 therefore deals only with measurement. The historical variable is kept intact, and the revised conversions are applied separately to the same occupational codes. No revised regression models are estimated here.

The historical reproduction remains fixed at `v1.0-original-reproduction`.

## The three versions compared

The first version is the ISEI variable used in the submitted dissertation. Stage 03 reconstructs it directly from the archived final SPSS syntax.

For the revised work I use `ISCO08ConveRsions::isco08toisei08()` as the primary conversion. The CRAN documentation states that this function follows Harry Ganzeboom's SPSS syntax modules and returns floating point ISEI-08 scores. The package requires ISCO-08 codes as four-digit strings.

I also retain `ISCO08ConveRsions::isco08toisei08_2()` as a sensitivity check. This function follows Ganzeboom's overview table and returns integer scores. The package documentation notes that the overview table was revised earlier than the SPSS syntax modules and recommends the syntax-based function where the two differ.

The audit was run with `ISCO08ConveRsions` version 0.2.0.

## Occupational codes and leading zeros

CGSS stores the occupational codes numerically. That creates a small but important problem. Codes such as 0100 and 0310 appear in the data as 100 and 310, while the conversion functions expect four-character strings.

A preliminary check confirmed that passing `100`, `110` or `310` directly does not return a valid score. The corresponding forms `0100`, `0110` and `0310` do. Stage 05 therefore converts every valid occupational code to a four-character string before applying either revised mapping.

This check is included in the script because a failure here would not necessarily produce an obvious error. It could simply remove valid observations.

## Coverage

Stage 03 contains 1,674 respondents with a valid current or recent non-farm occupation. After the historical origin and parental education requirements are applied, 1,650 respondents remain before ISEI is considered.

The historical mapping assigns ISEI to 1,646 of them. The four unmapped respondents all have ISCO code 1000. They are urban-origin bachelor graduates.

Both documented conversions assign scores to all 1,650 respondents. Under the syntax-based conversion, ISCO 1000 receives 65.12.

The resulting sample change is exactly the one expected from those four cases:

| Quantity | Historical | Syntax-based |
|---|---:|---:|
| Analytic N | 1,646 | 1,650 |
| Urban origin | 928 | 932 |
| Bachelor | 806 | 810 |

No respondent who received a historical score becomes unmapped under the syntax-based conversion. This was checked directly. The change from 1,646 to 1,650 is therefore a measurement coverage change, not a reconstruction of the rest of the sample.

## How much the scores change

The revised conversion does more than recover four cases.

On the 1,646 respondents observed under both the historical and syntax-based measures, the correlation is 0.946. The average syntax score is 4.38 points higher, and the mean absolute difference is 5.90 points. The largest individual absolute difference is 24.54 points.

| Mapping | N | Mean | SD | Minimum | Maximum |
|---|---:|---:|---:|---:|---:|
| Historical | 1,646 | 51.98 | 16.34 | 16.00 | 88.00 |
| Syntax-based | 1,650 | 56.39 | 17.94 | 13.87 | 88.96 |
| Overview table | 1,650 | 51.50 | 14.18 | 16.00 | 89.00 |

The historical measure has only 28 distinct scores in the pre-ISEI core. The syntax-based version has 200 and the overview version has 62.

The submitted conversion is therefore substantially coarser than either documented alternative, particularly the syntax-based mapping. This does not by itself make the historical variable ordinal. ISEI is intended as a metric socioeconomic index, but the old coding compressed considerably more occupational variation than I realised when the dissertation was submitted.

## The two documented conversions are not interchangeable

The syntax-based and overview versions also differ from one another. Across all 1,650 respondents their correlation is 0.917, with a mean absolute difference of 7.23 points.

The overview version happens to have an overall mean close to the historical measure. That does not mean the two are structurally closer. On their common sample, the historical to overview correlation is 0.898, lower than the historical to syntax correlation of 0.946.

For that reason I do not use similarity in the overall mean to choose between the mappings. The primary choice follows the provenance of the conversion itself. The overview version remains useful as a sensitivity analysis.

## Where the historical and syntax scores differ

The changes are not evenly distributed across occupational groups.

| ISCO major group | N | Mean change | Mean absolute change |
|---|---:|---:|---:|
| 0 | 4 | 0.05 | 15.87 |
| 1 | 165 | 2.69 | 3.63 |
| 2 | 471 | 6.05 | 7.04 |
| 3 | 316 | 6.81 | 8.89 |
| 4 | 322 | 2.35 | 4.14 |
| 5 | 241 | 2.33 | 3.73 |
| 7 | 68 | 3.15 | 5.43 |
| 8 | 27 | 2.10 | 3.86 |
| 9 | 32 | 5.54 | 6.60 |

Major group 0 is a useful reminder that the mean can hide large offsetting changes. Its four common-support cases split between two military codes moving in opposite directions, leaving a mean change of only 0.05 despite a mean absolute change of 15.87 points.

Major groups 2 and 3 stand out. Together they contain 787 of the 1,646 common support cases and account for about 63 percent of the total absolute score change. I had initially expected the coarser historical treatment of groups 7, 8 and 9 to dominate the comparison. It does not.

Several occupational codes also matter because they combine a sizeable score difference with many respondents. For example, ISCO 3359 has 94 common support cases and rises from 56 to 64.40. ISCO 2330 has 69 cases and rises from 71 to 82.41. ISCO 2341 has 56 cases and rises from 67 to 76.49.

The largest occupational cell is not automatically the most influential one. ISCO 4110 has 149 common support cases, but its score changes only from 45 to 43.33.

Full code-level results and the person-weighted ranking are stored in `output/revised_measurement/`.

## Is the measurement change related to education or rural origin?

I also checked this before fitting any revised models. The comparison uses the same 1,646 respondents under both measures.

| Group | N | Mean historical ISEI | Mean syntax ISEI | Mean change | Mean absolute change |
|---|---:|---:|---:|---:|---:|
| Junior college | 707 | 48.05 | 51.72 | 3.67 | 5.81 |
| Bachelor | 806 | 53.61 | 58.50 | 4.90 | 5.98 |
| Postgraduate | 133 | 63.08 | 68.12 | 5.05 | 5.85 |
| Urban origin | 928 | 51.57 | 55.79 | 4.22 | 5.69 |
| Rural origin | 718 | 52.52 | 57.11 | 4.59 | 6.16 |

The syntax conversion raises scores more, on average, among bachelor and postgraduate graduates than among junior college graduates. The rural and urban difference is smaller. Rural origin respondents gain about 0.38 points more on average.

The six education-by-origin cells do not show one simple rural pattern. The mean changes are 3.46 and 3.88 for urban and rural junior college graduates, 4.62 and 5.31 for bachelor graduates, and 5.13 and 4.90 for postgraduate graduates.

These are descriptive checks of the measurement change. They are not substitutes for the revised regression models. Their purpose is to show, before those models are estimated, whether the new occupational scores move uniformly across the groups used in the analysis. They do not.

## What this means for the revised analysis

The syntax-based conversion will be used as the primary ISEI measure in the revised analysis. The overview table version will be retained as a sensitivity check.

I will also keep two effects separate when the models are rerun. First, the same 1,646 respondents can be analysed with the historical and syntax-based scores. That comparison isolates the change in occupational measurement. The syntax-based sample can then be expanded from 1,646 to 1,650, which isolates the effect of recovering the four ISCO 1000 cases.

This distinction matters because the four recovered cases are concentrated in one part of the covariate distribution: all are urban-origin bachelor graduates.

Stage 05 stops here. It does not estimate a revised occupational model.

## Files produced

The measurement audit writes its results to `output/revised_measurement/`. The outputs include coverage checks, pairwise comparisons, occupational code differences, major group summaries and grouped measurement diagnostics.

The respondent-level CGSS data are not exported.

## Sources for the revised conversion

[Schwitter, Nicole. `ISCO08ConveRsions`, version 0.2.0. CRAN, 2023.](https://cran.r-project.org/package=ISCO08ConveRsions)

[Ganzeboom, Harry B. G. and Donald J. Treiman. *International Stratification and Mobility File: Conversion Tools*. Department of Social Research Methodology, Amsterdam.](https://www.harryganzeboom.nl/ismf/index.htm)

The sensitivity conversion follows Ganzeboom's ISCO-08 with ISEI-08 overview table, as documented in the [`ISCO08ConveRsions` package](https://cran.r-project.org/package=ISCO08ConveRsions).

## Status

Stage 05 passes all programmed checks. I treat the measurement audit as complete before moving to the revised model comparison.
