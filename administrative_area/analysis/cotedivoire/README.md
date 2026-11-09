# Côte d’Ivoire: department-wide analysis

This separate module starts from the checked administrative woman-level join.
Each woman receives her own department-equivalent's facility summaries, with no
Rural/Urban supply split. Her original residence remains a model covariate. The
original residence-specific report and all source joins are preserved.

From the repository root, using the existing verified join and extracted inputs:

```sh
Rscript administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R \
  --join-dir administrative_area/output \
  --cache-dir administrative_area/data/cache \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --work-dir output/aggregate_reports/cotedivoire \
  --output-dir reports
```

The administrative join must contain `checked_geography/` and its `audit/`
facility inputs directly. A legacy container with `admin_reference/` is also
accepted. No Rural/Urban join is required. Omit `--join-dir` and supply the input
ZIP to prepare the administrative join directly. The optional ZIP revalidates a
reused join against the bundle and validates/extracts manifest members. For a
previously verified snapshot, `join-dir` and `cache-dir` can be supplied without
the ZIP. If Pandoc is not on the executable search path, supply `--pandoc-dir` or
`RSTUDIO_PANDOC`. External locations are caller-supplied; internal references
are relative.

The runner renders `Cote_dIvoire_analysis_aggregate.Rmd` to
`reports/Cote_dIvoire_analysis_aggregate.html`. The Rmd can also be knitted with
the same caller-supplied join/cache parameters. Do not distribute cached inputs,
woman-level frames, fitted objects or working files with the self-contained HTML.

## Population, linkage and definitions

- Retain all 10,859 source records. The coverage map includes the 10,856 records
  with geography; the other three lack geography, consent and weight.
- Section 2 onward uses original SURGO `FPN2 = 1`, consent and positive `WEIGHT`:
  4,292 women. No historical need reconstruction or permanent-protection additions.
- Preserve the checked health-district rollups to SURGO department-equivalent
  areas. There are 82 surveyed areas, 78 with facility matches; 2,712 of 3,078
  facility records are in these shared areas. The four unmatched areas are Bonon,
  Facobly, Gohitafla and Taabo. Their 153 geographically identified source women,
  including 49 in need, remain unmatched rather than receiving zero supply.
- Preserve six personal demand domains: healthcare permission, anticipated stigma,
  health/side-effect beliefs, future fertility, low method confidence under the
  retained knowledge label, and opposition to FP. Coding is imported explicitly
  from `analysis/cotedivoire/cdi_definitions.R`.
- Retain six supply indicators: short/long-acting offering, free-product provision,
  trained-provider share, counseling availability, at least one LARC in stock,
  and most-popular-product stock. Binary flags become valid-response proportions;
  facility proportions become equal-facility means. Include valid zeros/FALSE;
  exclude unknown/undefined values per indicator. Do not construct facility weights.
- Counseling is retained in primary and complete-facility analyses. Among linked
  facilities: 2,011 Yes, 378 No, 323 Not applicable. The last category is not blank
  and is not silently recoded No. Daoukro free provision remains undefined.

## Scores and models

The main geographic classification uses equal-weight component z-scores, with
separate final-score medians; exact ties are unflagged. Higher demand is worse;
higher raw supply is favorable. Supply risk is negative readiness. All six
components are required. Four unmatched areas and Daoukro remain supply-unassessed.

The planning table uses plain-language component labels. Two companion summaries
count appearances among the three priorities in the 17 supply-only departments
and 18 demand-only departments, separately. Their percentages describe departments,
not women reporting a concern. Both-flagged areas are excluded from these two
summaries. Labels do not change indicator definitions, scores or classifications.
Below these summaries, separate tables list each supply-only and demand-only
department with its three priorities and original component percentages. Priority
order uses standardized component scores, not raw percentages. Demand percentages
use women in need, final survey weights and valid-domain denominators. Supply
percentages retain the original facility-valid definitions and response coverage.

To inspect the saved department demand-score distribution without rebuilding the
analysis, run:

```sh
Rscript administrative_area/analysis/cotedivoire/plot_demand_score_distribution.R \
  --data-dir output/aggregate_reports/cotedivoire/Cote_dIvoire_analysis_aggregate_files \
  --output-dir output/aggregate_reports/cotedivoire/score_distribution
```

This exports PNG/SVG figures, department scores and summary statistics. The mean
is zero by construction; the composite SD need not equal one. The existing median
classification threshold is retained, and this command does not edit the report.

Individual modern-use models use each woman's personal demand responses/score
and her department's supply index. All models adjust for age, age squared,
education, partnership, fertility goal and residence. The common model sample
contains 3,854 women across 77 departments. Use `WEIGHT`, department-clustered CR2
covariance, Satterthwaite intervals and small-sample joint block tests. Recorded
`IDZD` clusters support overview intervals and an inference sensitivity; original
strata/FPC are unavailable. No random effects or interactions are introduced.

Nominal woman/department counts do not guarantee adequate effective degrees of
freedom. Weight concentration and leverage can weaken the six-indicator joint
test. Report its effective denominator degrees of freedom alongside pointwise
intervals. These are associations and relative planning signals, not causal
binding-constraint diagnoses, intervention effects or portfolio allocations.

## Rendered appendices

1. Five scoring approaches: ranks, original percentages, median/IQR, median count
   and average z-scores. Median count requires four of six unfavorable components.
2. Stock-indicator diagnosis: preserve and explain the supplied stock flags,
   applicability and upstream coding limitations without silently repairing them.
3. Separate weighted demand/supply PCA scores, coefficients, variance and models.
   Mixed supply signs are labeled as a pattern, not monotonically worse provision.

The complete-facility and common-woman demand sensitivity sections are omitted
from the current HTML. Existing diagnostic calculations remain internal for
reproduction and audit; they do not replace the main valid-response estimates.
The main models retain their documented woman-level complete-case requirements.
The main component, classification, frequency and detailed priority tables remain
unchanged, including FP counseling and response-coverage explanations.

## Important source-quality limitation

The supplied UoM indicator is retained to reproduce the existing analysis. Its
undefined stock flags are not all item nonresponse and must not be zero-filled.
The upstream CDI stock-item selector excludes `s6_16` (male condoms); it selects
lettered questions. `any(present)` without removing unknowns also retains missing
values for certain brand combinations. The reported 86.1% stock presence is
conditional on 1,219 known flags, not all facilities.

Stock-related scores and associations are provisional pending a reviewed source
correction. A complete-case restriction cannot repair an omitted input question.
The module documents this explicitly instead of silently changing approved source
definitions or presenting the replication as validated measurement of shortfalls.

## Verification and display

`verify_analysis.R` independently reconstructs raw-field preservation, exposure
assignments, demand coding, scores, coefficients, CR2 covariance, PCA via weighted
SVD, and both completeness checks. `verify_html.py` checks report order, embedded
figures/downloads, maps and artifact portability. These validate reproduction,
not missing-data assumptions, construct validity, adequate power or causality.

Display boundaries use archived CNTIG/OCHA geometry with the existing documented
Bonon, Gohitafla and Ouelle reconstructions. Area conservation and source-name
coverage are checked. Polygons never assign a facility or woman to a geography.
