# Senegal: department-only analysis

This report is separate from the country reports using department–residence
exposures. It starts from the existing **checked department-only woman-level
join**. All women in one department receive that department's supply profile,
regardless of Rural/Urban residence. Residence remains a model covariate.

From the repository root, using an existing administrative join:

```sh
Rscript administrative_area/analysis/senegal/Senegal_analysis_aggregate.R \
  --join-dir administrative_area/output \
  --cache-dir administrative_area/data/cache \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --work-dir output/aggregate_reports/senegal \
  --output-dir reports
```

`join-dir` is the caller-supplied output of `administrative_area/R/run_join.R`
with checked geography. It contains `checked_geography/` and `audit/` directly.
A legacy join container with `admin_reference/` is also accepted. No Rural/Urban
join is required. Omit `--join-dir` and supply the input ZIP to build the
administrative join directly. A supplied ZIP revalidates the join's bundle hash
and all source-member checksums. For an already verified snapshot, the ZIP is
optional. Do not distribute source microdata, cached inputs, fitted objects or
working files with the HTML.

The output is `reports/Senegal_analysis_aggregate.html`. It is also possible to
knit `Senegal_analysis_aggregate.Rmd` with caller-supplied `join_dir`, `cache_dir`
and optional `input_zip` parameters. Internal references remain project-relative.
Pandoc must be installed; if it is not on the executable search path, supply its
directory with `--pandoc-dir` or the standard `RSTUDIO_PANDOC` environment variable.

## Definitions and primary analysis

- Preserve all 7,169 source women. Section 1 describes source coverage. Section 2
  onward uses original SURGO `FPN2 = 1`: 1,905 women.
- Use the same six personal demand definitions as the existing Senegal report.
- Retain all six supply measures in the main analysis: short/long-acting offering,
  free-product provision, trained-provider share, FP counseling availability, at
  least one LARC in stock, and most-popular-product stock. Counseling uses its
  valid Yes/No denominator; only 193 of 1,648 linked facilities answered, and the
  report discloses this sparse coverage.
- For primary profiles, average each original facility indicator over its valid
  responses within the department. Valid zero/FALSE values are included; unknown
  or undefined values are excluded from that indicator's denominator. No facility
  weights, pooled-product replacement, zero filling or FP-provider-only filter
  is introduced. The existing boolean profile rounding is retained.
- Planning scores use six weighted department concern percentages and six
  unweighted supply summaries. Average-z classification is primary. Favorable
  supply below its score median and demand above its score median are flagged;
  ties are unflagged. The resulting categories are relative planning signals.
- Below the main map, two summaries count appearances among the three priorities
  in the eight supply-only and eight demand-only departments. Separate tables
  list those departments, their ordered priorities and original percentages.
  Clearer display labels do not change indicators or classifications. Priority
  order uses component z-scores, not raw percentages; summary percentages count
  departments, not women. Both-flagged areas remain in the full planning table.
  The counseling response-coverage warning is retained alongside these tables.
- Modern use is the outcome in all models. Individual demand scores use each
  woman's own responses; contextual supply uses her department's six measures.
  All models adjust for age, age squared, education, partnership, fertility goal
  and residence. They retain `WEIGHT` and use department-clustered CR2 intervals
  with Satterthwaite degrees of freedom. No random effects or interactions.
- Recorded `RB0H` census districts support survey overview intervals and an
  inference sensitivity. Original strata and FPC are unavailable. Models share
  the same complete-case women; facility missingness and woman-level complete
  cases are different selection issues.

## Rendered appendices

1. Five scoring approaches: ranks, original percentages, median/IQR, median count
   and average z-scores. Median count requires four of six unfavorable components.
2. Separate weighted demand/supply PCA scores, coefficients, variance and models.
   Mixed supply signs are labeled as a pattern, not monotonically worse provision.

The complete-facility and common-woman demand sensitivity sections are omitted
from the current HTML. Existing diagnostic calculations remain internal for
reproduction and audit; they do not replace the main valid-response estimates.
The main models retain their documented woman-level complete-case requirements.
The main component, classification, frequency and detailed priority tables remain
unchanged, including FP counseling and response-coverage explanations.

`verify_analysis.R` independently checks raw-field preservation, the geographic
crosswalk, every assigned supply measure, scoring, coefficients and CR2
covariance, facility and demand completeness checks, common model samples and weighted-SVD PCA.
`verify_html.py` checks section order, maps, embedded figures/downloads, expected
counts and artifact portability. These checks do not establish causal effects,
representativeness of nonresponding facilities, or adequate statistical power.

## Files

`build_analysis.R` calculates summaries and planning scores; `fit_models.R` fits
the five adjusted specifications; `build_pca_scores.R` constructs PCA checks;
`prepare_map.R` prepares archived boundaries; `render_analysis.py` formats the
verified aggregates. The runner executes them in that order and renders the Rmd.
The shared original Senegal response coding and plotting remain explicit project
dependencies. Existing residence-specific reports and joins are not overwritten.
