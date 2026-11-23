# Côte d’Ivoire · aggregate

[HTML report](Cote_dIvoire_analysis_aggregate.html) · [Complete R analysis](Cote_dIvoire_analysis_aggregate.R) · [R Markdown](Cote_dIvoire_analysis_aggregate.Rmd)

## Run

```sh
Rscript aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

The R script includes the country definitions, preparation, descriptive analyses,
scores, models, applicable sensitivities and numerical checks. It calls common
utilities and rendering support automatically. No other country R scripts need
running. Keep the complete repository and authorized input ZIP.

By default, the HTML is replaced beside the R script. Optional runtime arguments
are `--output-dir`, `--work-dir`, `--cache-dir`, `--join-dir`, `--python` and
`--pandoc-dir`. Use `Rscript setup.R` once at the project root to restore packages.

## Population, assignment and inference

Coverage retains all source women; Section 2 onward uses original SURGO `FPN2/fpn2 = 1`.
Weight field: `WEIGHT`. All SURGO weighted estimates use `survey`;
modern-use models use `svyglm()` with `ids = ~1`, no strata/FPC and separate
administrative-area CR2/Satterthwaite inference. No random effects.

Only confirmed FP facilities contribute to current supply summaries, with valid
responses and genuine zeros retained separately for each measure. Facilities
contribute equally. Source eligibility/aggregation definitions are in the R script
and the report.

Women receive the whole-LGA/département/district FP-provider averages for their
area, combining Rural and Urban facilities.

Both demand and supply composite scores run higher = more challenges. Primary
area flags use zero; supplementary maps use 0.5, 1 and 1.5 final-composite SDs.
Individual models retain personal demand and assigned supply scores.

## Included checks and qualifications

- Latest regression-completeness appendix: included/excluded profiles, available-case
  models and response-weighted joint models with two-stage geographic jackknife intervals.
- Alternative score constructions and risk-distribution/threshold maps.
- Independent raw-input, aggregation, assignment, scoring and model checks.
- Four survey areas remain unmatched; Daoukro has undefined supply ratios. Missing profiles remain unassessed.

[Exact input filenames](../../docs/DATASET_FILENAMES.md) · [Run guide](../../docs/RUN_GUIDE.md) · [Methods](../../docs/SURVEY_AND_SCORING.md).
