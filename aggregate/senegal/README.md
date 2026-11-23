# Senegal · aggregate

[HTML report](Senegal_analysis_aggregate.html) · [Complete R analysis](Senegal_analysis_aggregate.R) · [R Markdown](Senegal_analysis_aggregate.Rmd)

## Run

```sh
Rscript aggregate/senegal/Senegal_analysis_aggregate.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

The R script includes the country definitions, preparation, descriptive analyses,
scores, models, applicable sensitivities and numerical checks. It calls common
utilities and rendering support automatically. No other country R scripts need
running. Keep the complete repository and authorized input ZIP.

By default, the HTML is replaced beside the R script. Optional runtime arguments
are `--output-dir`, `--work-dir`, `--cache-dir`, `--join-dir`, `--python` and
`--pandoc-dir`. Use `Rscript setup.R` once at the project root to restore packages.

## Input data for this report

**Supply the same complete `SURGO_Manitoba_linkage_inputs_complete.zip` used for all
seven reports.** The R script selects the country files and aggregation itself.
Do not select a previously joined dataset or create a smaller country ZIP.

The country analysis uses these exact ZIP members:

| Input | Exact member inside the ZIP |
|---|---|
| Original SURGO women | `inputs/surgo/senegal/surgo_senegal_women.dta` |
| Detailed UoM facilities | `inputs/uom/senegal/uom_senegal_facility_pooled.dta` |
| Cleaned facility indicators | `uom_pipeline/uom_summary_facility.csv` |
| Facility archetype assignments | `uom_pipeline/Facilities data with archetype assignments.csv` |
| Facility service volumes retained in the broad join | `uom_pipeline/new_demand_vol_facility.csv` |
| Reviewed geographic crosswalk; checked assignments are applied separately | `exports/uom_surgo_geo_crosswalk_reviewed.csv` |
| Supply dictionary | `exports/merged_supply_demand_dictionary.csv` |
| Checked département assignment evidence | `lookups/senegal_health_district_department_evidence.csv` |

The full bundle also contains required geographic provenance and cross-country
reconciliation inputs. Preparation verifies all 56 members and reconstructs the
joins; the table above is not a minimum replacement bundle. The complete filename
list is [here](../../docs/DATASET_FILENAMES.md).

This report averages eligible facilities within the whole administrative area
and assigns that area profile to each woman living there.

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
- Checked health-district-to-département assignments retain documented geographic qualifications.

[Exact input filenames](../../docs/DATASET_FILENAMES.md) · [Run guide](../../docs/RUN_GUIDE.md) · [Methods](../../docs/SURVEY_AND_SCORING.md).
