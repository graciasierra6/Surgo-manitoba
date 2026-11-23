# Côte d’Ivoire · Rural/Urban

[HTML report](CotedIvoire_rural_urban_updated.html) · [Complete R analysis](CotedIvoire_rural_urban_updated.R) · [R Markdown](CotedIvoire_rural_urban_updated.Rmd)

## Run

```sh
Rscript rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
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
| Original SURGO women | `inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta` |
| Detailed UoM facilities | `inputs/uom/cdi/uom_cdi_facility_pooled.dta` |
| Cleaned facility indicators | `uom_pipeline/uom_summary_facility.csv` |
| Facility archetype assignments | `uom_pipeline/Facilities data with archetype assignments.csv` |
| Facility service volumes retained in the broad join | `uom_pipeline/new_demand_vol_facility.csv` |
| Reviewed geographic crosswalk; checked assignments are applied separately | `exports/uom_surgo_geo_crosswalk_reviewed.csv` |
| Supply dictionary | `exports/merged_supply_demand_dictionary.csv` |
| Health-district code labels | `lookups/cdi_health_district_codebook.csv` |

The full bundle also contains required geographic provenance and cross-country
reconciliation inputs. Preparation verifies all 56 members and reconstructs the
joins; the table above is not a minimum replacement bundle. The complete filename
list is [here](../../docs/DATASET_FILENAMES.md).

This report uses the same source surveys as the aggregate report, but assigns
matching area–Rural/Urban facility averages, with a flagged same-area fallback
when the exact cell has no confirmed FP providers.

## Population, assignment and inference

Coverage retains all source women; Section 2 onward uses original SURGO `FPN2/fpn2 = 1`.
Weight field: `WEIGHT`. All SURGO weighted estimates use `survey`;
modern-use models use `svyglm()` with `ids = ~1`, no strata/FPC and separate
administrative-area CR2/Satterthwaite inference. No random effects.

Only confirmed FP facilities contribute to current supply summaries, with valid
responses and genuine zeros retained separately for each measure. Facilities
contribute equally. Source eligibility/aggregation definitions are in the R script
and the report.

Women receive matching administrative-area–Rural/Urban FP-provider averages. If
that exact cell has no confirmed FP providers, a flagged same-area whole-area
profile is used where available. An undefined component in an existing profile
does not trigger automatic fallback. Planning heatmaps/maps remain whole-area.

Both demand and supply composite scores run higher = more challenges. Primary
area flags use zero; supplementary maps use 0.5, 1 and 1.5 final-composite SDs.
Individual models retain personal demand and assigned supply scores.

## Included checks and qualifications

- Existing scoring and PCA sensitivities, including modern-use models. The new
  regression-completeness checks were scoped to the aggregate reports.
- Alternative score constructions and risk-distribution/threshold maps.
- Independent raw-input, aggregation, assignment, scoring and model checks.
- Four survey areas remain unmatched; Daoukro has undefined supply ratios. Missing profiles remain unassessed.

[Exact input filenames](../../docs/DATASET_FILENAMES.md) · [Run guide](../../docs/RUN_GUIDE.md) · [Methods](../../docs/SURVEY_AND_SCORING.md).
