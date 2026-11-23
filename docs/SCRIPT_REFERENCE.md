# R script reference

Use [the colleague run guide](COLLEAGUE_RUN_GUIDE.md) for the exact input list,
package setup, separate commands and expected report names. This index describes
every project R script, including shared helpers, optional diagnostics, historical
modules and tests. Input contracts distinguish original datasets from generated
working objects. It is not a list of scripts to run one after another.

Package lists identify explicit package calls/loads in each script; helpers can
add transitive dependencies. `DESCRIPTION` declares project dependencies and
`renv.lock` records all restored versions. Packages bundled with R do not need
separate installation. Restore the environment once with
`Rscript R/install_dependencies.R`. Namespace-qualified calls do not require
repeated `library()` calls in every stage.

Original datasets and checksums are in [INPUTS.md](INPUTS.md) and the
[dataset catalog](dataset_catalog.json). Main entry headers name their selected
country files explicitly; generated joins, frames, fitted models and caches are
runtime working data and are not repository input files.

## Entry points

| R script | Task |
|---|---|
| [`R/rebuild_all.R`](../R/rebuild_all.R) | Complete current report/diagnostic rebuild; optionally includes pinned historical reports. |
| [`R/run_aggregate_reports.R`](../R/run_aggregate_reports.R) | Run all four whole-area reports, or the --countries subset. |
| [`R/run_reports.R`](../R/run_reports.R) | Compatibility runner for historical reports; current Sindh delegates to district analysis. |
| [`R/install_dependencies.R`](../R/install_dependencies.R) | Restore the R package environment; run once before analyses. |
| [`administrative_area/R/run_join.R`](../administrative_area/R/run_join.R) | Build base administrative joins and upstream reconciliation, before current FP filtering. |
| [`urban_rural/R/run_join_urban_rural.R`](../urban_rural/R/run_join_urban_rural.R) | Build base residence joins and an administrative reference; no regression models. |
| [`administrative_area/analysis/lagos/Lagos_analysis_aggregate.R`](../administrative_area/analysis/lagos/Lagos_analysis_aggregate.R) | Current whole-area report: prepare FP-provider summaries, fit and verify, then render matching Rmd. |
| [`administrative_area/analysis/senegal/Senegal_analysis_aggregate.R`](../administrative_area/analysis/senegal/Senegal_analysis_aggregate.R) | Current whole-area report: prepare FP-provider summaries, fit and verify, then render matching Rmd. |
| [`administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R`](../administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R) | Current whole-area report: prepare FP-provider summaries, fit and verify, then render matching Rmd. |
| [`administrative_area/analysis/sindh/Sindh_analysis.R`](../administrative_area/analysis/sindh/Sindh_analysis.R) | Current whole-area report: prepare FP-provider summaries, fit and verify, then render matching Rmd. |
| [`urban_rural/analysis/lagos/Lagos_rural_urban_updated.R`](../urban_rural/analysis/lagos/Lagos_rural_urban_updated.R) | Current residence report: prepare exact/fallback FP-provider supply, fit and verify, then render matching Rmd. |
| [`urban_rural/analysis/senegal/Senegal_rural_urban_updated.R`](../urban_rural/analysis/senegal/Senegal_rural_urban_updated.R) | Current residence report: prepare exact/fallback FP-provider supply, fit and verify, then render matching Rmd. |
| [`urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R`](../urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R) | Current residence report: prepare exact/fallback FP-provider supply, fit and verify, then render matching Rmd. |

## Current country modules

Run the R entry listed in the colleague guide. It orchestrates preparation,
analysis, modeling, verification, Python rendering and the matching Rmd.
The `administrative_area/analysis/` and `urban_rural/analysis/` modules are current.
The older `analysis/<country>/` helpers retain shared/historical definitions and
are not substitutes for the current report entry points. Current report entries
select the appropriate helper versions.

## All R scripts: dependencies and data contracts

### `R/install_dependencies.R`

- Role: Environment setup.
- Packages used directly: `renv`, `utils`.
- Inputs: DESCRIPTION and renv.lock; installed package versions for exact-match reuse.
- Outputs: Project R library and dependency-version checks.
- Flags referenced in code/help: `--file`, `--latest`.

### `R/rebuild_all.R`

- Role: Complete build.
- Packages used directly: `jsonlite`, `rmarkdown`, `utils`.
- Inputs: Intact authorized input ZIP plus versioned display assets and package lockfile.
- Outputs: Seven current reports, eight diagnostics, optional three historical reports, validation; private working data.
- Flags referenced in code/help: `--all-pairs`, `--base-html`, `--cache-dir`, `--data-dir`, `--facility-input`, `--file`, `--help`, `--html-output`, `--include-historical`, `--input`, `--input-zip`, `--join-dir`, `--lagos-dir`, `--lagos-women-file`, `--name`, `--output`, `--output-dir`, `--output-json`, `--pandoc-dir`, `--python`, `--rebuilt-root`, `--reference-root`, `--report-dir`, `--results-dir`, `--resume-after`, `--selection`, `--senegal-dir`, `--senegal-residence-lookup`, `--senegal-women-file`, `--sindh-need-file`, `--values`, `--verify`, `--work-dir`.

### `R/run_aggregate_reports.R`

- Role: Report batch entry.
- Packages used directly: `digest`, `jsonlite`, `rmarkdown`, `utils`.
- Inputs: Input ZIP, or a verified administrative join with matching extracted cache.
- Outputs: Selected country HTMLs and aggregate_build_manifest.json; private working inputs/models.
- Flags referenced in code/help: `--cache-dir`, `--countries`, `--data-dir`, `--file`, `--help`, `--html`, `--input-zip`, `--join-dir`, `--name`, `--output-dir`, `--pandoc-dir`, `--python`, `--work-dir`.

### `R/run_reports.R`

- Role: Historical/compatibility entry.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Authorized input ZIP; optional verified residence join/cache.
- Outputs: Historical/compatibility country reports; current Sindh district report.
- Flags referenced in code/help: `--cache-dir`, `--countries`, `--file`, `--help`, `--input-zip`, `--join-dir`, `--name`, `--output-dir`, `--pandoc-dir`, `--python`, `--work-dir`.

### `administrative_area/R/00_config.R`

- Role: Configuration.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Source-field definitions for the documented input snapshot; no credentials.
- Outputs: Country configuration objects used by the module entry.

### `administrative_area/R/04_join.R`

- Role: Internal join stage.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`.
- Inputs: D01-D12 plus dictionaries, checked lookups and upstream comparison exports.
- Outputs: Base joins, facility_linkage_inputs.rds, supply profiles and linkage audits.

### `administrative_area/R/05_validate.R`

- Role: Internal join verification.
- Packages used directly: `jsonlite`.
- Inputs: Base joins, upstream women/area/geography exports, both archetype versions and dictionary.
- Outputs: Reconciliation tables and checks, including population overlap and preserved source rows.

### `administrative_area/R/aggregate_supply.R`

- Role: Shared pipeline helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.

### `administrative_area/R/geography.R`

- Role: Shared pipeline helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.

### `administrative_area/R/helpers.R`

- Role: Shared pipeline helper.
- Packages used directly: `dplyr`, `haven`, `jsonlite`, `readr`.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.

### `administrative_area/R/run_join.R`

- Role: Join entry.
- Packages used directly: `digest`, `jsonlite`, `utils`.
- Inputs: All manifest-listed ZIP members; country source fields in 00_config.R.
- Outputs: Base woman joins, facility_linkage_inputs.rds, supply profiles, dictionaries and audits.
- Flags referenced in code/help: `--cache-dir`, `--country-config`, `--file`, `--geography-version`, `--help`, `--input-zip`, `--name`, `--output-dir`.

### `administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R`

- Role: Current report entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Original country women/facilities and shared inputs below, prepared through the join/cache.
- Outputs: Matching HTML via the existing Rmd; inputs/*_fp*.rds, calculation files, model results and checks.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--cache-dir`, `--cleaned-facility`, `--country`, `--data-dir`, `--dictionary`, `--district-codebook`, `--facility-input`, `--file`, `--help`, `--html`, `--input`, `--input-zip`, `--join-dir`, `--output`, `--output-dir`, `--pandoc-dir`, `--pca-score-file`, `--project-root`, `--python`, `--raw`, `--raw-facility`, `--raw-women`, `--score-method`, `--sensitivity-dir`, `--subprefecture-boundary`, `--work-dir`.

### `administrative_area/analysis/cotedivoire/audit_stock_indicator.R`

- Role: Diagnostic/helper.
- Packages used directly: `haven`, `jsonlite`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--facility-input`, `--raw-facility`.

### `administrative_area/analysis/cotedivoire/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `haven`, `jsonlite`, `survey`, `tidyselect`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--demand-sample`, `--district-codebook`, `--facility-input`, `--facility-sample`, `--file`, `--input`, `--output-dir`, `--raw`, `--raw-facility`, `--reference-dir`.

### `administrative_area/analysis/cotedivoire/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`.

### `administrative_area/analysis/cotedivoire/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `administrative_area/analysis/cotedivoire/plot_demand_score_distribution.R`

- Role: Diagnostic/helper.
- Packages used directly: `ggplot2`, `jsonlite`, `svglite`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--output-dir`.

### `administrative_area/analysis/cotedivoire/prepare_fp_provider_data.R`

- Role: FP-provider preparation.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`, `tools`.
- Inputs: Base checked woman join; facility_linkage_inputs.rds; raw country facilities/women; supply dictionary.
- Outputs: Reaggregated FP-provider whole-area profiles, *_women_manitoba_fp_providers.rds/.dta and audits.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--dictionary`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw-facility`, `--raw-women`.

### `administrative_area/analysis/cotedivoire/prepare_map.R`

- Role: Map stage.
- Packages used directly: `jsonlite`, `sf`, `tools`.
- Inputs: Saved report values and versioned boundary assets; explicit display-name aliases.
- Outputs: Report map geometry/metadata and display checks; no geographic reassignment.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--data-dir`, `--file`, `--subprefecture-boundary`.

### `administrative_area/analysis/cotedivoire/supply_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).

### `administrative_area/analysis/cotedivoire/verify_analysis.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cleaned-facility`, `--data-dir`, `--facility-input`, `--input`, `--raw`, `--raw-facility`.

### `administrative_area/analysis/lagos/Lagos_analysis_aggregate.R`

- Role: Current report entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Original country women/facilities and shared inputs below, prepared through the join/cache.
- Outputs: Matching HTML via the existing Rmd; inputs/*_fp*.rds, calculation files, model results and checks.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cache-dir`, `--cleaned-facility`, `--data-dir`, `--dictionary`, `--facility-input`, `--file`, `--help`, `--html`, `--input`, `--input-zip`, `--join-dir`, `--output`, `--output-dir`, `--pandoc-dir`, `--pca-score-file`, `--project-root`, `--python`, `--raw-facility`, `--raw-women`, `--reference-html`, `--score-method`, `--sensitivity-dir`, `--work-dir`.

### `administrative_area/analysis/lagos/audit_missingness.R`

- Role: Diagnostic/helper.
- Packages used directly: `haven`, `jsonlite`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--facility-input`, `--output-dir`, `--raw`, `--uom-rdata`.

### `administrative_area/analysis/lagos/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `haven`, `jsonlite`, `survey`, `tidyselect`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--demand-sample`, `--facility-input`, `--facility-sample`, `--file`, `--input`, `--output-dir`, `--psu-var`, `--raw-facility`, `--reference-dir`, `--strata-var`.

### `administrative_area/analysis/lagos/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--output-dir`.

### `administrative_area/analysis/lagos/export_facility_training.R`

- Role: Diagnostic/helper.
- Packages used directly: `haven`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--area`, `--facility-input`, `--output-dir`, `--raw`.

### `administrative_area/analysis/lagos/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `administrative_area/analysis/lagos/plot_regression_sensitivity.R`

- Role: Figure stage.
- Packages used directly: `ggplot2`, `jsonlite`, `svglite`.
- Inputs: Saved aggregate regression sensitivity results.
- Outputs: Coefficient-comparison figures; no model refitting.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`.

### `administrative_area/analysis/lagos/prepare_fp_provider_data.R`

- Role: FP-provider preparation.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`, `tools`.
- Inputs: Base checked woman join; facility_linkage_inputs.rds; raw country facilities/women; supply dictionary.
- Outputs: Reaggregated FP-provider whole-area profiles, *_women_manitoba_fp_providers.rds/.dta and audits.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--dictionary`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw-facility`, `--raw-women`.

### `administrative_area/analysis/lagos/regression_sensitivity.R`

- Role: Regression sensitivity.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Saved analysis_frame.rds, model sample/score references and specified country.
- Outputs: Included/excluded profiles, available-case and response-weighted model comparisons.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`.

### `administrative_area/analysis/lagos/supply_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).

### `administrative_area/analysis/lagos/verify_analysis.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`, `readr`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cleaned-facility`, `--data-dir`, `--facility-input`, `--input`, `--raw-facility`, `--raw-women`.

### `administrative_area/analysis/lagos/verify_regression_sensitivity.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `jsonlite`.
- Inputs: Main analysis frame plus saved sensitivity results.
- Outputs: Independent sensitivity verification.json and numerical checks.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--sensitivity-dir`.

### `administrative_area/analysis/plot_models.R`

- Role: Shared figure stage.
- Packages used directly: `ggplot2`, `grid`, `svglite`.
- Inputs: Saved aggregate model results and prediction tables.
- Outputs: Coefficient and adjusted-probability figures; no model refitting.

### `administrative_area/analysis/plot_regression_sensitivity.R`

- Role: Figure stage.
- Packages used directly: `ggplot2`, `jsonlite`, `svglite`.
- Inputs: Saved aggregate regression sensitivity results.
- Outputs: Coefficient-comparison figures; no model refitting.
- Flags referenced in code/help: `--data-dir`.

### `administrative_area/analysis/regression_sensitivity.R`

- Role: Regression sensitivity.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Saved analysis_frame.rds, model sample/score references and specified country.
- Outputs: Included/excluded profiles, available-case and response-weighted model comparisons.
- Flags referenced in code/help: `--country`, `--data-dir`, `--file`, `--output-dir`.

### `administrative_area/analysis/report_pipeline.R`

- Role: Shared pipeline helper.
- Packages used directly: `digest`, `jsonlite`, `utils`.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.
- Flags referenced in code/help: `--cache-dir`, `--geography-version`, `--input-zip`, `--join-dir`, `--output-dir`.

### `administrative_area/analysis/senegal/Senegal_analysis_aggregate.R`

- Role: Current report entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Original country women/facilities and shared inputs below, prepared through the join/cache.
- Outputs: Matching HTML via the existing Rmd; inputs/*_fp*.rds, calculation files, model results and checks.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--cache-dir`, `--cleaned-facility`, `--country`, `--data-dir`, `--department-lookup`, `--dictionary`, `--facility-input`, `--file`, `--help`, `--html`, `--input`, `--input-zip`, `--join-dir`, `--output`, `--output-dir`, `--pandoc-dir`, `--pca-score-file`, `--project-root`, `--python`, `--raw`, `--raw-facility`, `--raw-women`, `--score-method`, `--sensitivity-dir`, `--work-dir`.

### `administrative_area/analysis/senegal/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `haven`, `jsonlite`, `survey`, `tidyselect`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--demand-sample`, `--department-lookup`, `--facility-input`, `--facility-sample`, `--file`, `--input`, `--output-dir`, `--raw`, `--raw-facility`, `--reference-dir`.

### `administrative_area/analysis/senegal/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`.

### `administrative_area/analysis/senegal/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `administrative_area/analysis/senegal/prepare_fp_provider_data.R`

- Role: FP-provider preparation.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`, `tools`.
- Inputs: Base checked woman join; facility_linkage_inputs.rds; raw country facilities/women; supply dictionary.
- Outputs: Reaggregated FP-provider whole-area profiles, *_women_manitoba_fp_providers.rds/.dta and audits.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--dictionary`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw-facility`, `--raw-women`.

### `administrative_area/analysis/senegal/prepare_map.R`

- Role: Map stage.
- Packages used directly: `jsonlite`, `sf`, `tools`.
- Inputs: Saved report values and versioned boundary assets; explicit display-name aliases.
- Outputs: Report map geometry/metadata and display checks; no geographic reassignment.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--data-dir`, `--file`.

### `administrative_area/analysis/senegal/supply_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).

### `administrative_area/analysis/senegal/verify_analysis.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cleaned-facility`, `--data-dir`, `--facility-input`, `--input`, `--raw`, `--raw-facility`.

### `administrative_area/analysis/sindh/Sindh_analysis.R`

- Role: Current report entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Original country women/facilities and shared inputs below, prepared through the join/cache.
- Outputs: Matching HTML via the existing Rmd; inputs/*_fp*.rds, calculation files, model results and checks.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--cache-dir`, `--cleaned-facility`, `--country`, `--data-dir`, `--dictionary`, `--district-aliases`, `--facility-input`, `--file`, `--help`, `--html`, `--input`, `--input-zip`, `--join-dir`, `--need-file`, `--output`, `--output-dir`, `--pandoc-dir`, `--project-root`, `--python`, `--raw-facility`, `--raw-women`, `--sensitivity-dir`, `--work-dir`.

### `administrative_area/analysis/sindh/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `haven`, `jsonlite`, `survey`, `tidyselect`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--facility-input`, `--file`, `--input`, `--need-file`, `--output-dir`, `--raw-facility`.

### `administrative_area/analysis/sindh/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--score-method`.

### `administrative_area/analysis/sindh/prepare_fp_provider_data.R`

- Role: FP-provider preparation.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`, `tools`.
- Inputs: Base checked woman join; facility_linkage_inputs.rds; raw country facilities/women; supply dictionary.
- Outputs: Reaggregated FP-provider whole-area profiles, *_women_manitoba_fp_providers.rds/.dta and audits.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--dictionary`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw-facility`, `--raw-women`.

### `administrative_area/analysis/sindh/prepare_map.R`

- Role: Map stage.
- Packages used directly: `jsonlite`, `sf`.
- Inputs: Saved report values and versioned boundary assets; explicit display-name aliases.
- Outputs: Report map geometry/metadata and display checks; no geographic reassignment.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--data-dir`.

### `administrative_area/analysis/sindh/save_updated_dataset.R`

- Role: Diagnostic/helper.
- Packages used directly: `haven`, `jsonlite`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--input`, `--output-dir`.

### `administrative_area/analysis/sindh/supply_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).

### `administrative_area/analysis/sindh/training_reconstruction.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).

### `administrative_area/analysis/sindh/verify_analysis.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`, `readr`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cleaned-facility`, `--data-dir`, `--district-aliases`, `--facility-input`, `--input`, `--need-file`, `--raw-facility`, `--raw-women`.

### `administrative_area/analysis/verify_regression_sensitivity.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `jsonlite`.
- Inputs: Main analysis frame plus saved sensitivity results.
- Outputs: Independent sensitivity verification.json and numerical checks.
- Flags referenced in code/help: `--data-dir`, `--sensitivity-dir`.

### `analysis/Surgo_UoM_checks.R`

- Role: Coverage diagnostic entry.
- Packages used directly: `haven`, `jsonlite`, `readxl`, `rmarkdown`, `tidyselect`, `tools`.
- Inputs: Four FP-provider woman joins, four raw facility files, facility audit, D13 and D14.
- Outputs: Coverage tables/working checks and Surgo_UoM_checks.html through the matching Rmd.
- Country source context: D01, D02, D03, D04, D05, D06, D07, D08, D09, D10, D11, D12, D13, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cotedivoire-input`, `--cotedivoire-raw-facility`, `--facility-input`, `--file`, `--help`, `--lagos-facility-input`, `--lagos-input`, `--lagos-raw-facility`, `--output-dir`, `--pandoc-dir`, `--senegal-input`, `--senegal-raw-facility`, `--senegal-residence-lookup`, `--sindh-input`, `--sindh-need-file`, `--sindh-raw-facility`, `--work-dir`.

### `analysis/build_data_checks.R`

- Role: Cumulative checks calculation.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`.
- Inputs: Saved four-country analysis directories, facility audit and verified extracted input cache.
- Outputs: Calculated aggregate values JSON consumed by render_data_checks.py.
- Flags referenced in code/help: `--cache-dir`, `--cotedivoire-dir`, `--facility-input`, `--file`, `--help`, `--lagos-dir`, `--output-json`, `--senegal-dir`, `--sindh-dir`.

### `analysis/check_demand_population.R`

- Role: Diagnostic/helper.
- Packages used directly: `jsonlite`, `stats`, `tools`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`.

### `analysis/check_health_beliefs.R`

- Role: Diagnostic/helper.
- Packages used directly: `tools`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Flags referenced in code/help: `--file`, `--join-dir`, `--output-dir`, `--sindh-need-file`.

### `analysis/check_method_awareness.R`

- Role: Awareness diagnostic entry.
- Packages used directly: `clubSandwich`, `ggplot2`, `haven`, `jsonlite`, `rmarkdown`, `survey`, `tools`.
- Inputs: Saved Lagos/Senegal frames and corresponding prepared woman joins.
- Outputs: Awareness/confidence aggregate checks, models and Method_awareness_check_Senegal_Lagos.html.
- Flags referenced in code/help: `--file`, `--help`, `--lagos-dir`, `--lagos-women-file`, `--output-dir`, `--pandoc-dir`, `--report-dir`, `--senegal-dir`, `--senegal-women-file`.

### `analysis/check_need_definition_concordance.R`

- Role: Diagnostic/helper.
- Packages used directly: `haven`, `jsonlite`, `readr`, `tools`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Flags referenced in code/help: `--cotedivoire-data`, `--file`, `--help`, `--lagos-data`, `--output-dir`, `--senegal-data`, `--sindh-data`, `--sindh-need-file`.

### `analysis/common/pipeline.R`

- Role: Shared pipeline helper.
- Packages used directly: `digest`, `jsonlite`, `utils`.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.
- Flags referenced in code/help: `--cache-dir`, `--countries`, `--data-file`, `--embed-in-source`, `--geography-version`, `--input`, `--input-zip`, `--join-dir`, `--output`, `--output-dir`, `--python`, `--source-report`, `--work-dir`, `--workflow`.

### `analysis/common/plot_models.R`

- Role: Shared figure stage.
- Packages used directly: `ggplot2`, `grid`, `svglite`.
- Inputs: Saved aggregate model results and prediction tables.
- Outputs: Coefficient and adjusted-probability figures; no model refitting.

### `analysis/common/risk_scores.R`

- Role: Shared pipeline helper.
- Packages used directly: `jsonlite`.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.

### `analysis/common/survey_methods.R`

- Role: Shared pipeline helper.
- Packages used directly: `clubSandwich`, `survey`.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.

### `analysis/cotedivoire/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `haven`, `jsonlite`, `survey`, `tidyselect`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--admin-input`, `--district-codebook`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw`, `--raw-facility`.

### `analysis/cotedivoire/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--output-dir`.

### `analysis/cotedivoire/cdi_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).

### `analysis/cotedivoire/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `analysis/cotedivoire/prepare_map.R`

- Role: Map stage.
- Packages used directly: `jsonlite`, `sf`, `tools`.
- Inputs: Saved report values and versioned boundary assets; explicit display-name aliases.
- Outputs: Report map geometry/metadata and display checks; no geographic reassignment.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--data-dir`, `--file`, `--subprefecture-boundary`.

### `analysis/lagos/apply_ibeju_free_provision_exception.R`

- Role: Diagnostic/helper.
- Packages used directly: `haven`, `jsonlite`, `tools`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--input`, `--output-dir`.

### `analysis/lagos/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `jsonlite`, `survey`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--facility-input`, `--file`, `--input`, `--output-dir`, `--psu-var`, `--strata-var`.

### `analysis/lagos/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--input`, `--output-dir`.

### `analysis/lagos/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--demand-resolution`, `--file`, `--input`, `--missing-popular`, `--outcome`, `--output-dir`, `--pca-score-file`, `--score-method`, `--supply-resolution`.

### `analysis/lagos/lagos_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).

### `analysis/plot_area_score_distributions.R`

- Role: Diagnostic/helper.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Flags referenced in code/help: `--file`, `--output-dir`, `--reports-dir`.

### `analysis/plot_demand_pair_table.R`

- Role: Diagnostic figure entry.
- Packages used directly: `grDevices`, `grid`, `jsonlite`, `svglite`.
- Inputs: Saved score_profile_contrasts.rds aggregate profiles.
- Outputs: Demand-pair comparison figures and optional HTML.
- Flags referenced in code/help: `--all-pairs`, `--area-a`, `--area-b`, `--country`, `--help`, `--html-output`, `--input`, `--output-dir`, `--regional-comparisons`.

### `analysis/plot_final_score_sd_distribution.R`

- Role: Diagnostic/helper.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Flags referenced in code/help: `--file`, `--geography`, `--input-report`, `--output-dir`.

### `analysis/plot_score_profiles_combined.R`

- Role: Diagnostic/helper.
- Packages used directly: `ggplot2`, `grDevices`, `grid`, `svglite`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Flags referenced in code/help: `--case`, `--help`, `--input`, `--output-dir`, `--supply-percentages`, `--supply-score`, `--view`.

### `analysis/plot_supply_pair_table.R`

- Role: Diagnostic figure entry.
- Packages used directly: `grDevices`, `grid`, `jsonlite`, `svglite`.
- Inputs: Saved score_profile_contrasts.rds aggregate profiles.
- Outputs: Supply comparison figures and Composite score_comparisons.html with demand comparisons.
- Flags referenced in code/help: `--file`, `--help`, `--html-output`, `--input`, `--output-dir`.

### `analysis/score_profile_contrasts.R`

- Role: Profile diagnostic entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Saved main country analysis directories, including Sindh district results.
- Outputs: score_profile_contrasts.rds and aggregate profile comparison HTML.
- Flags referenced in code/help: `--cotedivoire-dir`, `--file`, `--help`, `--lagos-dir`, `--output-dir`, `--pandoc-dir`, `--senegal-dir`, `--sindh-dir`, `--work-dir`.

### `analysis/score_profile_examples.R`

- Role: Profile diagnostic entry.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `rmarkdown`, `svglite`, `tools`.
- Inputs: Saved Lagos, Senegal and Côte d’Ivoire analysis directories.
- Outputs: Score_profile_examples.html plus aggregate comparison tables/figures.
- Flags referenced in code/help: `--cotedivoire-dir`, `--file`, `--help`, `--lagos-dir`, `--output-dir`, `--pandoc-dir`, `--senegal-dir`, `--work-dir`.

### `analysis/senegal/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `haven`, `jsonlite`, `readxl`, `survey`, `tidyselect`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--admin-input`, `--department-lookup`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw`, `--raw-facility`, `--residence-lookup`.

### `analysis/senegal/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--output-dir`.

### `analysis/senegal/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `analysis/senegal/prepare_map.R`

- Role: Map stage.
- Packages used directly: `jsonlite`, `sf`, `tools`.
- Inputs: Saved report values and versioned boundary assets; explicit display-name aliases.
- Outputs: Report map geometry/metadata and display checks; no geographic reassignment.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--data-dir`, `--file`.

### `analysis/senegal/senegal_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D02, D06, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).

### `analysis/sindh/build_analysis.R`

- Role: Descriptive/scoring stage.
- Packages used directly: `jsonlite`, `survey`, `tools`.
- Inputs: Prepared woman join, facility audit and raw fields supplied by the calling country entry.
- Outputs: analysis_frame.rds, report_data.json, analysis_audit.json and aggregate calculation tables.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--facility-input`, `--file`, `--input`, `--need-file`, `--output-dir`.

### `analysis/sindh/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--output-dir`.

### `analysis/sindh/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `analysis/sindh/prepare_map.R`

- Role: Map stage.
- Packages used directly: `jsonlite`, `sf`.
- Inputs: Saved report values and versioned boundary assets; explicit display-name aliases.
- Outputs: Report map geometry/metadata and display checks; no geographic reassignment.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--data-dir`.

### `analysis/sindh/sindh_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D04, D08, D09, D10, D11, D12, D14 (selected through the pipeline; not necessarily read in this helper).

### `analysis/tabulate_women_residence.R`

- Role: Diagnostic/helper.
- Packages used directly: `haven`, `tools`.
- Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
- Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
- Flags referenced in code/help: `--consent-code`, `--consent-var`, `--file`, `--geo-var`, `--help`, `--input`, `--output-file`, `--residence-var`, `--subset-code`, `--subset-var`, `--weight-var`.

### `tests/compare_analysis_update.R`

- Role: Test/validation utility.
- Packages used directly: `jsonlite`.
- Inputs: Test fixtures or caller-supplied earlier/current saved results, as specified in the code.
- Outputs: Assertions, validation records or aggregate before/after comparisons.
- Flags referenced in code/help: `--current-dir`, `--label`, `--output`, `--previous-dir`.

### `tests/compare_sindh_training_update.R`

- Role: Test/validation utility.
- Packages used directly: `jsonlite`.
- Inputs: Test fixtures or caller-supplied earlier/current saved results, as specified in the code.
- Outputs: Assertions, validation records or aggregate before/after comparisons.
- Flags referenced in code/help: `--current-dir`, `--output`, `--previous-dir`.

### `tests/test_sindh_training.R`

- Role: Test/validation utility.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Test fixtures or caller-supplied earlier/current saved results, as specified in the code.
- Outputs: Assertions, validation records or aggregate before/after comparisons.
- Flags referenced in code/help: `--file`.

### `tests/test_survey_methods.R`

- Role: Test/validation utility.
- Packages used directly: `clubSandwich`.
- Inputs: Test fixtures or caller-supplied earlier/current saved results, as specified in the code.
- Outputs: Assertions, validation records or aggregate before/after comparisons.
- Flags referenced in code/help: `--file`.

### `tests/verify_cotedivoire.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Flags referenced in code/help: `--admin-input`, `--data-dir`, `--facility-input`, `--input`, `--raw`.

### `tests/verify_lagos.R`

- Role: Verification.
- Packages used directly: `jsonlite`, `tools`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Flags referenced in code/help: `--data-dir`, `--facility-input`, `--model-dir`.

### `tests/verify_senegal.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Flags referenced in code/help: `--admin-input`, `--data-dir`, `--facility-input`, `--input`, `--raw`.

### `tests/verify_sindh.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Flags referenced in code/help: `--data-dir`, `--input`, `--need-file`, `--raw`.

### `urban_rural/R/00_config.R`

- Role: Configuration.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Source-field definitions for the documented input snapshot; no credentials.
- Outputs: Country configuration objects used by the module entry.

### `urban_rural/R/06_join_urban_rural.R`

- Role: Internal residence join stage.
- Packages used directly: `digest`, `haven`, `jsonlite`, `readxl`, `tidyselect`.
- Inputs: Administrative facility audit, original women, residence configuration and D13.
- Outputs: Base exact area/residence joins, supply profiles and coverage/issue audits.

### `urban_rural/R/run_join_urban_rural.R`

- Role: Join entry.
- Packages used directly: `jsonlite`.
- Inputs: Full input ZIP; residence configuration; optional validated Sindh residence lookup.
- Outputs: Base area/residence joins, admin_reference, exact profiles, coding and coverage audits.
- Flags referenced in code/help: `--administrative-module-dir`, `--cache-dir`, `--country-config`, `--file`, `--geography-version`, `--help`, `--input-zip`, `--name`, `--output-dir`, `--sindh-residence-lookup`, `--urban-rural-config`.

### `urban_rural/R/urban_rural_config.R`

- Role: Configuration.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Source-field definitions for the documented input snapshot; no credentials.
- Outputs: Country configuration objects used by the module entry.

### `urban_rural/R/urban_rural_helpers.R`

- Role: Shared pipeline helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
- Outputs: Shared helper functions/results; used through documented entry points.

### `urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R`

- Role: Current report entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Original country women/facilities and shared inputs below, prepared through the join/cache.
- Outputs: Matching HTML via the existing Rmd; inputs/*_fp*.rds, calculation files, model results and checks.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--cache-dir`, `--cleaned-facility`, `--data-dir`, `--dictionary`, `--district-codebook`, `--facility-input`, `--file`, `--help`, `--html`, `--input`, `--input-zip`, `--join-dir`, `--output`, `--output-dir`, `--pandoc-dir`, `--pca-score-file`, `--python`, `--raw`, `--raw-facility`, `--raw-women`, `--score-method`, `--subprefecture-boundary`, `--work-dir`.

### `urban_rural/analysis/cotedivoire/assign_residence_supply.R`

- Role: Residence assignment.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`, `tools`.
- Inputs: Base residence woman join; raw facilities; facility audit, dictionary and prepared whole-area frame.
- Outputs: Exact/fallback profiles, *_women_manitoba_rural_urban_fp.rds, revised analysis frame/report values.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--dictionary`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw-facility`.

### `urban_rural/analysis/cotedivoire/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`.

### `urban_rural/analysis/cotedivoire/check_classification_changes.R`

- Role: Optional historical diagnostic.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`.
- Inputs: Earlier saved country analysis and current inputs; earlier snapshot supplied separately.
- Outputs: Change-attribution tables for the specified historical comparison.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--facility-input`, `--file`, `--friday-data-dir`, `--output-dir`, `--raw-facility`, `--updated-data-dir`.

### `urban_rural/analysis/cotedivoire/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `urban_rural/analysis/cotedivoire/supply_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).

### `urban_rural/analysis/cotedivoire/update_data_checks.R`

- Role: Coverage HTML updater.
- Packages used directly: `jsonlite`, `tools`.
- Inputs: Existing Data_checks.html and saved country residence analysis frame.
- Outputs: HTML with that country fallback/coverage section appended or replaced.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--audit-json`, `--base-html`, `--data-dir`, `--file`, `--output`.

### `urban_rural/analysis/cotedivoire/verify_analysis.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Country source context: D03, D07, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cleaned-facility`, `--data-dir`, `--facility-input`, `--input`, `--raw`, `--raw-facility`.

### `urban_rural/analysis/lagos/Lagos_rural_urban_updated.R`

- Role: Current report entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Original country women/facilities and shared inputs below, prepared through the join/cache.
- Outputs: Matching HTML via the existing Rmd; inputs/*_fp*.rds, calculation files, model results and checks.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cache-dir`, `--cleaned-facility`, `--data-dir`, `--dictionary`, `--facility-input`, `--file`, `--help`, `--html`, `--input`, `--input-zip`, `--join-dir`, `--output`, `--output-dir`, `--pandoc-dir`, `--pca-score-file`, `--python`, `--raw-facility`, `--raw-women`, `--reference-html`, `--score-method`, `--work-dir`.

### `urban_rural/analysis/lagos/assign_residence_supply.R`

- Role: Residence assignment.
- Packages used directly: `haven`, `jsonlite`, `tidyselect`, `tools`.
- Inputs: Base residence woman join; raw facilities; facility audit, dictionary and prepared whole-area frame.
- Outputs: Exact/fallback profiles, *_women_manitoba_rural_urban_fp.rds, revised analysis frame/report values.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--dictionary`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw-facility`.

### `urban_rural/analysis/lagos/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--output-dir`.

### `urban_rural/analysis/lagos/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `urban_rural/analysis/lagos/verify_analysis.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`, `readr`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Country source context: D01, D05, D09, D10, D11, D12 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cleaned-facility`, `--data-dir`, `--facility-input`, `--input`, `--raw-facility`, `--raw-women`.

### `urban_rural/analysis/senegal/Senegal_rural_urban_updated.R`

- Role: Current report entry.
- Packages used directly: `jsonlite`, `rmarkdown`.
- Inputs: Original country women/facilities and shared inputs below, prepared through the join/cache.
- Outputs: Matching HTML via the existing Rmd; inputs/*_fp*.rds, calculation files, model results and checks.
- Country source context: D02, D06, D09, D10, D11, D12, D13 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--boundary`, `--cache-dir`, `--cleaned-facility`, `--data-dir`, `--department-lookup`, `--dictionary`, `--facility-input`, `--file`, `--help`, `--html`, `--input`, `--input-zip`, `--join-dir`, `--output`, `--output-dir`, `--pandoc-dir`, `--pca-score-file`, `--python`, `--raw`, `--raw-facility`, `--raw-women`, `--residence-lookup`, `--score-method`, `--work-dir`.

### `urban_rural/analysis/senegal/assign_residence_supply.R`

- Role: Residence assignment.
- Packages used directly: `haven`, `jsonlite`, `readxl`, `tidyselect`, `tools`.
- Inputs: Base residence woman join; raw facilities; facility audit, dictionary and prepared whole-area frame.
- Outputs: Exact/fallback profiles, *_women_manitoba_rural_urban_fp.rds, revised analysis frame/report values.
- Country source context: D02, D06, D09, D10, D11, D12, D13 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--dictionary`, `--facility-input`, `--file`, `--input`, `--output-dir`, `--raw-facility`, `--residence-lookup`.

### `urban_rural/analysis/senegal/build_pca_scores.R`

- Role: PCA sensitivity stage.
- Packages used directly: `ggplot2`, `grid`, `jsonlite`, `svglite`, `tools`.
- Inputs: Saved analysis_frame.rds with personal demand and assigned facility supply.
- Outputs: Exploratory PCA scores, loadings, diagnostic tables and figures.
- Country source context: D02, D06, D09, D10, D11, D12, D13 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`.

### `urban_rural/analysis/senegal/fit_models.R`

- Role: Model stage.
- Packages used directly: `clubSandwich`, `jsonlite`, `tools`.
- Inputs: Prepared analysis_frame.rds; optional saved PCA scores; runtime data/output directories.
- Outputs: Weighted modern-use models, score references, model_results.json and model_audit.json.
- Country source context: D02, D06, D09, D10, D11, D12, D13 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--data-dir`, `--file`, `--output-dir`, `--pca-score-file`, `--score-method`.

### `urban_rural/analysis/senegal/supply_definitions.R`

- Role: Country definitions helper.
- Packages used directly: bundled R; caller dependencies may apply.
- Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
- Outputs: Recoded values and definition/helper functions for the caller.
- Country source context: D02, D06, D09, D10, D11, D12, D13 (selected through the pipeline; not necessarily read in this helper).

### `urban_rural/analysis/senegal/update_data_checks.R`

- Role: Coverage HTML updater.
- Packages used directly: `jsonlite`, `tools`.
- Inputs: Existing Data_checks.html and saved country residence analysis frame.
- Outputs: HTML with that country fallback/coverage section appended or replaced.
- Country source context: D02, D06, D09, D10, D11, D12, D13 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--audit-json`, `--base-html`, `--data-dir`, `--file`, `--output`.

### `urban_rural/analysis/senegal/verify_analysis.R`

- Role: Verification.
- Packages used directly: `clubSandwich`, `haven`, `jsonlite`, `readxl`.
- Inputs: Caller-supplied source/analysis files and saved report/model results.
- Outputs: Independent verification checks; no change to analytical source inputs.
- Country source context: D02, D06, D09, D10, D11, D12, D13 (selected through the pipeline; not necessarily read in this helper).
- Flags referenced in code/help: `--cleaned-facility`, `--data-dir`, `--facility-input`, `--input`, `--raw`, `--raw-facility`, `--residence-lookup`.

### `urban_rural/tests/test_urban_rural.R`

- Role: Test/validation utility.
- Packages used directly: `haven`.
- Inputs: Test fixtures or caller-supplied earlier/current saved results, as specified in the code.
- Outputs: Assertions, validation records or aggregate before/after comparisons.
- Flags referenced in code/help: `--file`.
