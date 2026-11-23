# Inputs, scripts and outputs: colleague run guide

Use the complete repository together with the authorized input bundle. The country
R files call shared helpers and matching R Markdown templates; copying only one R
file is insufficient. No input survey data are distributed in this repository.

## What to obtain

1. The complete repository, including `R/`, both geographic modules, `analysis/`,
   `tests/`, `docs/`, `DESCRIPTION`, `renv.lock` and `analysis/assets/`.
2. `SURGO_Manitoba_linkage_inputs_complete.zip`, obtained separately through the
   project's authorized data-sharing arrangements. Keep it intact: **55 listed
   source/reference files plus `bundle_manifest.json`, or 56 members in total**.
3. R and the recorded packages, Python 3 and Pandoc, as described below.

There are **14 primary analytical inputs** in the following list. This is a useful
reading list, not a smaller replacement ZIP: the current runner also requires
supporting dictionaries, lookups and reconciliation inputs, and verifies every
manifest-listed file. Do not assemble a 14-file ZIP or flatten the folder structure.

## The 14 primary analytical inputs

Names below are exact, case-sensitive paths **inside the input ZIP**.

| ID | Exact ZIP member | What it supplies |
|---|---|---|
| D01 | `inputs/surgo/lagos/surgo_lagos_women.csv` | Original Lagos main sample: women, responses, LGA, residence and final weights. |
| D02 | `inputs/surgo/senegal/surgo_senegal_women.dta` | Original Senegal women, responses, département, residence and final weights. |
| D03 | `inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta` | Original Côte d’Ivoire women, responses, geography, residence and final weights. |
| D04 | `inputs/surgo/sindh/20250124_forms_wt.dta` | Selected weighted Sindh women file. Do not substitute `20250124_forms.dta`. |
| D05 | `inputs/uom/lagos/uom_lagos_facility_pooled.dta` | Detailed Lagos facilities: FP eligibility, identifiers, geography/residence and service frequency. |
| D06 | `inputs/uom/senegal/uom_senegal_facility_pooled.dta` | Detailed Senegal facilities: FP eligibility, identifiers, health district and service frequency. |
| D07 | `inputs/uom/cdi/uom_cdi_facility_pooled.dta` | Detailed Côte d’Ivoire facilities: FP eligibility, identifiers, health district, residence and service frequency. |
| D08 | `inputs/uom/sindh/uom_sindh_facility_pooled.dta` | Detailed Sindh facilities: FP eligibility, named district, service frequency and both Q406 training encodings. |
| D09 | `uom_pipeline/uom_summary_facility.csv` | Supplied cleaned facility indicators, including offering, free products, training and stock. Sindh training is corrected from D08 in analysis preparation. |
| D10 | `uom_pipeline/Facilities data with archetype assignments.csv` | Selected facility archetypes and supporting facility descriptors; joined on `loc` and `fid`. |
| D11 | `uom_pipeline/new_demand_vol_facility.csv` | Facility service-volume measures retained in the broad supply join. |
| D12 | `exports/uom_surgo_geo_crosswalk_reviewed.csv` | Reviewed upstream geographic crosswalk, reconstructed for reconciliation alongside separately checked assignments. |
| D13 | `inputs/SenegalURlookup.xlsx` | Senegal facility Rural/Urban classification by health district; a proxy, used for the residence workflow. |
| D14 | `surgo_pipeline/sindh/sindh_wra_design_ready.rds` | Supplied Sindh `fpn2_derived` by `col_id`; independently checked against the retained questionnaire rule. Not the original women file. |

The similarly named `curated/Facilities data with archetype assignments.csv` is a
different reconciliation input. Likewise, `surgo_pipeline/sindh/wra_design_ready.rds`
is an upstream comparison file, not D14. Folder names distinguish these inputs.

## Supporting files that must also remain in the ZIP

These are additional to the 14 above. They are already in the complete bundle.

| Group | Exact ZIP members | Use |
|---|---|---|
| Supply dictionary | `exports/merged_supply_demand_dictionary.csv` | Names, types and aggregation rules for the broad supply join. |
| Checked geography | `lookups/senegal_health_district_department_evidence.csv`; `lookups/cdi_health_district_codebook.csv`; `lookups/sindh_district_aliases.csv`; `lookups/geographic_sources.json` | Checked health-district/administrative-area assignments, code labels, explicit aliases and provenance. |
| Upstream geographic reconciliation | `merge_pipeline/uom_facility_geo.csv`; `merge_pipeline/uom_summary_facility_crosswalked.csv` | Verify reconstructed source geography and reviewed-crosswalk assignments. |
| Upstream area reconciliation | `exports/merged_supply_demand_department.csv`; `exports/merged_supply_demand_coverage.csv` | Verify original area summaries and population overlap; not replacements for individual women. |
| Archetype reconciliation | `curated/Facilities data with archetype assignments.csv` | Compare the independently supplied archetype version with D10. |
| Processed-women reconciliation | `surgo_pipeline/lagos/wra_design_ready.rds`; `surgo_pipeline/senegal/wra_design_ready.rds`; `surgo_pipeline/cotedivoire/wra_design_ready.rds`; `surgo_pipeline/sindh/wra_design_ready.rds` | Compare upstream geographic populations and joins. |
| Integrity and remaining references | `bundle_manifest.json`, `lookups/senegal_health_district_department.csv` and all other members in the full inventory | Manifest verification, auxiliary lookup, upstream code, archived exports and provenance. |

**Where datasets are documented:**

- [Input ZIP filenames](DATASET_FILENAMES.md): simple exact-name list for linking to source systems later.
- This guide: names, roles, main scripts, separate execution and expected outputs.
- [Complete ZIP inventory](INPUTS.md): every member, its purpose, byte size and SHA-256.
- [Input manifest](input_bundle_manifest.json): machine-readable integrity inventory.
- [Dataset catalog](dataset_catalog.json): primary-input IDs, exact members, keys,
  scope and pending Databricks mapping fields.
- [Script reference](SCRIPT_REFERENCE.md): direct R package dependencies and input
  contracts for each R script. The same information is summarized in script headers.
- Country READMEs: measure definitions, denominators, model samples and limitations.

## Databricks mapping

Databricks catalog/schema/table names and source snapshot identifiers have **not
yet been supplied**. The dataset catalog records them as pending. Do not infer
table names from filenames or describe an existing Databricks connection as tested.

For each proposed replacement, the data owner needs to identify the catalog,
schema, table or authorized file export, snapshot/version, row identifier, field
mapping, labels and special-response codes. An Excel lookup or supplied derived
RDS may need a maintained file asset rather than a table. All file formats need
their original coding and types, not just similarly named columns. Preserve the
approved need definition, final weights and geography keys. A new export changes
file checksums and requires a reviewed manifest update and validation; the current
pipeline expects the documented file bundle. Databricks ingestion is not implemented
by this documentation update. Keep credentials in runtime configuration.

## Software setup

Run commands from the repository root. The reference environment is **R 4.6.0**,
the versions in `renv.lock`, Python 3.9 or later and Pandoc (reference 3.8.3).

```sh
Rscript R/install_dependencies.R
```

This restores packages once. Country scripts use namespaced functions and the
project library; they do not reinstall packages before each analysis. `DESCRIPTION`
declares the 15 direct project packages: `base64enc`, `haven`, `dplyr`, `readr`,
`readxl`, `jsonlite`, `digest`, `sf`, `survey`, `clubSandwich`, `ggplot2`, `svglite`,
`tidyselect`, `rmarkdown` and `knitr`. `renv` restores dependencies. Bundled R
packages such as `stats`, `utils`, `tools`, `grid`, `graphics` and `grDevices` do not
need separate installation. Python renderers use the standard library. `sf` may
need system libraries or a compatible binary package; Pandoc is an external program.
See [environment requirements](../REPRODUCIBILITY.md#requirements-and-inputs).

Set `SURGO_MANITOBA_INPUT_ZIP` in the running shell to the authorized ZIP's location.
Do not write that location or credentials into project files. Every command below
reads this runtime variable; `--input-zip` also accepts a caller-supplied location.
If necessary, set `RSTUDIO_PANDOC` or supply `--pandoc-dir` at runtime.

For example, in a POSIX shell, enter the ZIP location at this prompt (without
surrounding quotation marks):

```sh
printf 'Authorized input ZIP location: '
IFS= read -r SURGO_MANITOBA_INPUT_ZIP
export SURGO_MANITOBA_INPUT_ZIP
```

## Option A: reproduce everything in one run

```sh
Rscript R/rebuild_all.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP" --include-historical false
```

This builds **seven current country reports and eight supplementary outputs**,
verifies their registered results and writes the HTMLs under
`output/reproduction/rebuilt/`. It preserves the published references in `reports/`
and `data_checks/`. Working joins, scores and fitted models go under
`output/reproduction/work/`; the verified input cache is `data/reproduction_cache/`.
It does not need pre-existing joins or analysis outputs.

The runner defaults to including three historical reports. Omit
`--include-historical false` only if those earlier definitions are wanted too;
their pinned source is downloaded separately. They are not the current FP-provider
reports. The full expected output list and comparison rules are in
[REPRODUCIBILITY.md](../REPRODUCIBILITY.md#current-output-inventory).

## Option B: run joins and country reports separately

These commands use a separate `output/colleague/` tree so they preserve the
published HTMLs. Run the following steps sequentially: concurrent extraction into
one cache is not supported. The original women and all survey columns are retained
in joins; the restriction to women in need occurs in the analyses.

### 1. Administrative-area join only

```sh
Rscript administrative_area/R/run_join.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" --geography-version both \
  --output-dir ../output/colleague/joins/administrative_area \
  --cache-dir ../data/colleague_cache
```

The join entry resolves relative output/cache arguments from **its module folder**,
hence `../` in this command. Results are under
`output/colleague/joins/administrative_area/`: `checked_geography/` contains each
`<country>_women_manitoba.rds` and `.dta`; `audit/facility_linkage_inputs.rds`
contains facility records with reconstructed geography. `supply_profiles/`,
`dictionaries/`, `audit/` and `run_parameters.json` document the join and reference
reconciliation. Country keys are `lagos`, `senegal`, `cotedivoire` and `sindh`.

**This base join preserves upstream supply definitions for reconciliation.** The
next analysis stage reaggregates confirmed FP providers. Use the resulting
`*_fp_providers.rds` for the current analysis, rather than interpreting the base
join as already FP-restricted.

### 2. Rural/Urban join only

```sh
Rscript urban_rural/R/run_join_urban_rural.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" --geography-version both \
  --output-dir ../output/colleague/joins/urban_rural \
  --cache-dir ../data/colleague_cache
```

This independently builds its own `admin_reference/`. Results are under
`output/colleague/joins/urban_rural/`, including
`checked_geography/<country>_women_manitoba_urban_rural.rds` and `.dta`,
residence-specific supply profiles and linkage audits. The base module flags
missing exact cells. The **country analysis preparation** applies the FP-provider
restriction and the flagged same-area fallback for cells with no FP providers.
Sindh has no documented woman Rural/Urban field: its residence join remains
flagged unavailable and is not used to create a Sindh residence report.

### 3. Four administrative-area reports, each independently

These commands reuse the administrative-area join from Step 1. Every command also
checks its match to the input ZIP. If skipping Step 1, omit `--join-dir` and that
country entry will build the required join automatically. Report entry points
resolve their relative paths from the **repository root**.

**Lagos**

```sh
Rscript administrative_area/analysis/lagos/Lagos_analysis_aggregate.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --join-dir output/colleague/joins/administrative_area --cache-dir data/colleague_cache \
  --work-dir output/colleague/aggregate/lagos --output-dir output/colleague/reports
```

**Senegal**

```sh
Rscript administrative_area/analysis/senegal/Senegal_analysis_aggregate.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --join-dir output/colleague/joins/administrative_area --cache-dir data/colleague_cache \
  --work-dir output/colleague/aggregate/senegal --output-dir output/colleague/reports
```

**Côte d’Ivoire**

```sh
Rscript administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --join-dir output/colleague/joins/administrative_area --cache-dir data/colleague_cache \
  --work-dir output/colleague/aggregate/cotedivoire --output-dir output/colleague/reports
```

**Sindh**

```sh
Rscript administrative_area/analysis/sindh/Sindh_analysis.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --join-dir output/colleague/joins/administrative_area --cache-dir data/colleague_cache \
  --work-dir output/colleague/aggregate/sindh --output-dir output/colleague/reports
```

Alternatively, run all four through `R/run_aggregate_reports.R`, with the same
input, join and cache arguments, `--work-dir output/colleague/aggregate` and
`--output-dir output/colleague/reports`. Use `--countries` to select a comma-separated
subset of `lagos,senegal,cotedivoire,sindh`.

### 4. Three Rural/Urban reports, each independently

These commands reuse Step 2. Omit `--join-dir` to let each entry prepare the
residence join itself. The heatmaps and area classifications retain whole-area
planning summaries; individual models receive their residence-specific or flagged
fallback supply profiles.

```sh
Rscript urban_rural/analysis/lagos/Lagos_rural_urban_updated.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --join-dir output/colleague/joins/urban_rural --cache-dir data/colleague_cache \
  --work-dir output/colleague/residence/lagos --output-dir output/colleague/reports
```

```sh
Rscript urban_rural/analysis/senegal/Senegal_rural_urban_updated.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --join-dir output/colleague/joins/urban_rural --cache-dir data/colleague_cache \
  --work-dir output/colleague/residence/senegal --output-dir output/colleague/reports
```

```sh
Rscript urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --join-dir output/colleague/joins/urban_rural --cache-dir data/colleague_cache \
  --work-dir output/colleague/residence/cotedivoire --output-dir output/colleague/reports
```

### 5. Supplementary coverage and score comparisons

Use Option A for the cumulative diagnostics as well as the seven country reports.
It runs `analysis/Surgo_UoM_checks.R` for coverage and
`analysis/plot_supply_pair_table.R` with `analysis/plot_demand_pair_table.R` for
component comparisons, passing the correct generated inputs. These diagnostics
read prepared analysis frames and joins, not just the original surveys. For
individual diagnostic commands and their input contracts, see
[analysis/README.md](../analysis/README.md#similar-totals-different-component-profiles).
The main country commands do not rebuild these separate diagnostic HTMLs.

## Main files and expected outputs

Every main R entry has a matching `.Rmd` in the same directory. Run the R entry
to execute preparation, calculations, models, verification and rendering in order;
it calls the Rmd automatically. Do not run every helper or template separately.

| Analysis | Main R entry | HTML produced in the selected report directory |
|---|---|---|
| Lagos, whole LGA | `administrative_area/analysis/lagos/Lagos_analysis_aggregate.R` | `Lagos_analysis_aggregate.html` |
| Senegal, whole département | `administrative_area/analysis/senegal/Senegal_analysis_aggregate.R` | `Senegal_analysis_aggregate.html` |
| Côte d’Ivoire, whole area | `administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R` | `Cote_dIvoire_analysis_aggregate.html` |
| Sindh, whole district | `administrative_area/analysis/sindh/Sindh_analysis.R` | `Sindh_analysis.html` |
| Lagos, LGA–residence assignment | `urban_rural/analysis/lagos/Lagos_rural_urban_updated.R` | `Lagos_rural_urban_updated.html` |
| Senegal, département–residence assignment | `urban_rural/analysis/senegal/Senegal_rural_urban_updated.R` | `Senegal_rural_urban_updated.html` |
| Côte d’Ivoire, area–residence assignment | `urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R` | `CotedIvoire_rural_urban_updated.html` |
| All current reports and diagnostics | `R/rebuild_all.R` | Seven above, plus eight supplementary outputs and reproduction validation. |

Published coverage and comparison reports are `data_checks/Surgo_UoM_checks.html`
and `data_checks/Composite score_comparisons.html`. Option A writes rebuilt copies
under its `data_checks/` folder; Option B writes the selected country HTMLs only.
Earlier `Lagos_analysis.html`, `Senegal_analysis.html` and
`Cote_dIvoire_analysis.html` are historical outputs, not current entry points.

For each country, the working directory contains:

| Working resource | Meaning |
|---|---|
| `inputs/<country>_women_manitoba_fp_providers.rds` (and `.dta`) | Analysis-specific whole-area FP-provider join; all original women retained. |
| `inputs/<country>_women_manitoba_rural_urban_fp.rds` | Residence analysis join, with exact/fallback flags; three countries only. |
| `<report_basename>_files/analysis_frame.rds` | Prepared analysis objects; contains restricted woman-level data. |
| `<report_basename>_files/report_data.json` | Aggregate report values, geographic scores, labels and classifications. |
| `<report_basename>_files/models_zscore/` | Main model results, score references, model audit and fitted objects. |
| `<report_basename>_files/checks/verification.json` | Independent numerical verification status. |
| `<report_basename>_files/pca_appendix/` | PCA sensitivity where implemented; absent from current Sindh analysis. |
| `<report_basename>_files/regression_sensitivity/` | Regression-completeness checks in the four administrative-area reports. |

Not every optional output exists for every workflow. Full rebuild output and work
directories are separate. Source datasets, caches, generated joins, private audit
tables and fitted objects stay out of uploads; share authorized input data through
the agreed data channel. Upload code, documentation and the intended aggregate
HTMLs, retaining the repository structure.

## How the calculation stages connect

1. Join raw and cleaned facility records using validated country-specific IDs;
   attach reviewed and checked geography, archetypes and service volumes. Preserve
   the upstream-parity join for comparison.
2. For current reports, select confirmed FP providers: Lagos/Sindh `q506 = 1`,
   Senegal `s5_6 = 1`, Côte d’Ivoire `s2_5_sp = 1`. Calculate each supply mean from
   eligible facilities with valid data for that measure, retaining genuine zeros.
   Facility summaries are unweighted; undefined ratios stay undefined.
3. Assign whole-area or matching area–residence means to each woman. For an exact
   residence cell with no eligible FP providers, use a flagged same-area profile
   if available. An undefined component in an existing cell does not trigger an
   automatic fallback; an area with no profile remains unmatched.
4. Coverage uses all women. Subsequent descriptive and modeling sections use women
   in need: original SURGO `fpn2/FPN2 = 1`, or the approved Sindh `fpn2_derived`.
   Weighted SURGO estimates use `survey` helpers. Original weight fields are
   `weight` (Lagos), `WEIGHT` (Senegal/CDI), `Normalizedweights` (Sindh), copied to
   internal `weight`. The design is `ids = ~1`, with no strata or FPC.
5. Construct risk-aligned scores, with both demand and supply higher = more
   challenges. Geographic scores average six component z-scores using equal-area
   reference means/SDs; primary flags are above zero. Individual models retain
   personal demand responses and the woman's assigned supply profile.
6. Fit modern-use models with `survey::svyglm()` and separate administrative-area
   CR2/Satterthwaite inference. Run numerical and HTML checks, then render the
   matching Rmd. Shared implementation is `analysis/common/survey_methods.R`.

See [survey/scoring methods](SURVEY_AND_SCORING.md) and country READMEs for full
definitions and the limits of the available survey-design information.

## Check success and diagnose a failed run

The runners stop on failed stages. Read the first failing stage and its numerical
verification record; an HTML file alone is not proof that the whole run completed.
Option A also writes `reproduction_validation.json` and compares results against
the registered publication. A valid-source run can still fail because the R/package
versions, Pandoc or system libraries are wrong; fix the environment before changing
definitions or checksum references.

Source/report release checks, run from the repository root:

```sh
python3 tests/check_aggregate_release.py
python3 tests/check_rural_urban_release.py
```

These release checks inspect the repository; they do not replace recomputing the
analysis from authorized inputs. Historical or optional diagnostics needing a
separate earlier saved build identify that requirement in their own guide.
