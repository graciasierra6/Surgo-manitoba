# Reproduction guide

## Software and data

Use the full repository and the authorized complete ZIP, obtained separately.
Names are in [DATASET_FILENAMES.md](DATASET_FILENAMES.md), with purposes and SHA-256
checksums in [INPUTS.md](INPUTS.md). Keep all 56 members with their internal folders.
Databricks source links are pending; the current runner reads the verified ZIP.

The reference environment is R 4.6.0 and the versions in `renv.lock`, Python 3.9+
and Pandoc (reference 3.8.3). Restore R dependencies once from the repository root:

```sh
Rscript setup.R
```

`DESCRIPTION` declares project packages; the seven country R files list their
packages and data contracts at the top. `renv.lock` pins transitive versions.
`sf` may require compatible spatial system libraries/compiler tools. Pandoc is an
external program; supply `--pandoc-dir` or `RSTUDIO_PANDOC` when needed. Package
installation requires access to the recorded package repository.

Enter the authorized ZIP location only at runtime. For a POSIX shell:

```sh
printf 'Authorized input ZIP location: '
IFS= read -r SURGO_MANITOBA_INPUT_ZIP
export SURGO_MANITOBA_INPUT_ZIP
```

## Complete rebuild

```sh
Rscript run_all.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

This reconstructs both joins, seven current country reports and eight supplementary
checks. Source data are verified by member size/hash. Current code starts with the
supplied original surveys, cleaned UoM indicators, archetypes and references; it
does not refit upstream archetypes or recreate every upstream UoM cleaning step.

| Parameter | Default | Meaning |
|---|---|---|
| `--work-dir` | `output/build` | Private joins, calculation objects, model fits and audits. |
| `--cache-dir` | `data/cache` | Manifest-verified extracted input files. |
| `--output-dir` | `output/rebuilt` | Rebuilt HTMLs, following the same aggregate/Rural-Urban layout. |
| `--verify` | `true` | Compare all 15 current outputs with reference signatures. |
| `--python` | `python3` | Installed Python executable. |
| `--pandoc-dir` | Environment/search path | Installed Pandoc directory. |

The root runner preserves reference reports in the source folders. It creates an
administrative reference under the residence join, then reuses verified joins and
inputs for country runs. No previous joins, caches or fitted models are required.
Original women and survey columns are retained in joins. Analysis preparation then
selects confirmed FP providers, aggregates valid responses and assigns those supply
profiles to women. The base joins used for upstream reconciliation intentionally
precede this FP-only analysis preparation.

## Run one country report

Each R script includes its country definitions, aggregation/assignment, descriptive
scores, models, applicable sensitivity checks and independent numerical verification.
It automatically uses the adjacent Rmd and the shared support utilities/renderers.
Run a complete script, not individual sections or every file in `support/`.

### Lagos_analysis_aggregate

```sh
Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

### Senegal_analysis_aggregate

```sh
Rscript aggregate/senegal/Senegal_analysis_aggregate.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

### Cote_dIvoire_analysis_aggregate

```sh
Rscript aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

### Sindh_analysis

```sh
Rscript aggregate/sindh/Sindh_analysis.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

### Lagos_rural_urban_updated

```sh
Rscript rural_urban/lagos/Lagos_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

### Senegal_rural_urban_updated

```sh
Rscript rural_urban/senegal/Senegal_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

### CotedIvoire_rural_urban_updated

```sh
Rscript rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

A single-report run can prepare its own join. To reuse a verified join, supply
`--join-dir` and its corresponding `--cache-dir`; keeping `--input-zip` checks that
it matches the same bundle. Aggregate scripts accept an administrative join or the
residence join's `admin_reference/`. Rural/Urban scripts require the residence join.
Do not initialize the same extracted cache concurrently.

Each script defaults to its adjacent HTML. Use `--output-dir` to preserve it and
build elsewhere. Working objects default to `output/<aggregation>/<country>/`.
The HTML filename matches the R basename, as does the adjacent Rmd. There is no
Sindh residence report because the woman's source lacks a documented residence type.

## Supplementary checks and outputs

The full runner executes `data_checks/Build_data_checks.R` after the seven reports.
That file contains the cumulative diagnostic calculations in one place. It can be
rerun using the full runner's generated work/cache directories:

```sh
Rscript data_checks/Build_data_checks.R --work-dir output/build --cache-dir data/cache --output-dir output/rebuilt
```

| Output folder within selected rebuilt root | HTML |
|---|---|
| `aggregate/<country>/` | Four current aggregate reports. |
| `rural_urban/<country>/` | Three current residence-specific reports. |
| `data_checks/` | `Surgo_UoM_checks.html`; `Composite score_comparisons.html`. |
| `generated_checks/` | `Demand_population_classification_check.html`; `Score_profile_examples.html`; `Score_profile_contrasts.html`; `Demand_pair_comparisons.html`; `Method_awareness_check_Senegal_Lagos.html`; `Data_checks.html`. |
| Root | `validation.json`, containing comparison results. |

Only the two principal diagnostic HTMLs are published in `data_checks/`; the other
six remain generated-only outputs with validated references. Historical report
versions remain in Git history and are not extra current builds.

Country work directories contain `inputs/*_fp*.rds` joins and
`<report_basename>_files/analysis_frame.rds`, `report_data.json`, `models_zscore/`
and `checks/verification.json`. The four aggregate analyses also have
`regression_sensitivity/`; PCA is included where previously implemented. These
are private working resources, not upload files.

## Validate

Numerical checks run inside each country R script. They independently verify
source rows/variables, facility eligibility, means, geography, assignments, weighted
percentages, scores, models, clustered uncertainty and implemented sensitivities.
The full runner also compares analytical report content and figures/maps with
versioned references. Source/report privacy and structure checks are:

```sh
python3 validation/check_release.py
```

A failed run stops at its failing stage. Do not change reference values to bypass
an analytical difference. A fresh build needs the recorded software environment;
source data changes, new exports or new package versions require review. Input
identity and a successful rebuild do not certify original sampling-design details,
geographic proxies, causal effects or adequate statistical power.
