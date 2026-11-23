# SURGO–Manitoba

This project links SURGO women’s survey records to University of Manitoba facility characteristics and examines demand barriers, supply constraints and modern contraceptive use.

**Choose either workflow below. Both run the same analysis and produce the HTML reports, maps, tables, models and implemented sensitivity analyses.** Input data are confidential and supplied separately in one ZIP; no input datasets belong in this repository.

## Find your report and R script

There are **seven reports**. Aggregate reports assign the whole LGA/département/district facility profile to each woman. Rural/Urban reports assign her matching area–residence profile, with a flagged whole-area fallback when the exact group has no confirmed FP providers. Sindh has no Rural/Urban report because its SURGO data lack a documented residence field.

| Setting | Aggregate: whole area | Rural/Urban: area and residence |
|---|---|---|
| Lagos | [R script](aggregate/lagos/Lagos_analysis_aggregate.R) · [HTML report](aggregate/lagos/Lagos_analysis_aggregate.html) | [R script](rural_urban/lagos/Lagos_rural_urban_updated.R) · [HTML report](rural_urban/lagos/Lagos_rural_urban_updated.html) |
| Senegal | [R script](aggregate/senegal/Senegal_analysis_aggregate.R) · [HTML report](aggregate/senegal/Senegal_analysis_aggregate.html) | [R script](rural_urban/senegal/Senegal_rural_urban_updated.R) · [HTML report](rural_urban/senegal/Senegal_rural_urban_updated.html) |
| Côte d’Ivoire | [R script](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R) · [HTML report](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.html) | [R script](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R) · [HTML report](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.html) |
| Sindh | [R script](aggregate/sindh/Sindh_analysis.R) · [HTML report](aggregate/sindh/Sindh_analysis.html) | Not available |

## Option 1 — run from a terminal or PowerShell

Use this route to rebuild the complete project or automate runs. Download/clone the repository and open a terminal **in its top folder**, the folder containing this README, `setup.R` and `run_all.R`. Keep the repository folders together.

**Software:** R (validation used 4.6.0), Python 3 and Pandoc. RStudio includes Pandoc for its own R sessions; a terminal must also be able to locate Pandoc, or use `--pandoc-dir`. Python is detected as `python3`, `python` or the Windows `py` launcher. [Software details and troubleshooting](docs/RUN_GUIDE.md).

### macOS Terminal

```sh
Rscript setup.R
printf 'Input ZIP file: '
IFS= read -r INPUT_ZIP
Rscript run_all.R --input-zip "$INPUT_ZIP"
```

### Windows PowerShell

```powershell
Rscript setup.R
$InputZip = Read-Host "Input ZIP file"
Rscript run_all.R --input-zip "$InputZip"
```

Paste the actual ZIP location without surrounding quotes when asked; spaces in the location are supported. `setup.R` is the one-time environment restore. The complete run rebuilds both joins, all seven country reports and eight supplementary checks, then compares results with the validated references.

**Where are the results?** Open `output/rebuilt/`. It mirrors the report table above, for example `output/rebuilt/aggregate/lagos/Lagos_analysis_aggregate.html`. Published reports in the source folders are preserved. Private calculations/joins go to ignored `output/build/`; verified extracted inputs go to ignored `data/cache/`.

To run **only one report**, use its R script instead of `run_all.R`. It installs/restores its own R dependencies automatically; `setup.R` is not required for this individual route:

```sh
Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip "$INPUT_ZIP"
```

In PowerShell, use the same command with `"$InputZip"`. Commands for all seven reports and optional software/output parameters are in the [run guide](docs/RUN_GUIDE.md).

## Option 2 — open one script in RStudio

Use this route when you prefer ordinary R scripts and a file-selection window. **You do not need to type an input location or select a prejoined dataset.**

### Choose your independent R script

Download **one** of these scripts for the report you want. Each contains its own R analysis and automatic package setup; use it with the confidential input ZIP.

| Setting | Aggregate: whole area | Rural/Urban: area and residence |
|---|---|---|
| Lagos | [Lagos aggregate R script](aggregate/lagos/Lagos_analysis_aggregate.R) | [Lagos Rural/Urban R script](rural_urban/lagos/Lagos_rural_urban_updated.R) |
| Senegal | [Senegal aggregate R script](aggregate/senegal/Senegal_analysis_aggregate.R) | [Senegal Rural/Urban R script](rural_urban/senegal/Senegal_rural_urban_updated.R) |
| Côte d’Ivoire | [Côte d’Ivoire aggregate R script](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R) | [Côte d’Ivoire Rural/Urban R script](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R) |
| Sindh | [Sindh aggregate R script](aggregate/sindh/Sindh_analysis.R) | Not available |

### Open the script and run it

1. Obtain the confidential complete input ZIP from the project team. Keep it as a ZIP; the script verifies and extracts it automatically.
2. Open the independent R script linked in this Option 2 table. In GitHub, download the actual `.R` file using **Raw → Download** (or use the downloaded repository). Avoid saving the GitHub webpage as an R file.
3. Start a fresh RStudio session and open that `.R` file: **File → Open File**.
4. Click **Source** in the script-editor toolbar. This runs the entire script from top to bottom.
5. In the native file-selection window, choose the confidential input `.zip` file.
6. Wait for package setup, analysis and report generation. The script automatically installs missing/different R dependencies at their recorded versions, loads them, constructs the joins and supply averages, runs the models and sensitivities, verifies results, and builds the HTML/maps.
7. Read the **Report output** location printed in the Console. Open the resulting `.html` in your browser. `RESULTS.json` identifies the result files. In a full repo, the default is the same `output/rebuilt/<workflow>/<setting>/` used by the CLI.

### Running a single downloaded R script

Each country script includes its own R calculations, join code, recodes, scores, model code, sensitivity analyses and package-version records. It does not source another R analysis file and does not require a separate `renv.lock` or prejoined dataset.

For HTML/maps it also needs public boundaries and presentation files. **Inside the full repository**, these come from `support/`. **With just one downloaded R script**, the script automatically downloads the matching public display files from a pinned repository version and verifies their checksums. You do not need to organize a `report_assets/` folder manually. Only public files are downloaded; confidential survey data are never transmitted.

R and the required R packages cannot replace **Python 3 and Pandoc**, which the existing HTML renderer uses. Install Python 3 before running; RStudio normally supplies Pandoc automatically. First-time package/resource downloads need internet access. If a required package cannot install, the Console identifies the dependency; fix the installation and click Source again. A loaded conflicting version may require **Session → Restart R** before rerunning.

The first section of each R script repeats these instructions and lists the exact source datasets. Its simple settings include `input_zip`, `output_dir` and `render_html <- TRUE`. For numerical outputs without HTML, set `render_html <- FALSE` (CLI: `--no-html`); that route requires only R, its packages and the inputs.

## Repository layout — what each place is for

```text
Surgo-manitoba/
  README.md                 Start here: both ways to run
  setup.R                   One-time package restore for the complete CLI run
  run_all.R                 Complete CLI rebuild and report comparison
  renv.lock                 Recorded R environment
  aggregate/
    lagos/                  Lagos aggregate .R, .Rmd, .html and README
    senegal/                Senegal aggregate report and code
    cotedivoire/            Côte d’Ivoire aggregate report and code
    sindh/                  Sindh district report and code
  rural_urban/
    lagos/                  Lagos residence-specific report and code
    senegal/                Senegal residence-specific report and code
    cotedivoire/            Côte d’Ivoire residence-specific report and code
  data_checks/              Coverage and component-comparison reports
  support/                  Public map/layout assets and full-build utilities
  docs/                     Input inventory, run guide and methods
  validation/               Numerical tests and report reference checks
  output/                   Created when running; PRIVATE/ignored
  data/                     Created when extracting inputs; PRIVATE/ignored
```

Use the **`.R` file** to run an analysis. The adjacent **`.Rmd`** is a rendering template, and **`.html`** is the published report you can read without running code. Shared utilities remain for the complete rebuild and supplementary checks; the seven individual report scripts include their own R calculations.

## Input ZIP to share

Give the same complete `SURGO_Manitoba_linkage_inputs_complete.zip` with any report script. It contains **56 members: 14 primary analytical inputs plus supporting/reference files and the manifest**. Keep its internal folder names unchanged. Each script checks the ZIP manifest; it chooses the relevant setting’s files automatically.

- [Exact dataset filenames](docs/DATASET_FILENAMES.md) — names to match to authorized data access.
- [Full ZIP inventory](docs/INPUTS.md) — purposes, sizes and checksums.
- [Dataset catalog](docs/dataset_catalog.json) — identifiers, weights and source mappings; Databricks links remain pending.

Input locations and credentials are supplied at runtime. The repository contains no input datasets. Running analyses creates extracted inputs, joined microdata and fitted objects that must remain private and must never be uploaded.

## Analysis reproduced

Coverage uses all source women. Subsequent analyses use women in need of FP, preserving the original SURGO classification in Lagos/Senegal/Côte d’Ivoire and the approved derived definition in Sindh. Weighted SURGO estimation uses `survey`, with `svyglm()`, `ids = ~1`, no strata/FPC, and separate LGA/département/district CR2/Satterthwaite inference. There are no random effects. Manitoba facility averages are unweighted valid-response averages among confirmed FP providers.

Original SURGO weight fields (copied into an internal `weight` column):

| Setting | Source weight | Main regression clustering |
|---|---|---|
| Lagos | `weight` | LGA |
| Senegal | `WEIGHT` | Département |
| Côte d’Ivoire | `WEIGHT` | Département |
| Sindh | `Normalizedweights` | District |

Both scores increase with demand barriers/supply constraints. Primary area flags compare the six-component average z-score with zero; maps also show 0.5, 1 and 1.5 final-composite SD cutoffs. Regression demand scores use each woman’s own responses, while supply scores reflect her assigned facility environment.

All four **aggregate** reports include the regression complete-case sensitivities: included/excluded profiles, genuine available-case models and response weighting with geographic jackknife checks. The three **Rural/Urban** reports retain their existing scoring/PCA sensitivities; the newer regression-completeness appendix remains scoped to aggregate analyses. Sindh includes the corrected training reconstruction. [Methods and limitations](docs/SURVEY_AND_SCORING.md).

## Checks before sharing code

```sh
python3 validation/check_release.py
```

Every country run includes independent numerical checks. The full CLI rebuild compares all current reports with the validated references. Reproduction verifies implementation; it does not establish causality or resolve documented source-data/design limitations. Only code, documentation, approved aggregate reports, public display assets and validation metadata belong in Git. Private inputs, caches, joins, libraries and fitted objects remain ignored.
