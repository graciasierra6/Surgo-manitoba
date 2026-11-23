# SURGO–Manitoba

This project links SURGO women’s survey records to University of Manitoba facility characteristics and examines demand barriers, supply constraints and modern contraceptive use.

## Start here: choose how you want to run a report

| Approach | Use this if… | Instructions |
|---|---|---|
| **1. RStudio** | You normally open and run ordinary R scripts. | [Open a script, click Source and select the input ZIP](#approach-1-rstudio). |
| **2. Claude Code or terminal** | You want the existing repository scripts executed for you, or prefer terminal commands. | [Run the repository with the complete input ZIP](#approach-2-claude-code-or-terminal). |

**Both approaches execute the same R analyses.** For a slower, step-by-step walkthrough, open [How to run the reports](README_RUN_REPORTS.md). It includes a copy-and-paste request for Claude Code, the exact country folders and filenames, output locations and help with common first-run problems.

## Where are the input data?

**The datasets are in the separately supplied `SURGO_Manitoba_linkage_inputs_complete.zip`. They are not stored on GitHub.** Obtain that complete ZIP from the project team and keep it zipped. The scripts check it, extract the inputs and select the files they need. You do not need to load individual datasets manually or supply a prejoined dataset.

Inside the ZIP, original women’s data are in `inputs/surgo/` and detailed facility data are in `inputs/uom/`. Cleaned summaries, archetypes, processed references and geographic files are also included. [Exact dataset filenames](docs/DATASET_FILENAMES.md) · [Complete ZIP inventory](docs/INPUTS.md).

**There is one required input ZIP: `SURGO_Manitoba_linkage_inputs_complete.zip` from OneDrive.** Obtain the code separately from GitHub: one R script is enough for the RStudio route; the terminal route uses a complete working copy of the repository. Keep the input ZIP zipped.

## Aggregate analyses: scripts and published HTML reports

Start in [aggregate/](aggregate/). Each country folder contains its R script, published HTML, R Markdown template and country README. Aggregate reports assign each woman the FP-provider facility averages for her whole LGA, département or district.

| Setting | Folder containing the script and report | R script to run | Published HTML to compare against |
|---|---|---|---|
| Lagos | [aggregate/lagos/](aggregate/lagos/) | [Lagos_analysis_aggregate.R](aggregate/lagos/Lagos_analysis_aggregate.R) | [Lagos_analysis_aggregate.html](aggregate/lagos/Lagos_analysis_aggregate.html) |
| Senegal | [aggregate/senegal/](aggregate/senegal/) | [Senegal_analysis_aggregate.R](aggregate/senegal/Senegal_analysis_aggregate.R) | [Senegal_analysis_aggregate.html](aggregate/senegal/Senegal_analysis_aggregate.html) |
| Côte d’Ivoire | [aggregate/cotedivoire/](aggregate/cotedivoire/) | [Cote_dIvoire_analysis_aggregate.R](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R) | [Cote_dIvoire_analysis_aggregate.html](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.html) |
| Sindh | [aggregate/sindh/](aggregate/sindh/) | [Sindh_analysis.R](aggregate/sindh/Sindh_analysis.R) | [Sindh_analysis.html](aggregate/sindh/Sindh_analysis.html) |

To view an HTML report from GitHub, download the actual file using **Download raw file**, then open the downloaded `.html` in your browser. GitHub may otherwise show its source code.

## Rural/Urban analyses: scripts and published HTML reports

These assign the matching area–residence facility profile, with a flagged whole-area fallback when the exact group has no confirmed FP providers. Sindh has no Rural/Urban report because its SURGO data lack a documented residence variable.

| Setting | Folder | R script to run | Published HTML |
|---|---|---|---|
| Lagos | [rural_urban/lagos/](rural_urban/lagos/) | [Lagos_rural_urban_updated.R](rural_urban/lagos/Lagos_rural_urban_updated.R) | [Lagos_rural_urban_updated.html](rural_urban/lagos/Lagos_rural_urban_updated.html) |
| Senegal | [rural_urban/senegal/](rural_urban/senegal/) | [Senegal_rural_urban_updated.R](rural_urban/senegal/Senegal_rural_urban_updated.R) | [Senegal_rural_urban_updated.html](rural_urban/senegal/Senegal_rural_urban_updated.html) |
| Côte d’Ivoire | [rural_urban/cotedivoire/](rural_urban/cotedivoire/) | [CotedIvoire_rural_urban_updated.R](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R) | [CotedIvoire_rural_urban_updated.html](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.html) |

## Before running either approach

The execution environment needs **R, Python 3.9 or later and Pandoc**. Validation used R 4.6.0 and the recorded package versions. RStudio normally supplies Pandoc for its own R sessions; terminal runs must also be able to locate it. Required R packages install automatically for individual scripts. First-time package and public-resource downloads require internet access. [Software details and troubleshooting](docs/RUN_GUIDE.md).

## Approach 1: RStudio

**You only need the desired R script and the complete input ZIP, plus the software above.** You do not need to write data-loading commands.

1. Choose a script from either report table above. Download the actual `.R` file using **Download raw file**, or find it in your downloaded copy of the repository.
2. Open RStudio and select **Session → Restart R**. Then select **File → Open File** and open the script.
3. Click **Source** at the top of the script editor. This runs the entire file from beginning to end.
4. When the file-selection window opens, select **`SURGO_Manitoba_linkage_inputs_complete.zip`**.
5. Let the script install/load its packages, prepare and join the data, run the analysis and checks, and build the HTML. Wait for **Completed** and the `>` prompt to return.
6. Open the new HTML at the output location printed in the Console. For Lagos aggregate, this is `output/rebuilt/aggregate/lagos/Lagos_analysis_aggregate.html`.

In a complete repository, outputs are created inside the repository. With one separately downloaded R script, they are created inside that script’s folder. Scripts can run independently and in any country order.

Each country script contains its R calculations, joins, recodes, scoring, models, implemented sensitivities and package-version records. A downloaded single script automatically obtains the matching public map/layout files from a pinned, checksum-verified version; a full repository uses its local `support/` files. No prejoined dataset or separate R helper script is needed for an individual report.

[Full RStudio walkthrough and all aggregate output filenames](README_RUN_REPORTS.md#approach-1-rstudio).

## Approach 2: Claude Code or terminal

### Have Claude Code execute the existing scripts

Claude Code can read files and execute commands. It can therefore follow this repository’s run instructions for you. [Official product documentation](https://code.claude.com/docs/en/overview).

Give it **the repository link, access to the complete input ZIP and the report you want**. A GitHub link alone supplies the code; the confidential data must be available separately in the environment where the scripts will run.

Ask it to:

1. Obtain a working copy of `https://github.com/graciasierra6/Surgo-manitoba` and record the commit used.
2. Read this README, [the step-by-step guide](README_RUN_REPORTS.md), [the technical run guide](docs/RUN_GUIDE.md) and the selected script’s input/package notes.
3. Confirm the input ZIP and required software are accessible, and restore the recorded R packages following `setup.R`.
4. Execute the selected `.R` script with the ZIP supplied through `--input-zip`. The four aggregate script paths are listed above. Each script does its own data loading, joins and analysis.
5. Check completion and verification results, compare the rebuilt report with its published counterpart, and return the generated HTML location and any differences. Use `output/rebuilt/` for new reports and preserve the published files.

For one report, copy this request:

> Reproduce Lagos aggregate from https://github.com/graciasierra6/Surgo-manitoba using SURGO_Manitoba_linkage_inputs_complete.zip, which I will make accessible to this session. Follow README_RUN_REPORTS.md and docs/RUN_GUIDE.md. Check the software and restore recorded dependencies, then execute aggregate/lagos/Lagos_analysis_aggregate.R with --input-zip. Use the existing analytical definitions. Report the commit, command executed, completion status, checks and generated HTML location. Identify any blockers or differences; do not claim reproduction without execution. Keep inputs and working data out of Git. Do not edit the published reports, commit or push.

For all four aggregate reports, use [the ready-to-copy four-report request](README_RUN_REPORTS.md#copy-and-paste-this-request-for-all-four-aggregate-reports). For the complete seven-report rebuild and supplementary checks, use `run_all.R`. These are different run scopes; `run_all.R` also rebuilds Rural/Urban reports.

The underlying scripts have been validated on macOS. A separate Claude Code execution has not been tested as part of this documentation, and a native Windows end-to-end run remains unverified. The assistant needs file and command-execution access; discussing the code alone does not rebuild reports.

### Or run the terminal commands yourself

Download/clone the complete repository and open macOS Terminal or Windows PowerShell **in its top folder**, containing `README.md`, `setup.R` and `run_all.R`.

**macOS — complete rebuild:**

```sh
Rscript setup.R
printf 'Input ZIP file: '
IFS= read -r INPUT_ZIP
Rscript run_all.R --input-zip "$INPUT_ZIP"
```

**Windows PowerShell — complete rebuild:**

```powershell
Rscript setup.R
$InputZip = Read-Host "Input ZIP file"
Rscript run_all.R --input-zip "$InputZip"
```

When asked, paste the actual input ZIP location without surrounding quotes. Spaces in the location are supported. `setup.R` restores the package environment. The complete run prepares both joins, seven country reports and eight supplementary checks, then compares results with the validated references.

To run **one report**, replace `run_all.R` with its script path, for example:

```sh
Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip "$INPUT_ZIP"
```

PowerShell uses `"$InputZip"`. Individual scripts also handle their own package setup, so `setup.R` is optional for the single-report route. [Commands for every report and optional arguments](docs/RUN_GUIDE.md).

## Where are the new HTML reports?

New reports go under **`output/rebuilt/`**, preserving the same workflow/country folders and HTML filenames shown in the report tables. For example:

```text
output/rebuilt/aggregate/lagos/Lagos_analysis_aggregate.html
output/rebuilt/aggregate/senegal/Senegal_analysis_aggregate.html
output/rebuilt/aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.html
output/rebuilt/aggregate/sindh/Sindh_analysis.html
```

Published reports remain in `aggregate/` or `rural_urban/`. `RESULTS.json` identifies the result files for each new run. Private calculations, joined data and models go to ignored `output/build/`; verified extracted inputs go to ignored `data/cache/`. Keep those working folders private.

## Repository layout — what each place is for

```text
Surgo-manitoba/
  README.md                 Start here: both ways to run
  README_RUN_REPORTS.md     Detailed beginner guide and copyable execution request
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
