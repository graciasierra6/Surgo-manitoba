# Running the analyses

Start with [the main README](../README.md) or the [step-by-step beginner guide](../README_RUN_REPORTS.md). Both label the same two approaches:

| Approach | What you do | Instructions |
|---|---|---|
| 1. RStudio | Open one R script, click Source and select the complete input ZIP. | [RStudio walkthrough](../README_RUN_REPORTS.md#approach-1-rstudio). |
| 2. Claude Code or terminal | Have the repository scripts executed with the input ZIP, or type the commands yourself. | [Execution steps and copyable request](../README_RUN_REPORTS.md#approach-2-claude-code-or-terminal). |

The beginner guide gives the exact [aggregate country folders, R scripts and published HTML filenames](../README_RUN_REPORTS.md#aggregate-analyses-exact-github-folders-scripts-and-html-reports). The confidential data are supplied in `SURGO_Manitoba_linkage_inputs_complete.zip`, separately from GitHub. A command-executing assistant needs access to that ZIP and the required software; the repository link alone does not run an analysis.

This page provides the technical commands, runtime parameters and troubleshooting for both approaches.

## Software

The validation environment is R 4.6.0 and the package versions recorded in `renv.lock`, Python 3.9+ and Pandoc (reference 3.8.3). Each report script embeds the same package-version records and installs/restores its own required libraries. A full `run_all.R` rebuild starts with `Rscript setup.R`.

RStudio normally exposes its bundled Pandoc when using Source. For CLI runs, install Pandoc on PATH or supply `--pandoc-dir` / `RSTUDIO_PANDOC`. Python is discovered as `python3`, `python` or Windows `py -3`; override with `--python` / `SURGO_MANITOBA_PYTHON`. External software is checked before a long report analysis starts. R spatial dependencies can require compatible system libraries or compiler tools when a binary package is unavailable.

## All seven single-report commands

Open a terminal in the repository root and obtain the ZIP location once.

macOS Terminal:

```sh
printf 'Input ZIP file: '
IFS= read -r INPUT_ZIP
```

Windows PowerShell:

```powershell
$InputZip = Read-Host "Input ZIP file"
```

Paste the location without surrounding quotes; the commands below quote the variable for you.

| Report | Command (macOS variable; PowerShell uses `$InputZip`) |
|---|---|
| Lagos — aggregate | `Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip "$INPUT_ZIP"` |
| Senegal — aggregate | `Rscript aggregate/senegal/Senegal_analysis_aggregate.R --input-zip "$INPUT_ZIP"` |
| Côte d’Ivoire — aggregate | `Rscript aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R --input-zip "$INPUT_ZIP"` |
| Sindh — aggregate | `Rscript aggregate/sindh/Sindh_analysis.R --input-zip "$INPUT_ZIP"` |
| Lagos — Rural/Urban | `Rscript rural_urban/lagos/Lagos_rural_urban_updated.R --input-zip "$INPUT_ZIP"` |
| Senegal — Rural/Urban | `Rscript rural_urban/senegal/Senegal_rural_urban_updated.R --input-zip "$INPUT_ZIP"` |
| Côte d’Ivoire — Rural/Urban | `Rscript rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R --input-zip "$INPUT_ZIP"` |

All paths/locations are runtime parameters. The same script can be opened in RStudio and run with Source; no terminal arguments are required, and a native picker requests the ZIP. You can also download just that one `.R` file: its package installer and R analysis are self-contained, and public display resources are downloaded and verified automatically.

## Optional arguments

| Argument | Purpose |
|---|---|
| `--input-zip FILE` | Authorized complete ZIP; interactive Source instead opens the picker. |
| `--input-dir DIR` | Already extracted inputs in the documented relative layout. |
| `--output-dir DIR` | Destination for HTML and result index. |
| `--work-dir DIR` | Private calculated tables, joins and model objects. |
| `--cache-dir DIR` | Verified extracted inputs. |
| `--join-dir DIR` | Advanced: existing verified administrative/residence join from the full pipeline. |
| `--library-dir DIR` | Writable library for recorded R package versions. |
| `--python EXE` | Python 3 executable or Windows launcher. |
| `--pandoc-dir DIR` | Folder containing Pandoc. |
| `--no-html` | R-only numerical results; omit HTML/map rendering. |
| `--no-install-packages` | Use an externally managed environment and fail if recorded packages are missing. |

Relative locations resolve against the project root in a complete checkout or the single script’s folder when downloaded separately. Default report outputs: `output/rebuilt/<workflow>/<setting>/`; private calculations: `output/build/<workflow>/<setting>/`; inputs: `data/cache/`.

## Input names and transformations

The exact original survey and facility files are listed at the top of every report script and in [DATASET_FILENAMES.md](DATASET_FILENAMES.md). [INPUTS.md](INPUTS.md) lists every ZIP member and role. Do not flatten the ZIP or substitute an unweighted survey version.

Each individual script includes readable sections for input reading, geographic assignment, FP-provider filtering, measure-specific denominators, aggregation and woman-level joining, survey recodes/weighted estimates, scoring/classification, models and sensitivities. For Rural/Urban, an exact group with no confirmed FP providers uses its same-area FP-provider averages and a fallback flag; an undefined component is not automatically replaced. Geography assignment follows the checked crosswalks and their documented qualifications.

## What will appear in the output?

The HTML is the complete report with maps, classifications, descriptive tables, adjusted models and the sensitivities implemented for that report. `RESULTS.json` identifies the report and key result files. Working calculations include `report_data.json`, `heatmap_values_and_denominators.csv`, `all_method_scores_and_classifications.csv`, `component_score_trace.csv`, `sd_threshold_classifications.csv`, `models_zscore/` and independent `checks/`. PCA and aggregate regression-sensitivity folders appear where used.

These working folders can contain microdata and fitted objects: keep them private. The full CLI run additionally creates the cumulative supplementary checks and validates all 15 rebuilt documents. Single-report runs do not rebuild unrelated country/check documents.

## Common first-run problems

- **Rscript is not recognized:** R is not on the terminal PATH. In the RStudio Console, run `file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")` to locate it. Configure PATH or invoke that executable at runtime. The RStudio Source route avoids this terminal setup.
- **A package/version conflict is already loaded:** select Session → Restart R, reopen the report script and click Source again. Installations go to a private local library; system libraries are preserved.
- **Package installation fails:** inspect the first error in the Console, confirm internet access and compatible R/system libraries, then rerun. The script does not silently substitute current package versions for the validated ones.
- **Python is missing:** install Python 3, restart RStudio/terminal if PATH changed, or provide its executable with the documented parameter. Numeric-only mode remains available.
- **Pandoc is missing:** Source from RStudio or install/configure Pandoc for the terminal. A runtime `--pandoc-dir` overrides detection.
- **ZIP file is not found or not complete:** use the supplied confidential complete bundle. Source opens a picker; terminal paths with spaces must be quoted.
- **Public resource download fails for a single script:** check internet access or run that script from the complete repository, which includes the required public assets. Downloads are pinned and checksum-verified.

## Validation scope

Numerical checks run inside each report. `validation/compare_analysis.R` compares saved results, `validation/verify_reports.py` compares reports, and `validation/check_release.py` checks the source/shareable tree. The new interfaces are tested on macOS; Windows commands, path handling and Python-launcher support are provided, but an actual Windows end-to-end run requires verification on Windows. No Windows execution result is implied by macOS tests.
