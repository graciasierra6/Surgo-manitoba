# Sindh — aggregate analysis

Open **`Sindh_analysis.html`** to read the published report. Open **`Sindh_analysis.R`** to reproduce it. The adjacent `.Rmd` supplies the rendering template; ordinary users run the R script.

## RStudio: Source and choose the ZIP

Start a fresh RStudio session, open `Sindh_analysis.R` and click **Source** in the script-editor toolbar. Select the confidential complete input ZIP in the native file picker. The script installs/loads its recorded R libraries automatically, constructs the joins and analysis, then builds the HTML/maps. Python 3 and Pandoc are required for HTML; RStudio normally supplies Pandoc.

The Console prints the output location: default `output/rebuilt/aggregate/sindh/` relative to the repository root, or the downloaded script’s folder when used alone. Private calculations go to `output/build/aggregate/sindh/` and inputs to `data/cache/`. These are ignored and must never be uploaded.

A single downloaded R script also works without other R files: its R calculations and package-version records are included, and public map/layout files are automatically downloaded from a pinned, verified snapshot. No confidential survey data are transmitted. In a full checkout the local public files in `support/` are used.

## Terminal / PowerShell

```sh
Rscript aggregate/sindh/Sindh_analysis.R --input-zip "$INPUT_ZIP"
```

Run from the repo root; in PowerShell replace `$INPUT_ZIP` with `$InputZip`, supplied through `Read-Host`. See [the main README](../../README.md) and [run guide](../../docs/RUN_GUIDE.md) for the two complete workflows, software setup, dataset names and optional output arguments. Source files used by this report are listed explicitly at the top of its R script.

The four aggregate reports include the regression complete-case sensitivity appendix: included/excluded women, available-case models, response weighting and geographic jackknife checks.
