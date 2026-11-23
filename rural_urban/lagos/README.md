# Lagos — Rural/Urban analysis

Open **`Lagos_rural_urban_updated.html`** to read the published report. Open **`Lagos_rural_urban_updated.R`** to reproduce it. The adjacent `.Rmd` supplies the rendering template; ordinary users run the R script.

## RStudio: Source and choose the ZIP

Start a fresh RStudio session, open `Lagos_rural_urban_updated.R` and click **Source** in the script-editor toolbar. Select the confidential complete input ZIP in the native file picker. The script installs/loads its recorded R libraries automatically, constructs the joins and analysis, then builds the HTML/maps. Python 3 and Pandoc are required for HTML; RStudio normally supplies Pandoc.

The Console prints the output location: default `output/rebuilt/rural_urban/lagos/` relative to the repository root, or the downloaded script’s folder when used alone. Private calculations go to `output/build/rural_urban/lagos/` and inputs to `data/cache/`. These are ignored and must never be uploaded.

A single downloaded R script also works without other R files: its R calculations and package-version records are included, and public map/layout files are automatically downloaded from a pinned, verified snapshot. No confidential survey data are transmitted. In a full checkout the local public files in `support/` are used.

## Terminal / PowerShell

```sh
Rscript rural_urban/lagos/Lagos_rural_urban_updated.R --input-zip "$INPUT_ZIP"
```

Run from the repo root; in PowerShell replace `$INPUT_ZIP` with `$InputZip`, supplied through `Read-Host`. See [the main README](../../README.md) and [run guide](../../docs/RUN_GUIDE.md) for the two complete workflows, software setup, dataset names and optional output arguments. Source files used by this report are listed explicitly at the top of its R script.

This Rural/Urban report retains its existing scoring/PCA sensitivities. The newer regression-completeness appendix is scoped to the aggregate reports.
