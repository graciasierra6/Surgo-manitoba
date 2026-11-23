# SURGO–Manitoba: how to run the reports

There are two ways to run the analyses. **Choose Approach 1 if you normally work in RStudio.** Choose Approach 2 to have Claude Code run the repository for you, or to type the terminal commands yourself. Both use the same scripts and input data to produce the HTML reports.

- [Approach 1: RStudio](#approach-1-rstudio)
- [Approach 2: Claude Code or terminal](#approach-2-claude-code-or-terminal)
- [Where to find the aggregate scripts and published HTMLs](#aggregate-analyses-exact-github-folders-scripts-and-html-reports)
- [Where the input data are stored](#where-are-the-input-data)

## Where are the input data?

**The input datasets are in the separately supplied ZIP file, `SURGO_Manitoba_linkage_inputs_complete.zip`. They are not stored on GitHub.** Give your colleague this ZIP along with the script or repository link.

Inside the ZIP, the original women's survey files are in `inputs/surgo/`, and the detailed facility survey files are in `inputs/uom/`. Cleaned facility summaries, archetype assignments and other required inputs are also included in the ZIP, in folders such as `uom_pipeline/`, `surgo_pipeline/`, `exports/` and `lookups/`.

Keep the complete ZIP zipped and its internal filenames unchanged. In RStudio, select this ZIP when the file-selection window opens. For the repository/terminal approach, supply it as the input ZIP. Each script finds the files it needs automatically.

## Before you start

You need:

- **The input ZIP:** `SURGO_Manitoba_linkage_inputs_complete.zip`, supplied separately by the project team. Keep it zipped. The scripts find, check and extract the required datasets automatically.
- **The code:** download one R script using the links below, or download the [complete repository](https://github.com/graciasierra6/Surgo-manitoba) using **Code → Download ZIP**, then extract the repository ZIP.
- **R and Python 3.9 or later**, installed in the environment where you will run the analysis. Use **RStudio** for Approach 1. HTML generation also requires **Pandoc**, which RStudio normally includes. For Approach 2, Pandoc must be available to the commands being executed.
- Internet access for the first run. Required R packages install automatically. The validated R version is **4.6.0**; package installation on a different R version may require help.

**The two ZIPs have different purposes:** extract the downloaded *repository* ZIP to get the code, but leave the confidential *input* ZIP zipped.

## Choose the report you want

**Aggregate** assigns each woman the facility averages for her whole LGA, département or district. **Rural/Urban** assigns the averages for her area and residence type, with a flagged whole-area fallback when that group has no confirmed FP-providing facilities.

### Aggregate analyses: exact GitHub folders, scripts and HTML reports

Start at [the aggregate folder on GitHub](https://github.com/graciasierra6/Surgo-manitoba/tree/main/aggregate). Open the country folder you want. **Each country folder contains both the R script and its published HTML report**, along with its README and R Markdown file.

| Setting | GitHub folder containing both files | R script to download and run | Published HTML to compare against |
|---|---|---|---|
| Lagos | [aggregate/lagos](https://github.com/graciasierra6/Surgo-manitoba/tree/main/aggregate/lagos) | [Lagos_analysis_aggregate.R](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/lagos/Lagos_analysis_aggregate.R) | [Lagos_analysis_aggregate.html](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/lagos/Lagos_analysis_aggregate.html) |
| Senegal | [aggregate/senegal](https://github.com/graciasierra6/Surgo-manitoba/tree/main/aggregate/senegal) | [Senegal_analysis_aggregate.R](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/senegal/Senegal_analysis_aggregate.R) | [Senegal_analysis_aggregate.html](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/senegal/Senegal_analysis_aggregate.html) |
| Côte d’Ivoire | [aggregate/cotedivoire](https://github.com/graciasierra6/Surgo-manitoba/tree/main/aggregate/cotedivoire) | [Cote_dIvoire_analysis_aggregate.R](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R) | [Cote_dIvoire_analysis_aggregate.html](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.html) |
| Sindh | [aggregate/sindh](https://github.com/graciasierra6/Surgo-manitoba/tree/main/aggregate/sindh) | [Sindh_analysis.R](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/sindh/Sindh_analysis.R) | [Sindh_analysis.html](https://github.com/graciasierra6/Surgo-manitoba/blob/main/aggregate/sindh/Sindh_analysis.html) |

GitHub may display the HTML source rather than the formatted report. To read a published report, open its link, use **Download raw file**, then open the downloaded `.html` in a browser.

### Rural/Urban analyses

| Report | R script to open or download |
|---|---|
| Lagos — Rural/Urban | [rural_urban/lagos/Lagos_rural_urban_updated.R](https://github.com/graciasierra6/Surgo-manitoba/blob/main/rural_urban/lagos/Lagos_rural_urban_updated.R) |
| Senegal — Rural/Urban | [rural_urban/senegal/Senegal_rural_urban_updated.R](https://github.com/graciasierra6/Surgo-manitoba/blob/main/rural_urban/senegal/Senegal_rural_urban_updated.R) |
| Côte d’Ivoire — Rural/Urban | [rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R](https://github.com/graciasierra6/Surgo-manitoba/blob/main/rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R) |

There are seven scripts. Sindh has no Rural/Urban report because its SURGO dataset does not provide a documented rural/urban variable.

## Approach 1: RStudio

### 1. Get the R script

Choose one link from the table. On GitHub, use **Download raw file** to save the actual `.R` file. If you downloaded the complete repository instead, find the same file in its folders.

Save a separately downloaded script in a writable folder where you want to keep the analysis outputs. You can keep the confidential input ZIP elsewhere.

### 2. Open it in RStudio

Open RStudio. Select **Session → Restart R**, then **File → Open File** and choose the `.R` script.

### 3. Click Source

Click **Source** at the top of the script editor. This runs the complete file from beginning to end. You do not need to select lines, type `source()`, or run the adjacent `.Rmd` file.

### 4. Select the input ZIP

A file-selection window opens. Select **`SURGO_Manitoba_linkage_inputs_complete.zip`** and click **Open**.

Use the same complete input ZIP for any of the seven scripts. The script selects the correct country's data itself.

### 5. Let the script finish

The script installs and loads its required R packages, reads the inputs, constructs the facility summaries and joins, calculates the scores, fits the models, runs its checks and generates the report. Package installation can make the first run take longer.

A single downloaded script also obtains the public map and display files automatically. Confidential input data are processed locally.

### 6. Open the new HTML report

Wait until the Console shows **Completed** and the usual `>` prompt returns. The Console prints the output location.

Open the resulting `.html` file in your browser. For example, Lagos aggregate produces:

```text
output/rebuilt/aggregate/lagos/Lagos_analysis_aggregate.html
```

In a complete repository, this folder is inside the repository. With one separately downloaded script, it is inside the folder containing that script.

For the four aggregate reports, the new files are:

| Setting | Newly generated HTML, relative to the repository or standalone script folder |
|---|---|
| Lagos | `output/rebuilt/aggregate/lagos/Lagos_analysis_aggregate.html` |
| Senegal | `output/rebuilt/aggregate/senegal/Senegal_analysis_aggregate.html` |
| Côte d’Ivoire | `output/rebuilt/aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.html` |
| Sindh | `output/rebuilt/aggregate/sindh/Sindh_analysis.html` |

Compare the new file with the corresponding published HTML linked in the aggregate table above. The published file stays in its original country folder; the new run saves its report under `output/rebuilt/`.

Repeat these steps with another script to produce another report. You do not need to run the countries in any particular order.

## Approach 2: Claude Code or terminal

### Have Claude Code run the existing scripts

Claude Code can read project files and execute commands. It can therefore carry out the terminal workflow below on your behalf: the same R scripts run with the same input ZIP. See the [official Claude Code overview](https://code.claude.com/docs/en/overview).

**The simple version is: give it the GitHub link, access to the complete input ZIP, and the name of the report you want.** Ask it to follow the repository instructions and run the existing script.

1. Open a Claude Code session that can access project files and run commands.
2. Provide **the repository link:** `https://github.com/graciasierra6/Surgo-manitoba`.
3. Make **`SURGO_Manitoba_linkage_inputs_complete.zip`** accessible in the authorized environment where the analysis will run. For a local session, tell it where the ZIP is stored. A remote session needs the ZIP made available there separately. Keep the ZIP outside Git.
4. Tell it **which report to reproduce**, using the R-script path from the aggregate table, or copy the example below.
5. Ask it to execute the steps below, check the results and give you the generated HTML location. Open that new HTML and compare it with the corresponding published report linked above.

You do not need to tell it which individual datasets to load. The scripts select those files from the complete ZIP and perform the preparation and joins.

**A GitHub link alone is not enough.** The link supplies the code; the confidential datasets are in the separate ZIP. The execution environment also needs R, Python and Pandoc, and access to install the recorded R packages. Ask Claude Code to check these requirements and identify anything missing. A session that can only discuss or inspect the code cannot produce the reports.

#### What Claude Code should do, in order

1. Obtain a working copy of the repository from the GitHub link, or use a copy you have already supplied. Work from the top folder containing `README.md`, `setup.R` and `run_all.R`.
2. Read `README.md`, this guide (`README_RUN_REPORTS.md` in the repository), `docs/RUN_GUIDE.md` and the top of the selected R script. The aggregate scripts and their published HTMLs are in `aggregate/lagos/`, `aggregate/senegal/`, `aggregate/cotedivoire/` and `aggregate/sindh/`.
3. Confirm access to the complete input ZIP. Check that R, Python 3.9 or later and Pandoc are available. Restore the recorded R packages with `Rscript setup.R` when using the complete repository. Identify any unresolved software or input problem before claiming a successful run.
4. Execute the selected R script with `--input-zip` followed by the actual ZIP location, supplied at runtime. The commands in the terminal section below show the syntax. For all four aggregate reports, execute the four aggregate scripts separately; `run_all.R` instead builds all seven reports and the supplementary checks.
5. Let each script finish, inspect its checks and open the newly generated HTML under `output/rebuilt/aggregate/`. Compare it with the corresponding published HTML in `aggregate/`. Run the existing verification steps and report any differences without changing the analysis or reference files to hide them.
6. Return the script and command executed, the repository commit used, whether each run completed, which checks passed or failed, and the location of each new HTML. A description of the code alone does not establish that the analysis ran.

#### Copy and paste this request for all four aggregate reports

> Reproduce the four aggregate SURGO–Manitoba reports using https://github.com/graciasierra6/Surgo-manitoba and SURGO_Manitoba_linkage_inputs_complete.zip, which I will make available to this session.
>
> Read README.md, README_RUN_REPORTS.md and docs/RUN_GUIDE.md. Obtain a working copy of the repository and record its commit. Check R, Python, Pandoc and access to the complete input ZIP. Restore the recorded R dependencies following setup.R.
>
> Execute these existing scripts, passing the ZIP location through --input-zip:
>
> - aggregate/lagos/Lagos_analysis_aggregate.R
> - aggregate/senegal/Senegal_analysis_aggregate.R
> - aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R
> - aggregate/sindh/Sindh_analysis.R
>
> Use the existing data preparation, joins and analysis. Keep the published HTMLs in aggregate/ as the comparison reports. Save new reports under output/rebuilt/aggregate/. Inspect the checks and compare the regenerated reports with the published versions, reporting any differences.
>
> Return a table with each script executed, completion status, checks passed or failed, and the generated HTML location. If execution is blocked, identify what is missing. Do not change the analytical definitions or reference results, and do not claim reproduction without executing the scripts. Keep the input ZIP, extracted data and working outputs out of Git. Do not commit or push anything.

**For just one report**, use the same request but retain only that report's R script. For example, keep only `aggregate/lagos/Lagos_analysis_aggregate.R` for Lagos aggregate. For the complete seven-report rebuild, ask it to follow `run_all.R` instead.

The generated aggregate HTML locations are the same ones listed under Approach 1. Compare the new HTML with the corresponding published report linked in the aggregate table.

This option describes how to start the existing reproduction workflow through Claude Code. The validation statement below concerns the underlying R scripts and reports; a separate Claude Code session has not been tested as part of this guide.

### Or type the terminal commands yourself

Use the commands below if you want to execute the same workflow directly. Keep the confidential ZIP outside the Git repository.

Use the **complete downloaded repository** for these instructions. Open macOS Terminal or Windows PowerShell in its top folder—the folder containing `README.md`, `setup.R` and `run_all.R`.

### 1. Set up the R packages

Run this once:

```text
Rscript setup.R
```

### 2. Provide the input ZIP location

On **macOS**, run:

```sh
printf 'Input ZIP file: '
IFS= read -r INPUT_ZIP
```

On **Windows PowerShell**, run:

```powershell
$InputZip = Read-Host "Input ZIP file"
```

When asked, paste the full location of the confidential input ZIP, without surrounding quotes, and press Enter. The commands below handle spaces in that location.

### 3. Run one report

For **Lagos aggregate**, use:

**macOS:**

```sh
Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip "$INPUT_ZIP"
```

**Windows PowerShell:**

```powershell
Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip "$InputZip"
```

To run another report, replace `aggregate/lagos/Lagos_analysis_aggregate.R` with that report's R-script path from the table above. Keep the input-ZIP part of the command unchanged.

To rebuild **all seven reports and the supplementary checks**, use instead:

**macOS:**

```sh
Rscript run_all.R --input-zip "$INPUT_ZIP"
```

**Windows PowerShell:**

```powershell
Rscript run_all.R --input-zip "$InputZip"
```

### 4. Find the HTML

Open `output/rebuilt/`. Its folders follow the same `aggregate` and `rural_urban` layout as the scripts. Each report's HTML has the same filename as its R script, with `.html` instead of `.R`.

## What is reproduced?

Each script includes the data preparation, geographic linkage, FP-provider filtering, facility aggregation, assignment of supply measures to women, weighted SURGO descriptives, scores, classifications, maps and regression results for its report. The aggregate reports also include the regression complete-case sensitivity checks. The Rural/Urban reports retain their implemented scoring/PCA sensitivities.

Coverage includes all source women; the analyses after coverage focus on women in need of family planning. The scripts document their source dataset names and statistical settings at the top. The full input-file list is in [the dataset inventory](https://github.com/graciasierra6/Surgo-manitoba/blob/main/docs/DATASET_FILENAMES.md).

The supplied complete input ZIP was checked against the project's recorded file checksums and matches the archive used in the validated rebuild. Validation reproduced the seven reports on macOS. Windows instructions are included, but a complete native Windows run has not yet been verified.

## If something stops

| What you see | What to do |
|---|---|
| A package/version conflict | Choose **Session → Restart R**, reopen the script and click **Source** again. |
| Package installation fails | Check internet access and read the first error. Some packages need compatible system software; ask for help with that error rather than changing the analysis. |
| No ZIP-selection window | Start a fresh RStudio session and click **Source** on the complete script. A supplied input setting can bypass the window. |
| ZIP missing or incomplete | Select the confidential file whose name ends in **`_complete.zip`**. Do not select the repository ZIP. |
| Python or Pandoc missing | Install/configure the named software, restart RStudio or the terminal, then rerun. RStudio normally provides Pandoc for Approach 1. |
| `Rscript` not recognized | Use Approach 1 in RStudio, or make R available to your terminal. |

Keep the input ZIP, extracted data and working outputs private. Working folders can contain joined woman-level data and fitted models; do not upload those folders to GitHub.
