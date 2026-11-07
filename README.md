# SURGO–Manitoba

Join individual SURGO women to their local University of Manitoba facility supply
environment in Lagos, Sindh, Senegal and Côte d’Ivoire, then generate the four
country analysis reports. Every source woman and original survey variable is
retained in the joins.

## Project structure

| Folder | Purpose |
|---|---|
| `administrative_area/` | Existing LGA/department/district aggregation and woman-level join. [Documentation](administrative_area/README.md) |
| `urban_rural/` | Existing administrative area + Rural/Urban aggregation and join. [Documentation](urban_rural/README.md) |
| `analysis/` | **New addition:** four country Rmds, analysis scripts, shared figures/style and public map assets. [Methods and country decisions](analysis/README.md) |
| `reports/` | Four self-contained HTML reports and their build manifest. |
| `R/` | Dependency installation and the single report-generation entry point. |
| `tests/` | Calculation, linkage-to-analysis, report and reproducibility checks. |

The two existing linkage modules are unchanged from the GitHub repository and
remain distinctly separate and independently runnable. Today's analysis is an
additional layer that consumes their outputs. It does not replace their geography,
aggregation or unmatched-data policies. New analysis filenames describe their
purpose; there are no duplicate country workflow runners.

## Apply today’s addition

Copy the addition package’s contents into the existing repository root. It adds
`analysis/`, `reports/`, `R/`, tests and dependency records, and updates this README.
It contains no replacement files for `administrative_area/` or `urban_rural/`.

## Inputs and setup

Keep the existing `SURGO_Manitoba_linkage_inputs.zip` outside the repository, for
example in synced OneDrive storage. Its 54 source files are checksummed. No input
files need to be added to that bundle for these reports. Public display boundaries
are included in `analysis/assets/`; no previous HTML or temporary folder is needed.

Requires R, Python 3.9+ (standard library only), and Pandoc. Install recorded R
package versions from `renv.lock`:

```sh
Rscript R/install_dependencies.R
```

Pandoc must be discoverable by `rmarkdown`. If needed, set `RSTUDIO_PANDOC` to your
Pandoc directory. Spatial R packages require compatible GDAL/GEOS/PROJ/UDUNITS
libraries. See the [analysis guide](analysis/README.md) for software details.

## Generate the four reports

From the repository root, set your external input location and run:

```sh
export SURGO_MANITOBA_INPUT_ZIP="/path/to/SURGO_Manitoba_linkage_inputs.zip"
Rscript R/run_reports.R
```

The command prepares and validates the joins, calculates the country summaries,
scores and models, runs their checks, and knits the four Rmds. Final HTMLs go to
`reports/`; private intermediate data and fits go to ignored `output/report_build/`.
All input, cache and output locations are runtime parameters. Use `--help` for
options, or select one country with `--countries lagos`.

| Report | Source women | Women in need | Common model sample |
|---|---:|---:|---:|
| [Lagos](reports/Lagos_analysis.html) | 4,555 | 2,159 | 1,865 |
| [Sindh](reports/Sindh_analysis.html) | 10,364 | 3,274 | 3,048 |
| [Senegal](reports/Senegal_analysis.html) | 7,169 | 1,905 | 1,445 |
| [Côte d’Ivoire](reports/Cote_dIvoire_analysis.html) | 10,859 | 4,292 | 3,768 |

Coverage maps use all source records. Subsequent results use original SURGO FPN2 = 1,
except Sindh's explicitly documented supplied reconstruction. Women's results use
survey weights; facility aggregation is unweighted. Models describe adjusted
cross-sectional associations, with geographic clustered inference and no random
effects. Missingness and incomplete survey-design information remain disclosed.

## Run the checks

```sh
Rscript urban_rural/tests/test_urban_rural.R
python3 tests/verify_report_reproduction.py --work-dir output/report_build
python3 tests/check_release.py
```

The saved-result check covers 27,149 values across all four analyses. The report
manifest records software versions and input/report hashes without runtime paths.
Only source code, documentation, public display assets, aggregate test references
and the four HTMLs belong in the repository. Input ZIPs, raw surveys, woman-level
datasets, fitted objects, caches and logs remain excluded.
