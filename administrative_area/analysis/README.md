# Administrative-area analysis

These four country modules generate the current reports using whole-area facility
profiles, without a Rural/Urban supply split. They extend the existing verified
administrative-area join and reaggregate only confirmed FP providers before
constructing analysis scores and assigning supply to individual women.

| Country folder | R/Rmd entry basename | Geographic level |
|---|---|---|
| `lagos/` | `Lagos_analysis_aggregate` | LGA |
| `senegal/` | `Senegal_analysis_aggregate` | Département |
| `cotedivoire/` | `Cote_dIvoire_analysis_aggregate` | Département-equivalent area |
| `sindh/` | `Sindh_analysis` | District |

Run `Rscript R/run_aggregate_reports.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"`
from the repository root. `--countries` selects a subset; `--join-dir` and
`--cache-dir` reuse documented verified inputs. Output/cache/report/Python/Pandoc
locations are runtime parameters. Each country also has a direct R entry point
and an Rmd. Shared input preparation is `report_pipeline.R`; shared model plotting
is `plot_models.R`. Required original surveys, the approved Sindh need derivation,
facility inputs, dictionaries and lookups come from the external input bundle.

The reports follow coverage; weighted need-group use/intention; joint heatmaps,
zero-mean z-score classifications, weakest service components and most elevated
reported concerns; five modern-use models; definitions and checks. Senegal,
Côte d’Ivoire and Lagos retain their scoring/PCA appendices. Sindh uses z-scores
only. All numerical definitions, denominators, geographic qualifications and
survey-design limits are documented in the country READMEs and HTML reports.

Final HTML filenames are stable, so rerunning replaces the corresponding report
instead of adding version-suffixed copies. The separate earlier residence-specific
reports and join module remain available; Sindh's legacy report entry delegates to
this district module. Base joins keep upstream parity; report preparation writes
FP-restricted working joins and models only under ignored output directories.

Run `python3 tests/check_aggregate_release.py` for release checks. Numerical checks
run within each R workflow. Only code, documentation, required existing public
display references, aggregate build metadata and HTML findings belong in GitHub.
Do not upload original input datasets, caches, fitted objects or woman-level outputs.
