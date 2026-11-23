# SURGO–Manitoba

Link SURGO women to recorded University of Manitoba facility characteristics in
Lagos, Senegal, Côte d’Ivoire and Sindh, and examine demand barriers, supply side
constraints and their associations with current modern contraceptive use.

## Open a report or run its code

Each country folder contains **one complete country R script, its matching Rmd,
and the current HTML report together**. Run the R script; it executes preparation,
analysis, models, implemented sensitivities, numerical checks and HTML rendering.
Country calculations are included in that script rather than scattered among
country helper files. Common ingestion/statistical utilities and renderers live
in `support/`; keep the complete repository when running any report.

| Geography | Aggregate: whole administrative area | Rural/Urban: residence-specific supply |
|---|---|---|
| Lagos | [Report](aggregate/lagos/Lagos_analysis_aggregate.html) · [R script](aggregate/lagos/Lagos_analysis_aggregate.R) · [Rmd](aggregate/lagos/Lagos_analysis_aggregate.Rmd) | [Report](rural_urban/lagos/Lagos_rural_urban_updated.html) · [R script](rural_urban/lagos/Lagos_rural_urban_updated.R) · [Rmd](rural_urban/lagos/Lagos_rural_urban_updated.Rmd) |
| Senegal | [Report](aggregate/senegal/Senegal_analysis_aggregate.html) · [R script](aggregate/senegal/Senegal_analysis_aggregate.R) · [Rmd](aggregate/senegal/Senegal_analysis_aggregate.Rmd) | [Report](rural_urban/senegal/Senegal_rural_urban_updated.html) · [R script](rural_urban/senegal/Senegal_rural_urban_updated.R) · [Rmd](rural_urban/senegal/Senegal_rural_urban_updated.Rmd) |
| Côte d’Ivoire | [Report](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.html) · [R script](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R) · [Rmd](aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.Rmd) | [Report](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.html) · [R script](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R) · [Rmd](rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.Rmd) |
| Sindh | [Report](aggregate/sindh/Sindh_analysis.html) · [R script](aggregate/sindh/Sindh_analysis.R) · [Rmd](aggregate/sindh/Sindh_analysis.Rmd) | Not available: no documented woman Rural/Urban field |

## Reproduce everything

1. Obtain the authorized complete input ZIP separately. Input datasets are never
   uploaded to this repository.
2. Restore packages with `Rscript setup.R` (versions recorded in `renv.lock`).
3. Set `SURGO_MANITOBA_INPUT_ZIP` to the ZIP location in the running environment.
4. Run:

```sh
Rscript run_all.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

This starts from the input ZIP, rebuilds the joins, seven country reports and eight
supplementary checks, and compares them with the validated report references.
Rebuilt files go to ignored `output/rebuilt/`, with the same country layout as
above. Published reference reports are preserved. Calculations and private joins
stay in ignored `output/build/`; verified extracted inputs stay in ignored `data/cache/`.

For one report, run its R script directly:

```sh
Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

The individual script defaults to replacing its adjacent HTML after its numerical
checks pass. Use `--output-dir` to build elsewhere. See the
[run guide](docs/RUN_GUIDE.md) for all seven commands, requirements and outputs.

## Where things are

| Folder/file | Contents |
|---|---|
| `aggregate/<country>/` | Whole-LGA/département/district analysis: R, Rmd, HTML and a short README. |
| `rural_urban/<country>/` | Residence-specific analysis: R, Rmd, HTML and a short README; three settings. |
| `data_checks/` | Coverage and score-comparison HTMLs plus their cumulative rebuild script/templates. |
| `support/` | Shared input/join functions, survey/scoring helpers, display assets and Python renderers. These run automatically. |
| `docs/` | Exact input filenames, full inventory, source catalog, run guide and methods. |
| `validation/` | Report reference signatures, source/release checks and validation evidence. |
| `setup.R`, `run_all.R` | Package setup and complete reproduction entry point. |

The previous `administrative_area/analysis/`, `urban_rural/analysis/`, top-level
`analysis/` and `reports/` layout has been replaced. Historical report/code versions
remain in Git history; they are not additional current report entry points.

## Input files and source links

- [Clean ZIP filename list](docs/DATASET_FILENAMES.md): exact basenames and ZIP-relative members.
- [Full input inventory](docs/INPUTS.md): all names, purposes, sizes and SHA-256 checksums.
- [Dataset catalog](docs/dataset_catalog.json): primary inputs, identifiers, weights and pending source mappings.

The complete ZIP contains **14 primary analytical inputs plus 42 supporting,
reference and manifest members: 56 total**. Do not flatten folders or omit reference
members; the runner verifies every manifest-listed file. Databricks identifiers
and links remain pending, rather than inferred from filenames. External locations
and credentials are runtime parameters, never personal paths saved in the code.

## Methods and today's updates

Coverage uses all women. Subsequent analyses use women in need of FP: original
SURGO `fpn2/FPN2 = 1` in Lagos, Senegal and Côte d’Ivoire; the approved instrument-based
`fpn2_derived` in Sindh. SURGO estimates use supplied weights through `survey`;
models use `svyglm()`, `ids = ~1`, no strata/FPC, and separately calculated
administrative-area CR2/Satterthwaite inference. No random effects are used.

Facility summaries use confirmed FP providers and valid responses separately for
each measure, with equal facility weight, retaining genuine zeros/FALSE. Whole-area
analyses assign area averages. Residence analyses assign matching area–Rural/Urban
profiles; cells with no confirmed FP providers use a flagged same-area fallback
where available. An undefined component is not automatically zero-filled or replaced.

Both scores run higher = more barriers/constraints. Geographic scores average six
component z-scores; above zero is the primary flag. Maps also show 0.5, 1 and 1.5
final-composite SD thresholds. Personal demand responses and assigned supply are
retained in individual models. [Methods and limitations](docs/SURVEY_AND_SCORING.md).

The four aggregate reports include the latest **regression Sensitivity checks**:
included/excluded profiles, formula-specific available-case models, and
response-weighted joint models with two-stage geographic jackknife intervals.
Sindh includes the corrected training reconstruction from raw Q406 text and
checkbox responses. The Rural/Urban reports retain their existing scoring/PCA
sensitivities; today's new regression-completeness appendix was scoped to aggregate
analyses. These distinctions are stated in the country READMEs.

## Validation and sharing

```sh
python3 validation/check_release.py
```

Numerical verification also runs inside every country script. `run_all.R` checks
rebuilt report values, embedded tables, figures and maps against versioned references.
Validation establishes implementation/reproduction for the documented inputs, not
causal identification or absence of source-data limitations.

Only code, documentation, approved aggregate reports, public display assets and
validation metadata belong in uploads. Input datasets, extracted caches, working
joins and fitted objects are ignored and must remain outside shared commits.
