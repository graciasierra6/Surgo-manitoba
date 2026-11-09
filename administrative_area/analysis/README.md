# Aggregate reports: Lagos, Senegal and Côte d’Ivoire

These workflows use **administrative-area supply averages**: LGA in Lagos and
department in Senegal and Côte d’Ivoire. Every woman receives the facility profile
for her own area. Rural/Urban residence remains a covariate but does not determine
her supply profile. The existing residence-specific workflows in `analysis/` and
the original four reports remain separate.

## Where the files belong

Copy this addition into the existing repository root, preserving folder names.
Every packaged file is new; no existing source, README, join or report is replaced.
Do not put the R scripts or Rmd files in `reports/`.

| Repository location | Purpose |
|---|---|
| `R/run_aggregate_reports.R` | One entry point to build the three aggregate reports. |
| `administrative_area/analysis/report_pipeline.R` | Checked administrative join preparation and runtime input verification. |
| `administrative_area/analysis/lagos/` | Lagos entry script, Rmd, calculation/rendering helpers and country README. |
| `administrative_area/analysis/senegal/` | Senegal entry script, Rmd, helpers and country README. |
| `administrative_area/analysis/cotedivoire/` | Côte d’Ivoire entry script, Rmd, helpers and country README. |
| `administrative_area/analysis/plot_models.R` | Aggregate-only plotting helper, including signed PCA interpretation. |
| `reports/*_aggregate.html` | Three self-contained final HTML reports. |
| `reports/aggregate_build_manifest.json` | Relative report names, hashes, software versions and verification status. |

The entry script and Rmd in each country folder use the same basename as its HTML:
`Lagos_analysis_aggregate`, `Senegal_analysis_aggregate`, or
`Cote_dIvoire_analysis_aggregate`.

## Generate all three

Install the existing project's recorded dependencies with
`Rscript R/install_dependencies.R`. Python 3.9+ and Pandoc are also required.
If needed, provide Pandoc's directory with `--pandoc-dir` or `RSTUDIO_PANDOC`.

Set `SURGO_MANITOBA_INPUT_ZIP` to the external, checksummed 54-file input bundle,
then run from the repository root:

```sh
Rscript R/run_aggregate_reports.R
```

The command prepares the administrative join with checked geography directly;
it does not run the Rural/Urban join. Final reports are written to `reports/`.
Private input caches, woman-level frames, fitted objects and working outputs go
to ignored `data/` and `output/` locations.

```sh
Rscript R/run_aggregate_reports.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --countries lagos,senegal,cotedivoire \
  --work-dir output/aggregate_reports \
  --cache-dir data/cache \
  --output-dir reports
```

To reuse a verified administrative join and extracted input cache:

```sh
Rscript R/run_aggregate_reports.R \
  --join-dir administrative_area/output \
  --cache-dir administrative_area/data/cache
```

If an input ZIP is also supplied, its hash and member checksums are revalidated.
A legacy join container with an `admin_reference/` subdirectory is accepted for
compatibility. Every external location is an argument or environment variable;
relative internal paths are resolved from the repository root.

Individual country entry scripts accept `--join-dir`, `--cache-dir`, `--work-dir`,
`--output-dir`, `--input-zip`, `--python` and `--pandoc-dir`. Their Rmd files accept
the corresponding parameters and call the same calculation functions.

## Population and report content

| Report | Source women | Original SURGO need group | Common model sample |
|---|---:|---:|---:|
| Lagos | 4,555 | 2,159 | 1,865 |
| Senegal | 7,169 | 1,905 | 1,445 |
| Côte d’Ivoire | 10,859 | 4,292 | 3,854 |

Coverage maps use all source records. Section 2 onward uses the original supplied
`fpn2/FPN2 = 1` classification with documented weight/consent eligibility. No older
`need_n` recoding or extra hysterectomy/sterilization additions are applied.

The main reports retain all six selected demand and supply components, including
FP counselor availability. They contain the overview, joint heatmap, five scoring
approaches, main classification map, frequency summaries, detailed supply-only
and demand-only priority tables, all-area planning table, adjusted modern-use
models and PCA appendix. Côte d’Ivoire also retains its stock-quality diagnosis.
The complete-facility and complete-demand comparison sections are **not displayed**.
Historical diagnostic outputs are internal and do not replace the main estimates.

Main geographical summaries use valid responses separately for each component;
they do not require the same complete facilities or women for every percentage.
Main regression models separately require complete outcome, predictors and
covariates, and use a common woman sample across model specifications.

## Important interpretation limits

- These are contextual geographic assignments, not links to the facility attended.
- Demand means use women's final survey weights. Facilities have no supplied
  sampling weights and contribute equally to their valid-response means.
- Main geographical indices average component z-scores with equal component
  weights. Classification compares final demand/readiness scores with separate
  medians; it does not use the component mean as the classification threshold.
- Unknown facility ratios, inapplicability, unanswered items, noninterviewed
  records and upstream zero/FALSE coding are distinct. Retain coverage tables
  and the documented one-facility Lagos free-provision exception.
- Models use geographic CR2/Satterthwaite inference, covariates and no random
  effects/interactions. Full original survey design is not claimed where EA,
  strata or finite-population information is unavailable.
- These scripts reproduce the supplied snapshot; explicit sample assertions
  intentionally stop if a different dataset/wave requires reviewed definitions.
- Numerical checks validate reproduction, not missing-data assumptions, adequate
  power, representativeness, construct validity or causal binding constraints.

Run `python3 tests/check_aggregate_release.py` to check both the original release
and these additions.

Country READMEs document their specific geography, stock coding, counseling
coverage, sample exclusions and PCA limitations. Never upload raw surveys,
facility-level exports, caches, model objects, logs or input ZIPs with these files.
