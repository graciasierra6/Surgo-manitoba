# Updated residence-specific analyses

These modules reproduce the three current Rural/Urban reports. They supplement
the separate administrative-area analysis; neither join module is changed.

| Geography | R entry script and matching Rmd | HTML report |
|---|---|---|
| Lagos | `lagos/Lagos_rural_urban_updated.R` | `reports/Lagos_rural_urban_updated.html` |
| Senegal | `senegal/Senegal_rural_urban_updated.R` | `reports/Senegal_rural_urban_updated.html` |
| Côte d’Ivoire | `cotedivoire/CotedIvoire_rural_urban_updated.R` | `reports/CotedIvoire_rural_urban_updated.html` |

All paths in this table are project-relative; entry-script paths are relative to
this guide. Each country directory contains definitions, aggregation/assignment,
models, PCA, rendering and independent verification helpers. Run the scripts from
the project root, retaining the existing project structure and shared helpers.

## Reproduce from the external input bundle

Install the documented project dependencies and make Pandoc available, as described
in the root README. Supply the authorized input bundle through a runtime environment
variable or a command-line parameter. Input datasets are not part of the repository.

```sh
Rscript urban_rural/analysis/lagos/Lagos_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
Rscript urban_rural/analysis/senegal/Senegal_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
Rscript urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

Each entry builds/validates the joins, aggregates FP facilities, assigns supply,
calculates geographic and personal scores, fits the main/PCA models, verifies
results, and knits its matching Rmd. HTML reports embed their figures and maps.
Working joins, calculation tables, fitted models and caches remain in ignored
`output/` or `data/` directories and must not be uploaded.

Options include `--work-dir`, `--output-dir`, `--cache-dir`, `--join-dir`,
`--python` and `--pandoc-dir`. An existing verified residence join can be supplied
with `--join-dir` and its corresponding `--cache-dir`; using `--input-zip` as well
checks that the join matches the bundle. Do not share a cache being initialized
by simultaneous input extraction. See each country README for exact commands.

## Definitions and limitations

From Section 2 onward, all three reports use original SURGO `FPN2 = 1`, consent
and positive final survey weights. Demand inputs are healthcare permission,
anticipated stigma, PB2A-only health beliefs, reversible-method fertility concerns,
method confidence (retained knowledge label), and FP opposition. Supply means use
only confirmed FP providers and valid responses separately by measure. True
zero/FALSE values are included; unknown or undefined values are not zero-filled.

The six supply components are short/long-acting offering, free-product proportion,
trained personnel proportion, daily provision, at least one LARC in stock and
original cleaned stockout burden. Reverse burden to continuity before scoring.
Geographic z-scores average six components and use zero cutoffs: demand above zero,
favorable supply readiness below zero. Unknown supply remains unassessed.

Planning heatmaps and classifications combine all FP facilities in each
administrative area. Individual models/PCA instead assign each woman her own
area–Rural/Urban means. A zero-provider residence cell receives a flagged whole-area
fallback if available. Existing exact-cell providers with an undefined component
do not trigger a component fallback. Both resolutions remain explicit.

Lagos has one fallback cell, Ibeju-Lekki–Urban (62 women in need). Senegal has
16 rural fallback cells (522 women in need); its facility residence is a
health-district lookup proxy, with nine unknown proxy records. Côte d’Ivoire has
no fallback recipients: four surveyed areas remain unmatched, and Daoukro has
partially undefined exact profiles. These distinctions are retained in the reports.
Sindh has no SURGO Rural/Urban field and continues to use the district-level report;
there is no new Sindh residence-specific file.

Models use modern use, personal demand, assigned supply risk, agreed covariates,
final survey weights and administrative-area-clustered CR2 inference. Recorded
sampling clusters are used where documented; original strata/FPC are incomplete.
No random effects or interactions are added. Categories are relative planning
signals; neither classification nor regression identifies causal binding constraints.

## Supplementary checks

- `analysis/check_demand_population.R` and its Python renderer reproduce the
  all-women versus need-group classification comparison without changing main
  results. Use the current aggregate or residence analysis output for each country;
  Sindh uses its existing district output.
- The Senegal and Côte d’Ivoire `update_data_checks.R` scripts append/replace their
  residence-coverage sections in a caller-supplied existing Data checks HTML.
  Their tables are recalculated from the analysis frames; earlier audit sections
  are preserved from the supplied HTML, not rebuilt from raw inputs by these
  updaters. The checked reference report is included as `reports/Data_checks.html`.
- Côte d’Ivoire `check_classification_changes.R` compares Friday and updated saved
  scoring inputs across all combinations of the documented changes. Friday saved
  analysis outputs must be supplied separately; this optional diagnostic does not
  require or upload individual records in its exported comparisons.

The main reports can be reproduced from the input bundle without running these
supplementary diagnostics. Run the portable source/report checks with:

```sh
python3 tests/check_rural_urban_release.py
```
