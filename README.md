# SURGO–Manitoba

Join individual SURGO women to their recorded University of Manitoba facility
supply environment in Lagos, Senegal, Côte d’Ivoire and Sindh. Original woman
records and survey variables are retained. Supply profiles are contextual area
measures; they do not identify the facility a woman attended.

## Inputs and data protection

Original surveys, cleaned facility inputs, intermediate joins, fitted models and
woman-level outputs stay outside GitHub. Supply external inputs at runtime through
`SURGO_MANITOBA_INPUT_ZIP`, `--input-zip` or documented existing-join/cache arguments.
The input ZIP is checked against its manifest. No personal computer paths or
credentials belong in shared code, reports or documentation. Public display assets
and aggregate report/build-validation metadata are included; input survey datasets
are not. The root `.gitignore` excludes working data, caches and generated datasets.

Install the declared dependencies:

```sh
Rscript R/install_dependencies.R
```

Python 3 formats saved aggregate results; statistical calculations and model fits
use R. Pandoc must be available; its directory can be supplied at runtime with
`--pandoc-dir` or `RSTUDIO_PANDOC`.

## Two geographic join modules

| Module | Join level | Documentation |
|---|---|---|
| `administrative_area/` | Lagos LGA, Senegal/CDI département-equivalent area, Sindh district | [Administrative-area join](administrative_area/README.md) |
| `urban_rural/` | Administrative area plus Rural/Urban, where available | [Residence-specific join](urban_rural/README.md) |

```sh
Rscript administrative_area/R/run_join.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP" --geography-version both
Rscript urban_rural/R/run_join_urban_rural.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP" --geography-version both
```

Base joins retain the original supply-summary definitions for upstream reconciliation.
The current administrative-area **analysis preparation then reaggregates supply
among confirmed FP providers**, writes the analysis-specific woman-level join to
ignored working output, and uses that profile in all subsequent calculations.
The residence-specific join module is unchanged by this analysis update. Sindh's
women file has no documented Rural/Urban variable and is matched by district only.

## Current administrative-area reports

Reports replace their earlier versions under the same filenames. One runner
builds all four, with inputs supplied at runtime:

```sh
Rscript R/run_aggregate_reports.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

A verified join and extracted bundle can be reused:

```sh
Rscript R/run_aggregate_reports.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --cache-dir "$SURGO_MANITOBA_CACHE_DIR" \
  --countries all
```

| Geography | Report | Source women | Women in need | Common model sample |
|---|---|---:|---:|---:|
| Lagos | [Lagos_analysis_aggregate.html](reports/Lagos_analysis_aggregate.html) | 4,555 | 2,159 | 1,896 |
| Senegal | [Senegal_analysis_aggregate.html](reports/Senegal_analysis_aggregate.html) | 7,169 | 1,905 | 1,558 |
| Côte d’Ivoire | [Cote_dIvoire_analysis_aggregate.html](reports/Cote_dIvoire_analysis_aggregate.html) | 10,859 | 4,292 | 3,951 |
| Sindh | [Sindh_analysis.html](reports/Sindh_analysis.html) | 10,364 | 3,274 | 3,115 |

Matching R entry scripts, Rmds, definitions, model and verification helpers are in
`administrative_area/analysis/<country>/`. See the [analysis guide](administrative_area/analysis/README.md)
and each country README for reproduction and qualifications. The separate earlier
residence-specific Lagos, Senegal and Côte d’Ivoire reports remain available under
`reports/Lagos_analysis.html`, `reports/Senegal_analysis.html` and
`reports/Cote_dIvoire_analysis.html`. Their workflow is `R/run_reports.R`.
Sindh always delegates to the current district workflow, since residence splitting
is unavailable there.

The updated Lagos residence-specific analysis is a separate report:
[Lagos_rural_urban_updated.html](reports/Lagos_rural_urban_updated.html). Its
[module guide](urban_rural/analysis/lagos/README.md) documents exact FP-provider
LGA–Rural/Urban assignment, the Ibeju-Lekki–Urban coverage limitation and
reproduction. Run it with:

```sh
Rscript urban_rural/analysis/lagos/Lagos_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

Its planning heatmaps/maps retain LGA-wide summaries. Its models use residence
profiles with a flagged LGA-wide fallback when an exact residence cell has zero
FP providers. Undefined values are not zero-filled. The old three
residence-specific HTMLs and the four current aggregate reports are preserved.

The same updated residence-specific workflow is available for Senegal:
[Senegal_rural_urban_updated.html](reports/Senegal_rural_urban_updated.html), with
[source and reproduction notes](urban_rural/analysis/senegal/README.md).

```sh
Rscript urban_rural/analysis/senegal/Senegal_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

Senegal uses matching département–Rural/Urban FP-provider means, with flagged
département-wide fallback for 16 empty rural FP-provider cells. Facility residence
is a health-district lookup proxy; this limitation and all fallback groups are
documented in the report. Planning heatmaps/maps remain département-wide, while
models and PCA use personal demand and assigned residence/fallback supply.

The current Côte d’Ivoire residence workflow is also available:
[CotedIvoire_rural_urban_updated.html](reports/CotedIvoire_rural_urban_updated.html),
with [source and reproduction notes](urban_rural/analysis/cotedivoire/README.md).

```sh
Rscript urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

It uses matching département–Rural/Urban FP-provider means for individual models
and PCA while preserving département-wide planning summaries. The empty-cell
fallback rule is implemented, but no fallback is required in the current source.
Four unmatched survey areas and Daoukro's undefined ratios remain explicitly
unresolved; they are not assigned zero or another département's services.

See the [updated residence-analysis guide](urban_rural/analysis/README.md) for
shared reproduction instructions and supplementary checks.

## Population, measures and scores

Coverage precedes the need restriction. From Section 2 onward, Lagos, Senegal and
Côte d’Ivoire use original SURGO FPN2 = 1. Sindh uses the previously authorized
instrument-based `fpn2_derived`, because its source FPN2 field is entirely code 95.
The existing Sindh SA1 = 95/97 exclusion policy is retained and explicitly qualified;
no permanent-protection additions are made. These flags are not the DHS unmet-need
algorithm.

Require FP provision: Lagos/Sindh Q506 Yes; Senegal s5_6 Yes; Côte d’Ivoire s2_5_sp
Yes. The Côte d’Ivoire filter is general FP service provision and does not also
require Yes to its separate on-site question s5_1. Within each geographic area,
average valid facility values equally, retaining valid zeros/FALSE and excluding
unknown or undefined values separately by measure. No facility weights or automatic
zero filling. Preserve the original cleaned UoM definitions.

Six supply inputs capture short/long-acting offering, free-product proportion,
trained-provider proportion, daily provision of offered modern-method classes,
at least one LARC in stock, and original cleaned stockout burden. Counseling and
popular-product stock are no longer selected components. Six demand domains cover
general healthcare permission, anticipated stigma, PB2A-only health beliefs,
future-fertility concerns, method confidence under the retained knowledge label,
and opposition to FP. MC5 measures confidence, not an objective knowledge test.

For geographic classification, calculate weighted demand percentages and unweighted
FP-facility summaries. Standardize each component against its side's complete-area
reference mean and sample SD, then average six z-scores. Reverse stockout burden
to continuity before supply scoring. Flag demand above zero and supply readiness
below zero. These are relative patterns, not causal binding constraints or absolute
service standards. Tables show the weakest service components and most elevated
reported concerns, ordered by component z-scores rather than intervention effects.
Sindh uses only this scoring approach. Other countries retain their documented
alternative-scoring/PCA appendices.

## Models and interpretation

Five primary models all have **current modern-method use** as outcome: personal
demand score; assigned supply risk; both scores; six personal demand indicators;
and those indicators plus supply risk. Each country uses one common complete model
sample. Personal demand scores retain each woman's own responses; supply risk is
the negative of assigned readiness: whole-area profiles in the aggregate workflow,
or area–Rural/Urban profiles with flagged whole-area fallbacks in the updated
residence workflow.

All models retain final SURGO weights and adjust for age, age squared, education
and fertility goal. Partnership and Rural/Urban are included in Lagos, Senegal and
Côte d’Ivoire. In Sindh, partnership is constant in the modeled group and Rural/Urban
is unavailable. Primary uncertainty uses geographic CR2 clustering, Satterthwaite
intervals, HTZ block tests and Holm adjustment within each joint model's two tests.
No random effects or interactions. Available recorded sampling-cluster fields
support survey summaries/inference checks in Senegal, Côte d’Ivoire and Sindh.
Original strata/FPC are unavailable; Lagos EA/PSU remains unconfirmed and its
overview design intervals are withheld. Country reports explain these limits.

Important qualifications remain: limited geographic contexts; component overlap;
uncertainty conditional on estimated scores; incomplete source responses and some
geographic-linkage qualifications. Sindh training proportions are defined for only
278/2,823 FP facilities. Côte d’Ivoire has four unmatched survey areas and Daoukro's
undefined free/daily measures, leaving five supply-unassessed areas. Verification
checks implementation; it does not establish power, causal dominance or an optimal
investment allocation.

## Checks and outputs

```sh
python3 tests/check_aggregate_release.py
python3 tests/check_rural_urban_release.py
```

Each workflow independently checks raw-field preservation, FP eligibility,
geographic assignments, valid-response means, scoring direction/cutoffs, personal
scores, weighted model coefficients, geographic covariance and report contents.
The release checks inspect the actual source/report files and decoded embedded
assets. `reports/aggregate_build_manifest.json` records relative report names,
hashes, software versions and verification status. The corresponding updated
residence record is `reports/rural_urban_build_manifest.json`. Neither manifest
contains input datasets.
Working inputs and outputs remain in ignored directories. Upload source code,
documentation and the HTML reports, without local input/output folders or ZIPs.
