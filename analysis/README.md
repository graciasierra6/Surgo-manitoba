# Country analysis

Run **`Rscript R/run_reports.R`** from the repository root. This is the only analysis
workflow entry point. Each country Rmd can also be knitted independently with its
input parameters; it calls the same shared pipeline.

## Where to work

- **Change a measure:** edit that country's `*_definitions.R`.
- **Change summaries or geographic scores:** edit `build_analysis.R`.
- **Change a model:** edit `fit_models.R`.
- **Change PCA construction:** edit `build_pca_scores.R`.
- **Change a figure:** edit the shared `common/plot_models.R`.
- **Change report wording/layout:** edit the country's `render_analysis.py` or the
  shared `common/report.css` and `common/report.html`.
- **Check calculations/results:** use the corresponding scripts in `tests/`.

Country-specific calculations remain explicit because survey fields, routing,
covariates and geography differ. Plotting, styling and orchestration are shared.
Historical comparison scripts, figure-refresh wrappers, one-off diagnostics and
alternate Python workflow runners are not part of the active repository.

## Workflow and parameters

`common/pipeline.R` runs the stages in order: prepare the existing joins → build
country summaries/scores → prepare display maps → fit the primary models → build
PCA scores and sensitivity models → verify calculations → format and knit → verify
the final HTML. The existing urban/rural entry point automatically prepares its
administrative reference. It does not merge the two linkage modules into one.

| Option | Default |
|---|---|
| `--input-zip` | `SURGO_MANITOBA_INPUT_ZIP` |
| `--countries` | `all`, or a comma-separated subset of `lagos,sindh,senegal,cotedivoire` |
| `--output-dir` | `reports` |
| `--work-dir` | `output/report_build` |
| `--cache-dir` | `data/cache` |
| `--join-dir` | Blank: build joins. Alternatively, reuse a verified urban/rural output including its administrative reference. |
| `--python` | `python3`, or `SURGO_MANITOBA_PYTHON` |

All relative paths resolve from the repository root. Quote paths containing spaces.
Reused joins must match the input-bundle hash and contain both geographic versions.
Original source records and joins are never overwritten by country analysis.

Dependencies are recorded in `renv.lock` (R 4.6.0 reference environment). The root
installer restores a project library, which the runner loads when present.
`Rscript R/install_dependencies.R --latest` explicitly selects current CRAN versions
instead; small numerical or formatting differences are then possible. Python uses
only its standard library. R Markdown requires Pandoc; `sf` requires compatible
system spatial libraries. No TeX, Databricks connection or GitHub sign-in is needed.
Checks intentionally target the documented input snapshot. A revised survey wave
requires review of definitions/reference checks, rather than bypassing assertions.

## Country choices retained from the current reports

| Country | Need group and assigned supply |
|---|---|
| Lagos | Original `fpn2 = 1`. Main sample, exact LGA–residence supply. Retain the approved Ibeju-Lekki Urban free-provision exception and the LGA fallback for its popular-stock component only. |
| Sindh | Supplied `fpn2_derived`, independently reproduced. Retain its approved SA1 special-code exclusion policy. District-only supply using the source crosswalk; no woman's residence field is invented. The 881 facilities lacking source district names are not silently added. |
| Senegal | Original `FPN2 = 1`. Checked department–residence supply, with flagged department-wide fallback only where a residence profile is absent: 1,815 source women, including 522 in need. Facility residence is a health-district lookup proxy; Mbacke's Urban-labelled facilities do not establish absence of Rural services. |
| Côte d’Ivoire | Original `FPN2 = 1`. Exact checked department–residence supply, with no geographic or component fallback. Four departments have no facility match; other undefined components, including Daoukro free provision, remain missing. |

These are analysis-specific selections of the existing join outputs. The upstream
modules and their default policies remain unchanged. Country-specific field names,
item coding and limitations are also described in each HTML's definitions section.
The older broad `need_n` population and historical oversample comparisons are not
used in these reports.

## Scores, outcomes and uncertainty

Six demand domains retain missing-aware coding: any known positive establishes a
concern; zero requires valid negatives on every included item; other combinations
remain unresolved. Planning heatmaps use weighted valid-domain prevalence among
women in need. Supply planning summaries combine all included facilities in each
administrative area using original valid-response means and no facility weights.

Five geographic scoring methods are compared: rank average, mean percentages,
median/IQR, median count and average z-score. The primary z-score classification
flags demand above its score median and favorable supply readiness below its
score median; exact ties are unflagged. Median count flags at least four of six
unfavorable components. Missing components prevent a complete score. These are
relative planning categories, not causal diagnoses or absolute adequacy standards.

Model demand scores use each woman's own six responses, standardized against the
weighted need-group item distributions and averaged equally. Supply risk uses her
assigned geographic/residence profile with fixed geographic reference means/SDs
and reversed direction. Both enter continuously in original average-z units.
Separate weighted PCAs use the same inputs; mixed signs are disclosed as patterns.
Geographic map scores are not substituted for personal demand in the models.

All outcomes are current **modern contraceptive use**. Confirmed traditional-only
use and nonuse are zero. Five specifications enter demand score, supply score,
both scores, six individual demand indicators, and six indicators plus supply.
All adjust for age, age², education and fertility goal. Partnership is included
where it varies; Sindh’s need group is entirely partnered, so that constant term
is omitted. Recorded
Rural/Urban residence is included where available. Z-score and PCA models share
one complete-case sample per country. No random effects or interactions are fitted.

Final women's weights are retained: Lagos `weight`, Sindh `Normalizedweights`,
Senegal/CDI `WEIGHT`. Available sampling identifiers inform overview intervals:
Sindh `clusterCode`, Senegal `RB0H`, CDI `IDZD`. Lagos official PSUs remain
unconfirmed; full-design overview intervals are withheld. Original strata/FPC are
not documented. Model uncertainty uses administrative-area clustered CR2,
Satterthwaite intervals and small-sample block tests; Holm correction covers the
two block tests within a joint model. These are associations, not intervention
effects or shares of nonuse attributable to each side. Country reports retain
missingness, coverage, small-sample and effective-degrees-of-freedom cautions.

## Validation

Country checks in `tests/` independently reconstruct the calculations. The saved
aggregate reference JSON additionally checks 27,149 values: need counts, weighted
bars, scores/classifications, model samples, coefficients, tests and predictions.
Its Lagos reference is the personal-demand model actually used by the report,
not an older contextual-demand model. Presentation-only duplicate labels are
excluded; numerical values are retained. Tolerances are 1e-9 absolute and 1e-7
relative; flags/categories/counts must agree exactly.

`check_release.py` verifies source syntax, portable paths, archived asset checksums,
offline figures and upload exclusions, including decoded SVG/CSV content. These
checks establish technical reproduction, not power, construct validity or causal
identification. No respondent data or fitted objects are embedded in the HTMLs.
