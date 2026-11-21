# Residence-specific analysis and shared definitions

## Separate demand-population check

Current main classifications already use weighted concern percentages among women
in need. `check_demand_population.R` compares that population with all eligible
women, keeping the six domain definitions, geographic reference set, supply flags
and zero-cutoff rule fixed. Each population gets its own component means/SDs.
It independently reproduces the saved need-only classifications before comparing
them and never overwrites the main data or results.

```sh
Rscript analysis/check_demand_population.R \
  --data-dir "$SURGO_MANITOBA_COUNTRY_ANALYSIS_DIR" \
  --output-dir output/demand_population_check/COUNTRY
python3 analysis/render_demand_population_check.py \
  --results-dir output/demand_population_check \
  --output reports/Demand_population_classification_check.html
```

Run the R check for each selected country, using `lagos`, `senegal`,
`cotedivoire` or `sindh` as its output subdirectory for the combined renderer.
Inputs and intermediate calculation CSVs stay in ignored working folders;
the HTML embeds only aggregate area comparisons.

The current administrative-area reports are built with
`Rscript R/run_aggregate_reports.R`; see the
[administrative-area analysis guide](../administrative_area/analysis/README.md).
Those four reports use FP-provider-only supply summaries, PB2A-only health
beliefs and zero cutoffs for the primary geographic z-scores. Their definitions,
models and checks were rebuilt together.

This folder retains the earlier residence-specific workflow for Lagos, Senegal
and Côte d'Ivoire and supplies shared survey definitions to both workflows.
The current updated residence modules are in `urban_rural/analysis/` and generate
`*_rural_urban_updated.html`; see the
[updated residence guide](../urban_rural/analysis/README.md). Sindh has no
residence split: either runner delegates to its current district module in
`administrative_area/analysis/sindh/`.

## Current health-belief definition

As agreed on November 9, 2026, the primary **Health / side-effect beliefs**
indicator uses **PB2A only** in all four country definition files: 1 for very or
somewhat likely to experience health problems; 0 for somewhat or very unlikely;
all other responses remain unresolved. PB2B and PB2C no longer enter this
primary indicator. Future-fertility concerns continue to use the existing MP4
definition. The prior PB2A–PB2C any-concern definition is retained in
`check_health_beliefs.R` solely for comparison.

The three earlier HTMLs named `Lagos_analysis.html`, `Senegal_analysis.html` and
`Cote_dIvoire_analysis.html` retain their historical results. They are distinct
from the current `*_rural_urban_updated.html` reports. The updated residence
modules include the revised definitions and FP-provider restriction and are
rebuilt independently from the administrative-area reports.

The comparison check reads a caller-supplied administrative join and Sindh need
file, calls the current country definitions, and exports aggregate statistics:

```sh
Rscript analysis/check_health_beliefs.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --sindh-need-file "$SURGO_MANITOBA_SINDH_NEED_FILE" \
  --output-dir output/data_checks
```

Run **`Rscript R/run_reports.R`** from the repository root for the earlier
residence-specific workflow. This compatibility entry point retrieves the
checksummed original source revision, keeping historical definitions isolated
from current shared functions. For all current reports, use `R/rebuild_all.R`.
The remaining
residence-specific descriptions below apply to the three earlier reports; the
linked administrative-area guide governs current Sindh results.

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
| Lagos | Original `fpn2 = 1`. Main sample, exact LGA–residence FP-provider supply where available. Ibeju-Lekki–Urban has no confirmed FP provider and receives a flagged LGA-wide profile; the former free-provision exception is not used. |
| Sindh | Delegates to the current district module, using the authorized `fpn2_derived`, checked district assignments and FP-provider-only supply. See its [README](../administrative_area/analysis/sindh/README.md). No woman's residence field is invented. |
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
women in need. Supply planning summaries combine confirmed FP-providing facilities in each
administrative area using valid-response means and no facility weights.

Five geographic scoring methods are compared: rank average, mean percentages,
median/IQR, median count and average z-score. The primary z-score classification
flags both demand and supply constraints strictly above zero, the equal-area
reference mean; numerical ties within 1e-10 are unflagged. Median count flags at least four of six
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
Senegal/CDI `WEIGHT`. SURGO weighted estimates use the survey package with
`ids = ~1`, no strata and no FPC. Original sampling identifiers are not specified.
Lagos overview intervals remain withheld. Models use `survey::svyglm()` followed
by administrative-area clustered CR2,
Satterthwaite intervals and small-sample block tests; Holm correction covers the
two block tests within a joint model. These are associations, not intervention
effects or shares of nonuse attributable to each side. Country reports retain
missingness, coverage, small-sample and effective-degrees-of-freedom cautions.

## Validation

Country checks independently reconstruct the current calculations. The complete
rebuild also compares the resulting HTML content, downloadable values, scripts
and figure labels with published references; see `REPRODUCIBILITY.md`. The older
`report_reference_values.json` and `verify_report_reproduction.py` apply only to
the preserved three-country historical workflow, not current Sindh. Their
numerical tolerances are 1e-9 absolute and 1e-7 relative; flags/categories/counts
must agree exactly.

`check_release.py` verifies source syntax, portable paths, archived asset checksums,
offline figures and upload exclusions, including decoded SVG/CSV content. These
checks establish technical reproduction, not power, construct validity or causal
identification. No respondent data or fitted objects are embedded in the HTMLs.

## Similar totals, different component profiles

`score_profile_examples.R` and its matching Rmd build a separate, static diagnostic
of the updated Rural/Urban reports' area-level planning scores. They search complete
above-average challenge profiles for pairs within 0.10 original average-z points,
then show six illustrative pairs with their signed component contributions, raw
percentages and valid denominators. Supply is displayed as negative readiness so
higher values mean more challenges on both sides. The original scores, models and
reports are unchanged.

```sh
Rscript analysis/score_profile_examples.R \
  --lagos-dir "$SURGO_MANITOBA_LAGOS_ANALYSIS_DIR" \
  --senegal-dir "$SURGO_MANITOBA_SENEGAL_ANALYSIS_DIR" \
  --cotedivoire-dir "$SURGO_MANITOBA_CDI_ANALYSIS_DIR" \
  --output-dir reports --work-dir output/score_profiles
```

The directories are the generated `*_rural_urban_updated_files` working outputs
from the country modules. `--pandoc-dir` is an optional runtime parameter.
`reports/Score_profile_examples.html` embeds all six figures. Candidate-pair tables
and the numerical/source-hash audit remain in ignored working output. A component's
share of positive z-points is an arithmetic concentration diagnostic, not a share
of the net score, a percentage of nonuse explained, or a causal finding. Examples
include moderate positive totals and are not an inventory of the most severe areas.

`plot_score_profiles_combined.R` creates a paired figure from those saved examples:
display-aligned component percentages on the left and the same components'
signed index contributions on the right. Area colors and rows match across
panels. The reference ticks are equal-area means from the existing index.
For supply, favorable percentages are displayed as their complements; stockout
burden remains unchanged. This display transformation does not alter the scores.
Y-axis labels retain the heatmap indicator names, with a gap description beneath
the five complemented provision measures. Stockout burden retains its original
percentage because higher burden already indicates greater challenges.
For demand examples, the panels use plain-language titles and retain the original
weighted percentages. The unchanged confidence indicator is labelled by what it
measures: confidence choosing a method or managing side effects.

```sh
Rscript analysis/plot_score_profiles_combined.R \
  --input "$SURGO_MANITOBA_SCORE_PROFILE_EXAMPLES" --case senegal_supply \
  --output-dir output/combined_score_profiles
```

The input is the aggregate-only `score_profile_examples.rds` generated by the
preceding diagnostic. The script writes an SVG and aggregate plotting values;
the vector figure can be rasterized for presentation previews.
For the simpler leadership-facing demand figure, use `--case senegal_demand
--view percentages`. It displays the weighted concern percentages and the two
overall score labels, without reference ticks or the contribution panel. The
existing combined figure remains available for technical explanation.
The presentation view has no footnotes; definitions, weighting and valid-response
denominators remain documented in the underlying diagnostic report.

For a presentation pair using Côte d’Ivoire demand and Senegal supply, render
`cdi_broader_demand` from the four-geography contrast RDS and `senegal_supply`
from the original score-profile RDS with `--view percentages`. Add
`--supply-score readiness` to the supply command to retain the original negative
readiness score labels. The six supply bars still use higher-is-worse percentages
(complements of favorable indicators, with stockout burden unchanged). This
changes the score labels only; it does not recalculate the original scores.
Add `--supply-percentages original` to retain the six original heatmap values
instead of complementary gaps. Every row states its direction: higher is better
for the first five indicators, and lower is better for stockout burden. For the
larger-sample Abidjan–Gagnoa supply illustration, use `--case
cdi_concentrated_supply` with the four-geography contrast RDS. Neither profile
contains a 0% or 100% component. Abidjan has five below-reference components;
Gagnoa has three, but free provision is much more unfavorable than its two smaller
weaknesses. It must not be described as literally below average on only one item.

`plot_demand_pair_table.R` provides a presentation-table alternative from the
same aggregate profile RDS. It retains raw percentages and bolding for z > 0,
adds continuous orange shading for positive component z-scores (saturating at
z = 3), and shows each pair's absolute percentage-point difference with a blue
bar on a fixed 0–100 scale. Reference means and the higher area are labelled in
text. There is no new difference cutoff, significance claim or classification.
Select geography and area keys through `--country`, `--area-a` and `--area-b`;
`--input` points to the generated aggregate profile RDS. SVG/aggregate values
go to `--output-dir`; optional `--html-output` creates a standalone preview
with an accessible values table. No main report is changed.
The presentation figures omit the subtitle and bottom footnote; definitions
remain in a collapsed section of each standalone HTML. Use `--all-pairs` to
render the four selected Côte d’Ivoire, Senegal, Lagos and Sindh comparisons;
with `--html-output`, the same command builds one self-contained comparison
document. Example:

```sh
Rscript analysis/plot_demand_pair_table.R \
  --input "$SURGO_MANITOBA_SCORE_PROFILE_CONTRASTS" --all-pairs \
  --output-dir output/demand_pair_tables \
  --html-output reports/Demand_pair_comparisons.html
```

`--regional-comparisons` retains these four pairs and adds Hyderabad–Sanghar
in Sindh. It builds `data_checks/Composite score_comparisons.html`
with an introduction and a descriptive finding beneath each figure. Every
selected area has a positive demand score; an area can also have a supply flag.
The findings distinguish the number of above-reference components from the
magnitude of those elevations. They do not interpret a positive component as
automatically severe or a percentage-point difference as statistically significant.
The existing four-pair report and all main analyses remain unchanged.

```sh
Rscript analysis/plot_demand_pair_table.R \
  --input "$SURGO_MANITOBA_SCORE_PROFILE_CONTRASTS" --regional-comparisons \
  --output-dir output/regional_comparisons \
  --html-output "data_checks/Composite score_comparisons.html"
```

The current combined report adds eight supply-readiness comparisons below the
five demand comparisons. Rebuild that version with `plot_supply_pair_table.R`:

```sh
Rscript analysis/plot_supply_pair_table.R \
  --input "$SURGO_MANITOBA_SCORE_PROFILE_CONTRASTS" \
  --output-dir output/supply_pair_tables \
  --html-output "data_checks/Composite score_comparisons.html"
```

The supply script regenerates the original five demand figures unchanged and
adds Ibeju-Lekki–Ikorodu, Matam–Goudiry, Abidjan–Gagnoa,
Bongouanou–M’Bahiakro, Ajeromi-Ifelodun–Ojo, Bongouanou–Tanda and
Biankouma–Méagui and Divo–Bongouanou. Bongouanou–Tanda emphasizes different service weaknesses;
Biankouma–Méagui emphasizes broader modest-to-moderate shortfalls versus two
pronounced shortfalls offset by four above-average components. A finding appears
directly beneath every figure. Composite scores are constraint-aligned: positive means greater constraints.
Every supply row displays
a favorable percentage, with stock continuity equal to 100 minus stockout
burden. Bold and orange indicate below-reference supply values; the demand
figures retain above-reference bolding. This display transformation preserves
the existing supply scores and classifications.

Every finding is preceded by a short label identifying its main message:
different types of challenge, different breadth and severity, or both.
The labels describe the comparison's emphasis; they are not new area
classifications. Adding them preserves the existing finding text and figures.
Divo–Bongouanou carries the breadth-and-severity label: five modest-to-moderate
shortfalls in Divo versus two pronounced and one moderate shortfall in
Bongouanou, partly offset by three above-average components.

The script searches within all four geographies and writes an aggregate
candidate-pair table to the working output. Selected pairs have supply-constraint
scores above zero with absolute differences no larger than 0.10, at least
20 valid FP facilities per component and no displayed 0%/100% value. These
illustration screens do not establish statistical precision. No positive-constraint-score
area has exactly one below-reference supply component. The Matam example
instead has one large and two marginal shortfalls; the text makes that
distinction explicit. Valid-facility counts by component are written to the
aggregate values CSVs in the parameterized working output and appear in the
standalone previews. The combined presentation report retains the opening
paragraph but omits the weighting/context paragraph, comparison navigation,
and all expandable “Definitions and accessible values” sections. Its supply
introduction also omits the one-component-versus-six-component sentence.
Detailed definitions remain in the standalone previews and documented source.
The main analyses and regressions are unchanged.

`score_profile_contrasts.R` and its matching Rmd extend this read-only comparison
to all four geographies, including the Sindh district report. They retain the
near-equal score screen, rank candidate pairs by raw percentage-point contrast,
and distinguish broader profiles from those concentrated in one component.
The separate Senegal breadth example explicitly relaxes score similarity and
flags its smaller item denominators. No model is refitted. The supply percentage
figure retains the original heatmap values and original readiness labels. Higher
is better for the first five measures; lower stockout burden is better.

```sh
Rscript analysis/score_profile_contrasts.R \
  --lagos-dir "$SURGO_MANITOBA_LAGOS_ANALYSIS_DIR" \
  --senegal-dir "$SURGO_MANITOBA_SENEGAL_ANALYSIS_DIR" \
  --cotedivoire-dir "$SURGO_MANITOBA_CDI_ANALYSIS_DIR" \
  --sindh-dir "$SURGO_MANITOBA_SINDH_ANALYSIS_DIR" \
  --output-dir reports --work-dir output/score_profile_contrasts
```

The self-contained `reports/Score_profile_contrasts.html` includes the four-area
search summary, selected percentage figures, valid Ns and selection limitations.
The Sindh parameter points to its generated `*_files` directory containing
`report_data.json`. Candidate tables and source-hash validation stay in ignored
working output. Neither a numerical similarity screen nor a minimum valid N
constitutes a test of significance or a precision guarantee.

`check_need_definition_concordance.R` provides a separate, read-only validation
of the authorized Sindh need reconstruction against populated original FPN2
flags in the other three geographies. Supply four original surveys through
`--lagos-data`, `--senegal-data`, `--cotedivoire-data`, `--sindh-data`, and the
authorized Sindh need file through `--sindh-need-file`. CSV, DTA and RDS inputs
are supported. Aggregate cross-tabs, weighted/unweighted agreement, retention
of the original need group, and discrepancy reasons go to `--output-dir`
(default `output/need_definition_concordance`). No respondent-level output is
exported and no supplied classifications are changed. High overall agreement
does not establish exact replication of an unavailable original Sindh flag;
special-code handling and discrepancies within the in-need group are reported
separately.

## Method awareness and confidence

`check_method_awareness.R` and its matching Rmd build a separate diagnostic for
Lagos and Senegal women with original SURGO FPN2 = 1. Awareness combines eight
modern methods named in KA1 or recognized in KA2 before an explanation. The
existing MC5 confidence flag is reconstructed independently. The diagnostic
examines weighted distributions, overlap, adjusted modern-use associations and
leave-one-area-out prediction, retaining the current covariates and assigned
residence-specific supply risk. Contradictory answers, spontaneous naming and
awareness nonlinearity are checked separately. The main six-domain reports,
indices and classifications are not changed by this diagnostic.

```sh
Rscript analysis/check_method_awareness.R \
  --lagos-dir "$SURGO_MANITOBA_LAGOS_ANALYSIS_DIR" \
  --lagos-women-file "$SURGO_MANITOBA_LAGOS_ANALYSIS_JOIN" \
  --senegal-dir "$SURGO_MANITOBA_SENEGAL_ANALYSIS_DIR" \
  --senegal-women-file "$SURGO_MANITOBA_SENEGAL_ANALYSIS_JOIN" \
  --output-dir output/method_awareness_check --report-dir reports
```

The analysis directories contain generated `analysis_frame.rds` and
`report_data.json` files. The joins are the analysis-prepared, FP-provider
residence joins generated in each country's `inputs` working directory, not the
unfiltered base join. All external locations are caller-supplied parameters.
`--pandoc-dir` optionally supplies the local Pandoc directory at runtime.
The report is `reports/Method_awareness_check_Senegal_Lagos.html`; it contains
aggregate statistics and embedded figures, with no respondent records or fitted
objects. Detailed calculation outputs remain in ignored working output.

## Cumulative coverage and completeness tables

`Surgo_UoM_checks.R` and its matching Rmd build
`data_checks/Surgo_UoM_checks.html`. A left-side contents panel links to Côte d’Ivoire,
Lagos, Senegal and Sindh, in that order. Each section has two coverage tables:
all source women and women in need of family planning. Côte d’Ivoire, Lagos and
Senegal show Rural, Urban and Total columns. Sindh uses district-level coverage
because its SURGO file has no Rural/Urban identifier; residence is not inferred.

| Geography | Final woman weight | Need definition | Raw FP-provider eligibility |
|---|---|---|---|
| Côte d’Ivoire | `WEIGHT` | Original `FPN2 = 1` | `s2_5_sp = 1` |
| Lagos | `weight` | Original `fpn2 = 1` | `q506 = 1` |
| Senegal | `WEIGHT` | Original `FPN2 = 1` | `s5_6 = 1` |
| Sindh | `Normalizedweights` | Approved upstream `fpn2_derived = 1`, matched by `col_id` | `q506 = 1` |

No need definitions, facility assignments or main analytical results are changed.
Counts retain every source record in the respective population. Percentages are
weighted column percentages using consenting women with valid positive final
weights; each table displays the weighted denominator and excluded record count.
There are no confidence intervals or model estimates in this report, so no survey
variance approximation is introduced. No facility weights are applied.

Facility counts retain all recorded areas, including areas without SURGO women.
Each population introduction reports the number of women and represented areas,
all Manitoba facility records and their represented areas, and the subset of
confirmed FP providers and their represented areas. An overlap sentence counts
named areas with both sources, SURGO only and Manitoba FP-provider data only.
Supply presence in these overlap counts requires at least one confirmed FP
provider; the counts refer to whole administrative areas, not residence cells.
Demand presence is population-specific: at least one source woman for the
all-women table, or at least one woman in need for the need-group table. Records
with unrecorded geography remain in respondent totals but not in area counts.
Whole-area counts use unique facilities without double-counting fallback
providers. Exact residence matches, flagged same-area fallbacks and unmatched
records are recomputed from the underlying FP-eligibility responses and reconciled
against the existing woman-level joins. Each population has an assignment summary;
where applicable, the report also lists affected fallback groups.

Senegal's facility residence uses the existing health-district lookup proxy.
FP providers with unknown proxy residence remain in whole-département totals,
without being assigned to Rural or Urban. Côte d’Ivoire's three source women with
unrecorded geography and residence remain in the Total/unmatched counts. The
four unmatched named survey areas remain visible. Coverage means a geographic
profile exists, not that every supply indicator has a valid value.

After both population tables in Côte d’Ivoire and Senegal, restored-matches tables
show the current health-district assignments, with facility-record counts beside
each district name. Both identify the reference as the uploaded GitHub merge
prepared by JC and report resident counts and weighted population shares.
The tables are derived from audited facility records whose upstream target was
missing and whose checked target was restored, combined with the population
coverage summaries. Côte d’Ivoire recovers three previously unmatched survey
areas (89 facility records). Senegal adds 515 records from 22 health districts to
16 départements that already had profiles. Its explanation states that all 7,169
women were already matched at the département level; the resident-count columns
refer to women whose existing profiles use a changed facility pool. Côte d’Ivoire's
corresponding columns identify newly matched women. Multiple districts within a département are combined so
resident counts are not duplicated. Diakhao–Fatick remains explicitly provisional.
A separate note retains the 41-record Ziguinchor reassignment from Bignona;
Ziguinchor has no SURGO respondents. Bignona gains 16 Thionck-Essyl records but
loses 41 Ziguinchor records, a net reduction of 25 recorded facilities.
Parenthetical facility counts include all
records; the analytical summaries continue to filter to FP providers and valid
responses. Current crosswalk assignments are not facility-coordinate boundary
validation.

Set the following environment variables to caller-supplied input locations, then
run the entry point. The same values can be passed as command-line arguments
listed by `Rscript analysis/Surgo_UoM_checks.R --help`.

```sh
Rscript analysis/Surgo_UoM_checks.R \
  --facility-input "$SURGO_MANITOBA_FACILITY_LINKAGE_INPUT" \
  --lagos-input "$SURGO_MANITOBA_LAGOS_WOMEN_JOIN" \
  --lagos-raw-facility "$SURGO_MANITOBA_LAGOS_RAW_FACILITY" \
  --cotedivoire-input "$SURGO_MANITOBA_COTEDIVOIRE_WOMEN_JOIN" \
  --cotedivoire-raw-facility "$SURGO_MANITOBA_COTEDIVOIRE_RAW_FACILITY" \
  --senegal-input "$SURGO_MANITOBA_SENEGAL_WOMEN_JOIN" \
  --senegal-raw-facility "$SURGO_MANITOBA_SENEGAL_RAW_FACILITY" \
  --senegal-residence-lookup "$SURGO_MANITOBA_SENEGAL_RESIDENCE_LOOKUP" \
  --sindh-input "$SURGO_MANITOBA_SINDH_WOMEN_JOIN" \
  --sindh-raw-facility "$SURGO_MANITOBA_SINDH_RAW_FACILITY" \
  --sindh-need-file "$SURGO_MANITOBA_SINDH_NEED_FILE" \
  --work-dir output/surgo_uom_checks --output-dir reports
```

The first three country inputs are the current analysis-prepared FP-provider
residence joins; Sindh uses the current district-only FP-provider join. All retain
original survey columns. The facility input is the checked, crosswalked
`facility_linkage_inputs.rds` generated by the administrative join. Raw facility
surveys provide eligibility and residence validation; the Senegal lookup and
Sindh approved need file come from the parameterized input bundle. Inputs are
read-only, their hashes are checked after preparation, and no input locations are
written to the shared report. Aggregate intermediate tables stay in ignored
working output. The shared report is standalone and contains no microdata.

`tabulate_women_residence.R` remains available as a separate, generic one-table
helper, including an optional caller-specified population restriction. Additional
checks can extend the cumulative report without changing the main analyses or
the earlier `Data_checks.html`.

Current script and publication editions are distinguished in [REPRODUCIBILITY.md](../REPRODUCIBILITY.md).
The current code adds zero, +0.5, +1 and +1.5 final-composite SD maps and distributions
and is reflected in the current published HTMLs. See [survey and scoring](../docs/SURVEY_AND_SCORING.md).
