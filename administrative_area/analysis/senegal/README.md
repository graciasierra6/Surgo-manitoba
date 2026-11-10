# Senegal_analysis_aggregate

This is the current administrative-area workflow. It replaces the earlier report
under its stable filename; separate residence-specific reports are unchanged.
Original input datasets and working outputs are external or ignored, not uploaded.

## Build

Run from the repository root after installing the recorded project dependencies:

```sh
Rscript administrative_area/analysis/senegal/Senegal_analysis_aggregate.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --work-dir output/Senegal_analysis_aggregate \
  --output-dir reports
```

Alternatively, reuse a verified administrative join and its extracted input cache:

```sh
Rscript administrative_area/analysis/senegal/Senegal_analysis_aggregate.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --cache-dir "$SURGO_MANITOBA_CACHE_DIR" \
  --work-dir output/Senegal_analysis_aggregate \
  --output-dir reports
```

External locations are supplied at runtime. Optional `--python` and `--pandoc-dir`
select installed runtimes. The project library and dependencies are shared with
the existing repository. The Rmd accepts equivalent parameters. Working data and
microdata remain in ignored output directories and must not be uploaded.

The named entry script is **`Senegal_analysis_aggregate.R`**, matching the updated
HTML. Keep the complete `administrative_area/analysis/senegal/` module in
the existing repository: the entry script calls its preparation, score, model and
verification helpers and the project's existing shared code. If invoking a copy
of the entry script from elsewhere, supply `--project-root` explicitly. Do not
upload only that one script and expect it to reproduce the report independently.
Use `--help` for all runtime options. No input locations are embedded in the code.

## Dataset and definitions

1. Match all 2,043 original facility IDs to the raw pooled Senegal facility file.
2. Restrict to **s5_6 = 1**, on-site FP service provision: 1,941 facilities countrywide,
   including 1,568 in the 33 départements represented in SURGO.
3. Reaggregate **all existing supply columns**, not only the selected six, using
   the original aggregator on the restricted facility sample. Counts, archetype
   shares, pooled ratios and service volumes therefore use the same eligibility
   restriction. Individual facility indicators retain their original definitions.
4. For each mean, retain finite valid values, including recorded zeros and FALSE.
   Undefined ratios and missing values do not enter that measure's denominator.
   Facilities are unweighted. No new facility weights or missing-as-zero rules are
   introduced. Existing upstream rounding of Boolean geographic means is retained.
5. Assign the resulting département profile to every resident woman. Preserve all
   7,169 women and all original SURGO fields; verify the RDS and Stata round trip.
   Rural/urban residence is a covariate, not a supply matching key.
6. Sections 2 onward use original SURGO **FPN2 = 1**: 1,905 women. Health / side-effect
   beliefs use **PB2A only**; the other five demand domains remain unchanged.

Selected supply components are:

| Component | Facility definition | Raw direction |
|---|---|---|
| Method offering | Original `one_larc_3_sa_offer` flag | Higher = better |
| Free-product provision | Original `p_free_prod` | Higher = better |
| Trained-provider share | Original `p_train` | Higher = better |
| Daily provision of offered modern methods | New Q5.7 ratio below | Higher = more frequent provision |
| At least one LARC in stock | Original `one_larc_stockin` flag | Higher = better |
| Stockout burden | Original `p_any_stockout`, without recoding | Higher = worse |

Counselor availability and popular-product stock remain available in the joined
supply inventory but are excluded from every selected index, classification,
priority list, primary model and PCA calculation in this revision.

### Daily provision

Use raw Senegal `s5_7a`–`s5_7g`: pills, injectables, male and female condoms,
emergency contraception, IUD and implants. Numeric codes are 1 Daily, 2 Weekly,
3 Biweekly, 4 Monthly and 5 Not at all. For each confirmed FP provider, calculate:

`count(code = 1) / count(code in 1:4)`

Missing/unrecognized responses enter neither count; Not at all is excluded from
the offered-method denominator. A zero denominator is undefined, not a zero daily
share. Average the valid facility ratios equally within each département; do not
pool the numerator and denominator across facilities. The proportion is defined
for 1,560 of 1,568 linked FP facilities; eight have no recorded offered category.
A zero ratio with a positive denominator is a genuine zero and remains included.
Some facilities have partial method responses; their ratio is conditional on the
recorded categories, not proof that unrecorded methods are absent.

A département value of 75% means its average reporting FP facility provides 75%
of its recorded offered modern categories daily. It is not the percentage of
facilities providing every method daily. High regularity can coexist with narrow
method breadth; the separate offering component captures that dimension. This
is reported provision frequency, not a validated daily-access adequacy standard.
The private audit and aggregate coverage export also record an at-least-weekly
ratio for comparison; the selected index uses daily only.

### Stockout definition and direction

Keep the original cleaned `p_any_stockout` values as supplied. All 1,568 linked
FP providers have numeric values. This is a within-facility product-group share,
not a flag for any facility stockout and not an explicitly offered-method-only
ratio. In Senegal the source history question refers to the last three months.
Numerical completeness does not establish complete underlying responses or
independently validate upstream missing-data rules. This revision changes the
selected indicator, not the approved upstream construction.

Heatmaps show raw burden, higher = worse. Before all supply scoring, ranking and
PCA, replace burden with `stock continuity = 1 - burden`, leaving the other five
favorable inputs unchanged. All six now run higher = better. Average their z-scores
for readiness and negate that average for supply risk. This is algebraically the
same as reversing the other five inputs to risks, keeping burden as a risk, and
averaging their z-scores. Raw and aligned percentages are both saved in the score
trace, with explicit direction flags.

### Geography retained

The reviewed district-to-département assignments are unchanged. Existing review
flags remain for Diakhao → Fatick (16 records), Yeumbeul → Keur Massar (9), and
Sangalkam → Rufisque (19). The eligibility revision does not resolve these mapping
qualifications or provide facility-coordinate verification.

## Scores and decision rule

For each of the six demand components, calculate the survey-weighted percentage
among valid women in need in each département. For each supply component, use
the FP-provider-only summary above. Standardize each component across the same
33 départements: `(value - unweighted département mean) / sample SD`. Every
département contributes equally to these normalization references.

Average six component z-scores separately for demand and direction-aligned favorable supply. The
final scores have mean zero across the 33 départements, up to floating-point
precision; the SD of their average need not be one. Their sum would yield the
same ranking and zero-based classification, but six times the numerical values.

- Demand flag: final demand score > 0.
- Supply flag: final favorable-readiness score < 0.
- Both: both flags; neither: neither flag.
- Absolute values within 1e-10 of zero are treated as ties and unflagged.

These are relative planning comparisons, not adequacy standards, significance
tests or causal diagnoses. The benchmark is the equal-département average in the
surveyed set, not the national population mean. The rank-average, raw-percentage,
Median/IQR and median-count alternatives remain in an appendix with their own
documented rules. All main planning results use the zero-threshold z-score rule.

Standardization and explicit equal weighting follow established composite-index
methods described by the [OECD/JRC handbook](https://doi.org/10.1787/9789264043466-en).
That guidance does not validate these particular indicators, weights or cutoffs.
Correlated inputs, extreme values, sparse responses and sampling uncertainty can
affect the scores. Equal averaging allows strengths to offset weaknesses.

## Models and outputs

Five primary weighted modern-use models use a common sample: personal demand
score, assigned supply risk score, both scores, six personal demand indicators,
and six personal demand indicators plus supply risk. All adjust for age, age²,
education, partnership, fertility goal and rural/urban residence. No random
effects or interactions are fitted. Models use département-clustered CR2 inference
with Satterthwaite intervals; a recorded-RB0H survey-GLM check reproduces weighted
point estimates. Original strata and FPC are unavailable. Intervals condition on
the estimated indices and do not propagate score-construction uncertainty.

The model demand score standardizes each woman's own six responses using
need-group survey-weighted item means and population SDs. It is separate from
the contextual planning demand score. Supply risk is the negative average of the
six assigned département supply z-scores. Neither score is categorized for fitting.

The PCA appendix is secondary and uses the same updated indicators and women.
Signed loadings and limited dimensionality are reported rather than treated as
validated causal importance. Complete-facility and complete-demand comparison
sections are not included.

Priority recurrence and detailed top-three tables include every flagged
département on the relevant side, including both-flagged areas. Counts therefore
overlap between demand and supply groups; within each group a département
contributes to three selected components. Priorities are component z-score rankings, not
regression effects or necessarily three deficits below an absolute standard.

## Verification

The separate preparation, numerical and HTML checks verify original survey
preservation, FP eligibility, all six facility means, every woman’s assignment,
PB2A coding, direct daily-provision ratios, preservation of cleaned stockout burden,
weighted heatmaps, stockout sign reversal and equivalent risk-first scoring,
component standardization, zero-based flags,
coefficient and CR2 reproduction, PCA SVD equivalence, map coverage, table counts,
embedded figures/downloads and artifact portability. These checks do not prove
unbiased nonresponse, adequate power or causal binding constraints.
