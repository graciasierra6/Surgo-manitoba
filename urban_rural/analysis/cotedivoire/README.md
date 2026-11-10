# Côte d’Ivoire: updated Rural/Urban supply assignment

Generate `reports/CotedIvoire_rural_urban_updated.html` with the updated definitions
and the same report order as the current Senegal and Lagos residence analyses:

```sh
Rscript urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

Alternatively, supply `--join-dir "$SURGO_MANITOBA_JOIN_DIR"` and
`--cache-dir "$SURGO_MANITOBA_CACHE_DIR"` for an existing verified residence join
and extracted input bundle. Optional parameters are `--work-dir`, `--output-dir`,
`--python` and `--pandoc-dir`. The adjacent Rmd calls the same pipeline.
Keep the module within this project; it reuses administrative-area preparation,
planning/map helpers and the shared survey definitions and plotting code.

All external input locations are supplied at runtime. Intermediate joins,
facility inputs, fitted models and caches stay in ignored working directories.
Share code and rendered reports, not input datasets or individual records.

## Population and assignment

Retain all 10,859 original SURGO rows and fields. From Section 2 onward, use the
original `FPN2 = 1` with consent and positive `WEIGHT`: 4,292 women in need.
No reconstructed need variable or historical need additions are applied.

Facility eligibility is raw **`s2_5_sp = 1`**, providing FP services generally.
It does not additionally require `s5_1 = 1`, the separate on-site question.
This retains the definition agreed for the updated aggregate report. Of 3,078
source facilities, 2,723 meet this criterion country-wide and 2,370 are in the
78 areas shared with SURGO. Raw `s1_5` gives facility Rural/Urban (1/2);
woman residence is `FV_MILIEU` (1/2). Reviewed geography and the documented
Abidjan, Bouake, Gagnoa and Korhogo rollups remain unchanged.

Average each facility indicator among eligible providers with valid values in the
same département–Rural/Urban cell, giving each facility equal weight. Include
genuine zero/FALSE; exclude unknown or undefined values separately by indicator.
Assign rural women their own département's rural means and urban women the urban
means. No facility weights or woman counts enter facility aggregation.

If an exact residence cell has **zero eligible FP providers**, use a flagged
whole-département FP-provider average when available. Do not fill an undefined
component when the exact cell already has FP providers. Do not assign zero or
borrow another département when no profile exists at either resolution.

In the verified snapshot:

- There are 131 observed geographic residence groups. Of these, 126 have exact
  FP-provider profiles and 124 have all six selected measures.
- **No département-wide fallback is required.** Every observed residence group
  within a matched département has eligible FP providers.
- Bonon, Facobly, Gohitafla and Taabo have no linked facility data at either
  resolution: 153 source women, including 49 in need, across five residence cells.
  Three further source rows lack geography, consent and weight.
- Daoukro has 19 eligible general FP providers (11 Rural, 8 Urban), all answering
  No to on-site provision. Free-product and daily-provision denominators are
  undefined in both cells. Their 43 women in need retain incomplete supply;
  this is not an empty-cell fallback case.
- Supply is complete for 4,200 women in need. All original rows remain in the join;
  the common regression/PCA model sample has 3,951 women and 2,262 modern users
  across 77 départements and 124 distinct numeric supply scores.

## Six components on each side

Supply uses original cleaned `one_larc_3_sa_offer`, `p_free_prod`, `p_train`,
`one_larc_stockin` and `p_any_stockout`, plus daily provision from raw `s5_2` items.
Counseling and most-popular-product stock are not selected. Daily provision counts
seven modern-method classes once: pills, injectables, male/female condoms,
emergency contraception, IUD and implants. Within each facility, Daily classes
(code 1) / offered classes (codes 1–4); Not at all (5) is excluded. Injectable and
implant variants use the most frequent recorded offered variant. No offered class
means an undefined ratio. Average the valid facility ratios equally.

Retain the original cleaned stockout definition, including its extraction
limitations documented in the stock appendix. Numeric completeness does not prove
complete underlying responses. Higher burden is worse: convert to continuity
`1 - burden` before all supply scoring and PCA. Other supply components are already
favorable. Within-facility proportions are not pooled product/personnel ratios.

Demand uses HS7C healthcare permission; PB7B/C anticipated stigma; **PB2A only**
health beliefs; coherent MP4 reversible-method future-fertility concerns; MC5A/B
confidence (retained display label “Low method knowledge”); and FP1 opposition.
Unknown answers are not automatically absence of concern. MC5 measures confidence
and self-efficacy, not objective knowledge.

## Planning versus individual models

Planning heatmaps, categories and component tables combine Rural/Urban facilities
within each département and reproduce the current aggregate report. Demand cells
are weighted valid-domain prevalences among women in need. Supply cells are
unweighted valid-facility means. Do not average two residence means without their
component-specific facility denominators to reconstruct département means.

Planning references are equal-area means and sample SDs: 82 complete-demand areas
and the same 77 complete-supply areas for every supply component. Average six
component z-scores; flag demand strictly above zero and favorable readiness below
zero (1e-10 tolerance). Missing supply stays unassessed. These are relative
planning signals, not absolute adequacy standards or causal constraints.

Models use each woman's own six demand responses. Standardize them with
item-specific final-weighted means and population SDs among women in need and
average all six for personal demand risk. Supply risk is the negative average of
her six assigned favorable component z-scores using fixed 77-département planning
references. Residence scores are not recentered and need not average zero.

All five main and five PCA models use **current modern use**, original `WEIGHT`,
age, age squared, country-specific education, partnership, fertility goal and
Rural/Urban. Specifications use demand score; supply risk; both; six personal
indicators; and six indicators plus supply risk. All share one complete sample.
There are no random effects or interactions.

Overview intervals use recorded `IDZD` census-district clusters, constructing the
full consenting positive-weight design before need subgroup selection. Original
strata/FPC are unavailable. Primary model covariance is département-clustered CR2,
with Satterthwaite intervals and HTZ block tests. Holm adjustment covers the pair
of block tests within each joint model. Recorded-PSU inference is exported as a
sensitivity. Very low effective degrees of freedom in some tests require caution.

PCA sensitivity uses separate final-weighted correlation PCAs for personal demand
and assigned residence supply. Repeated supply profiles contribute their summed
represented-woman weights, not facility weights. Signed loadings, variance,
permutation/deletion checks and the same models are reported. Mixed-sign PC1 is
interpreted as a pattern, not a monotonic risk index.

## Verification and limitations

Independent checks reproduce raw fields, need, weights, geography, facility
eligibility/residence, daily provision, every assigned component/denominator,
whole-département planning scores, personal scores, model coefficients/CR2 and
PCA scores. HTML checks cover section order, maps, figures, internal links,
embedded aggregate downloads and portable content. Previous reports are retained.

Unmatched areas do not establish absence of services; aggregate assignment is not
a facility-attendance link. Models describe cross-sectional associations, not
causal bottlenecks or investment returns. Score-construction uncertainty is not
propagated into intervals. VIF, convergence and geographic counts do not establish
statistical power or recover unavailable original survey-design information.

## Append need-group residence coverage to Data checks

```sh
Rscript urban_rural/analysis/cotedivoire/update_data_checks.R \
  --base-html "$SURGO_MANITOBA_DATA_CHECKS_HTML" \
  --data-dir "$SURGO_MANITOBA_CDI_ANALYSIS_DIR" \
  --output reports/Data_checks.html
```

The analysis directory is this module's `CotedIvoire_rural_urban_updated_files`
working output. The updater preserves earlier sections and replaces only its own
marked section if rerun. It displays a combined Rural/Urban fallback summary and separate detailed tables, with
unweighted counts and both sample and final-weighted percentages. Both fallback
tables contain zero cases in the verified source. The 49 women in need without
any linked département profile are listed separately, not called fallback
recipients. Daoukro's 43 partially defined exact assignments are also distinguished.
An optional `--audit-json` exports aggregate calculation checks. Assignments,
classifications and fitted models are not changed.

## Separate the classification updates

```sh
Rscript urban_rural/analysis/cotedivoire/check_classification_changes.R \
  --friday-data-dir "$SURGO_MANITOBA_CDI_FRIDAY_ANALYSIS_DIR" \
  --updated-data-dir "$SURGO_MANITOBA_CDI_ANALYSIS_DIR" \
  --facility-input "$SURGO_MANITOBA_FACILITY_LINKAGE_INPUT" \
  --raw-facility "$SURGO_MANITOBA_CDI_RAW_FACILITY" \
  --output-dir output/classification_checks/cotedivoire_drivers
```

This diagnostic first reproduces the saved Friday and updated main z-score
classifications and their numerical scores. It evaluates all 32 combinations of
five changes: zero versus median cutoff, PB2A-only health beliefs, replacement
supply components, FP-provider filtering and common versus component-specific
supply reference areas. Saved demand heatmap inputs retain survey weighting and
need-group restriction; supply inputs are reaggregated under each facility and
component option. Comparisons change one option at a time, including both a
Friday baseline and removal of one update from the final specification. Counts
overlap and do not partition the total classification changes. These are
classification sensitivity checks, not regression results or causal attribution.
The reports, assignments and fitted models are unchanged by this diagnostic.
