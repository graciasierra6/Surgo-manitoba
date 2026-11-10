# Sindh — updated district analysis

This is the current administrative-area workflow. It replaces the earlier report
under its stable filename; separate residence-specific reports are unchanged.
Original input datasets and working outputs are external or ignored, not uploaded.

## Reproduce

Run from the existing repository root, with a caller-supplied input bundle:

```sh
Rscript administrative_area/analysis/sindh/Sindh_analysis.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --work-dir output/aggregate_reports/sindh \
  --output-dir reports
```

Or reuse a verified administrative join and extracted bundle:

```sh
Rscript administrative_area/analysis/sindh/Sindh_analysis.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --cache-dir "$SURGO_MANITOBA_CACHE_DIR" \
  --work-dir output/aggregate_reports/sindh \
  --output-dir reports
```

Optional `--project-root`, `--python` and `--pandoc-dir` locate installed resources
at runtime. The adjacent Rmd accepts equivalent input parameters. This module
uses existing shared project helpers, plotting, templates, country coding and
public boundary assets; its entry script is not standalone. All external data
locations are parameterized; internal resources are project relative.

## Population and geographic join

Preserve all **10,364** source women and every original survey field. The coverage
map uses all source women and all **4,308** facilities across 30 districts.
From Section 2 onward, use the previously authorized **fpn2_derived = 1** among
consenting positive-weight women: **3,274**. The raw `fpn2` field is entirely code
95 and cannot identify need. Retain the existing instrument-based derivation,
including its SA1 = 95/97 exclusion policy. These special responses do not prove
sexual inactivity; this revision reproduces the approved rule rather than silently
changing it. Do not add the later `need_n` permanent-protection cases. This is not
the DHS unmet-need algorithm or a populated original SURGO classification.

Use the **checked district join**, based on the facility survey's named `district`
field and supplied `sindh_district_aliases.csv`. This locates the 881 facilities
omitted by the older pipeline's incomplete `district_name` crosswalk. All formerly
matched assignments agree with the checked field; no prior matched district is
reassigned. Raw names, aliases and every woman assignment are verified.

There is no Rural/Urban field in the Sindh woman file. Each woman receives her own
district's FP-provider summaries, without settlement splitting or geographic
fallback. This is contextual service provision, not a facility-attendance linkage.

## FP-facility eligibility and aggregation

Require **Q506 = 1 (Yes)**: “Does this facility provide any family planning services
at the facility?” There are **2,823 Yes, 1,035 No and 450 unanswered** records.
Only Yes records enter supply aggregation, and all 2,823 have a checked district.

Match original facility IDs using `key`; only 13 records with blank `key` use the
existing `facility_id` fallback. Verify uniqueness and preserve the source files.
Reaggregate original supply columns and archetype counts/shares within district
among eligible FP providers. For each mean include valid numeric zero/FALSE and
exclude unknown or undefined values for that measure. Each facility contributes
equally; no facility weights, zero filling or common complete-facility restriction.
Selected component means retain full precision before scoring; broader inherited
flag summaries retain the original five-decimal convention.

| Selected component | Facility input | District interpretation |
|---|---|---|
| Short + long-acting offering | `one_larc_3_sa_offer` | Share meeting the original ≥1 long-acting plus ≥3 short-acting offering criterion |
| Free-product provision | `p_free_prod` | Average facility fraction of recorded products provided free |
| Trained-provider share | `p_train` | Average facility fraction of recorded personnel classified as trained |
| Daily provision | Frequency ratio below | Average facility fraction of offered modern-method classes provided daily |
| At least one LARC in stock | `one_larc_stockin` | Share meeting the original current-stock criterion |
| Stockout burden | Original cleaned `p_any_stockout` | Average facility share of product groups with a coded stock interruption |

Counseling and most-popular-product stock remain in the broader joined inventory
but are not selected index components. Preserve the original cleaned UoM stockout
values and their source grouping/missing-answer rules. Numeric completeness does
not establish complete underlying questionnaire responses.

**Training coverage is limited:** only **278 of 2,823 FP facilities (9.8%)** have
defined `p_train` values. Each district has a valid mean, but some rest on very few
reporters. Undefined values may reflect routing or unusable personnel denominators
as well as unanswered questions; they are not assumed to mean no trained provider.
The heatmap, response-count table and score interpretation flag this limitation.

### Daily provision

Seven classes use pills (`q507ad`), injectables (`q507ag/ah`), male/female condoms
(`q507ae/af`), ECP (`q507ao`), IUD (`q507aa/aa1/aa2`: interval, post-abortion,
postpartum), and implants (`q507aj`). IUD contexts and injectable variants count
once using the most frequent recorded offered variant. A known Daily variant
establishes Daily. If none is recorded offered, Not at all requires every included
variant to say so; otherwise the class remains unknown. Partial source information
is flagged. Hormonal-IUD `q507ab` and Noristerat `q507ai` are entirely blank and are
not included as known No responses. Removal and sterilization services are excluded.

Codes 1 Daily, 2 Weekly, 3 Fortnightly, 4 Monthly count as offering; code 5 Not at
all does not. Within each FP facility calculate `Daily classes / offered classes`.
A positive offered denominator with zero Daily is a valid zero. Zero recorded
offered classes gives an undefined ratio. Average valid ratios across the district.
The measure describes reported scheduling, not a validated daily-access standard;
high frequency can coexist with narrow method choice.

## Demand, z-scores and classification

Keep six personal domains: HS7C general healthcare permission, PB7B/C anticipated
social stigma, **PB2A only** health/side-effect beliefs, coherent MP4_1–9 responses
for reversible-method future-fertility concerns, MC5A/B low confidence under the
retained knowledge label, and FP1 opposition to FP. MC5 measures self-efficacy, not
objective method knowledge. Require known negative constituents to code a composite
zero; a known positive can establish a concern; unresolved responses stay missing.

Calculate weighted district demand percentages among women in need with valid
responses for each domain. Calculate unweighted valid FP-facility means for supply.
Use the same **30 complete district profiles** as the reference on each side.
For each component subtract the unweighted district mean and divide by sample SD.
Average six component z-scores separately for demand and supply readiness.

Raw stockout burden is higher = worse. Before supply standardization replace it
with **stock continuity = 1 - burden**. All six scoring inputs then run higher =
favorable. Heatmaps continue to display raw burden and color it accordingly.

Both final indices have mean zero across the 30 reference districts. Flag demand
**> 0** and supply readiness **< 0**, with 1e-10 ties unflagged. Categories are
both, demand only, supply only or neither flagged. They describe relative concerns
and provision, not causal binding constraints or absolute service adequacy. Equal
weighting permits offsetting strengths/weaknesses and can emphasize correlated
components. Component correlations and valid denominators remain available.

Top-three selected components use the largest demand or smallest favorable supply component
z-scores. Recurrence and detail tables include every district flagged on that side,
including both-flagged districts; the two groups overlap. Percentages describe
districts, not women, facilities or attributable causes of nonuse.

## Models, weights and inference

Fit five models with **current modern-method use only** as outcome:

1. Personal demand risk score + covariates.
2. Assigned district supply risk score + covariates.
3. Both scores + covariates.
4. Six personal demand indicators + covariates.
5. Those six indicators + assigned supply risk score + covariates.

Use the same **3,115 women**, including **1,710 modern users**, across all five.
Adjust for age, age squared, Pakistan-specific education and fertility goal.
Partnership is constant among modeled women and cannot enter as an identifiable
coefficient. Rural/Urban is unavailable and is not inferred from facility data.

Each woman's demand score averages her own six responses standardized against
item-specific final-weighted means and population SDs in the need group. It is not
a district demand score. Her supply risk is negative district readiness; all
residents share the district score. Scores enter per original average-z point.

Retain **Normalizedweights**. Overview intervals use recorded unrounded
**clusterCode**, building the full consenting positive-weight design before need
subsetting. Do not substitute the lower-precision `rb0e` copy or invent strata/FPC.
Primary model inference uses **district-clustered CR2** covariance, Satterthwaite
coefficient intervals, HTZ block tests and Holm correction within each joint model's
two block tests. A sampling-cluster survey-GLM inference check is exported; point
estimates agree. Original strata, FPC and full design documentation remain unavailable.

No random effects or interactions. Thirty district contexts limit supply inference;
convergence and VIF checks do not establish power or causality. Intervals condition
on the estimated scores and do not propagate their construction uncertainty.
Associations do not determine causal demand/supply dominance or an investment split.

## Outputs and verification

`inputs/sindh_women_manitoba_updated.rds` and `.dta` retain all original woman
records plus the updated supply summaries, demand domains, scores and model-sample
flag. Personal demand scores are missing outside the need group; contextual supply
scores can still be assigned to every resident. These restricted woman-level
outputs remain in ignored working folders and are excluded from code packages.

Independent checks reproduce original survey fields and weights, the approved need
rule, raw district names/aliases, Q506 eligibility, daily provision, original cleaned
stockout values, valid denominators, district means, every woman assignment,
direction alignment, zero cutoffs, top priorities, individual scores, common model
rows, weighted coefficients, district CR2 covariance and joint tests. HTML checks
inspect section order, maps, definitions, figures and decoded aggregate downloads.
These verify implementation, not source-data validity or causal identification.
