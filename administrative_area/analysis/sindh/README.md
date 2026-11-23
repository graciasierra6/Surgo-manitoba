# Sindh — updated district analysis

Start with the [input and separate-run guide](../../../docs/COLLEAGUE_RUN_GUIDE.md) for the exact dataset
names, dependency setup, commands and expected outputs. The [full ZIP inventory](../../../docs/INPUTS.md)
lists all 56 members; the 14 primary inputs alone are not a replacement bundle.
[Script dependencies and data contracts](../../../docs/SCRIPT_REFERENCE.md) are also summarized in R headers.

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
| Trained-provider share | Reconstructed Q406 ratio | Average facility fraction of staff with known status classified as trained |
| Daily provision | Frequency ratio below | Average facility fraction of offered modern-method classes provided daily |
| At least one LARC in stock | `one_larc_stockin` | Share meeting the original current-stock criterion |
| Stockout burden | Original cleaned `p_any_stockout` | Average facility share of product groups with a coded stock interruption |

Counseling and most-popular-product stock remain in the broader joined inventory
but are not selected index components. Preserve the original cleaned UoM stockout
values and their source grouping/missing-answer rules. Numeric completeness does
not establish complete underlying questionnaire responses.

### Training reconstruction

The supplied cleaned `p_train` has valid values for only **278 of 2,823 FP
facilities**. The analysis now reconstructs this measure from both Q406 encodings:
topic checkboxes and the original multi-selection text. It retains the original
Sindh topic set: **A** IUD insertion, **C** injectables, **D** implant insertion,
**G** IUD removal and **H** implant removal. B (postpartum IUD training) and E/F
(sterilization) do not count as included training. Topic sets differ across the
country-specific source scripts; this is not a harmonization of their definitions.

Within a staff record, an included-topic Yes establishes training. A complete set
of included-topic No responses, explicit None (Z), or an interpretable full text
selection with no included topic establishes no included training. A partial set
of No checkboxes alone does not establish a negative. Explicit Don't know (Y)
remains unknown even when the selected-topic checkboxes contain zeros. Conflicting
text/checkbox selections, None with a substantive topic, Don't know with a topic,
malformed selections and otherwise unresolved responses are excluded from the
known-status denominator. This is a documented Sindh correction, not unchanged
application of the original missing-response code.

For each facility calculate `known trained staff / staff with known status`.
Genuine zero proportions remain valid; zero known staff gives an undefined ratio.
Average valid facility proportions equally within district among Q506 Yes
facilities. This is not a pooled proportion of all district personnel.

The revised ratio is defined for **2,811 of 2,823 FP facilities (99.6%)**. It uses
**15,116 staff with known status**, of whom **4,809** have included training.
There are 57 explicit Don't-know records and two malformed records; six of the
Don't-know records also have contradictory topic selections, so those counts overlap.
Unknown staff are not imputed and may differ from staff with known status.

The original cleaned inputs stay unchanged. In the analysis working copy only,
`p_train`, `n_tot` and `n_train` are replaced together so mean and pooled summaries
use coherent numerators and denominators. Original values and reconstruction
counts are retained in private facility-level audit outputs; district comparisons
and classification changes are included in the HTML. Other inherited training
summaries, including `mdn_skill_lvl`, retain their original definitions and are
not selected index components. `tests/test_sindh_training.R` checks response
scenarios; report verification independently reconstructs staff statuses from raw
responses and checks all district means and assignments.
The interpretable overlap contains 3,696 staff and 18,480 selected-topic
comparisons, with no text/checkbox disagreements; unknown and contradictory
responses are not used to justify text recovery.

To compare an earlier build with this revision, run
`Rscript tests/compare_sindh_training_update.R --previous-dir PREVIOUS_ANALYSIS_DIR --current-dir CURRENT_ANALYSIS_DIR --output output/sindh_training_validation.json`.
This requires the saved analysis directories, not just their HTML reports. The
checked release records are summarized in `tests/sindh_training_validation.json`.

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
Average six risk-aligned component z-scores separately for demand and supply constraints.

Raw stockout burden is higher = worse. Retain it as a constraint and complement
the other five provision measures before supply standardization. All six index
inputs run higher = worse. Heatmap percentages remain in their original units.

Both final indices have mean zero across the 30 reference districts. Flag demand
**> 0** and supply constraints **> 0**, with 1e-10 ties unflagged. Categories are
both, demand only, supply only or neither flagged. They describe relative concerns
and provision, not causal binding constraints or absolute service adequacy. Equal
weighting permits offsetting strengths/weaknesses and can emphasize correlated
components. Component correlations and valid denominators remain available.

Top-three selected components use the largest demand or supply-constraint component
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
a district demand score. Her supply risk is district constraints; all
residents share the district score. Scores enter per original average-z point.

Retain **Normalizedweights**. Overview intervals use `ids = ~1`, without strata or FPC.
Neither `clusterCode` nor `rb0e` is specified as a sampling PSU in this analysis.
Primary model inference uses **district-clustered CR2** covariance, Satterthwaite
coefficient intervals, HTZ block tests and Holm correction within each joint model's
two block tests. No additional sampling-cluster survey-GLM sensitivity is fitted. Original strata, FPC and full design documentation remain unavailable.

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

## Shared survey and scoring implementation

Use `survey::svymean()` for SURGO weighted estimates and `survey::svyglm()`
for the existing modern-use models, with `ids = ~1`, no strata or FPC, and
the supplied weight. Report the separately calculated administrative-area CR2
standard errors with Satterthwaite tests. Supply constraints are positive when
above average; this reverses the former readiness sign without changing the
primary flags or the supply-risk values already used in models.

The final Sensitivity analyses section exposes the four earlier scoring
alternatives. The map appendix includes the final-score distribution and zero,
0.5, 1 and 1.5 SD thresholds. See the project-level
[survey and scoring methods](../../../docs/SURVEY_AND_SCORING.md) for denominators,
reference sets, inference details and publication-review requirements.


## Regression sensitivity checks

The administrative-area report includes a separate **Sensitivity checks** section
at the end of its appendix. It retains the current need definition, personal demand
coding, district-wide supply assignments and fixed score references. The main models,
heatmaps and classifications are unchanged. This does not modify the separate
residence-specific analysis.

1. Describe all 3,274 women in need, comparing 3,115 included with
   159 excluded women on weighted modern use among known outcomes,
   demographics, unresolved-response drivers and geographic distribution.
2. Compare the identical supply-only formula in the common sample and its
   broader eligible sample of 3,263 women, without requiring demand responses.
3. Fit six single-demand-indicator models in the common and formula-specific
   eligible samples. Each also includes supply risk and the original covariates.
   These and the supply-only check use original SURGO weights,
   `survey::svyglm()`, `ids = ~1`, no strata/FPC, and district-clustered
   CR2/Satterthwaite inference.
4. Refit both joint z-score specifications using original weights divided by an
   estimated probability of complete demand information. The response model uses
   known modern use, the agreed covariates and district indicators. Supply risk is
   constant within district and is not redundantly added alongside those indicators.
   Fully observed areas within the broader eligible target receive probability 1;
   the response model is fitted in areas with both complete and incomplete records.

For response-weighted inference, delete each of the 30 districts and re-estimate both the response and outcome
   models. The centered jackknife covariance is `(G-1)/G` times the crossproduct of
   centered replicate coefficients; intervals use `G-1 = 29` degrees of freedom.
   The survey-only joint comparison uses the same jackknife. Primary report
   intervals remain CR2/Satterthwaite. This geographic inference does not assert
   that district is the original sampling PSU.

No unknown demand response is recoded or imputed, and no weights are trimmed.
Response weighting still uses the 3,115 complete women and targets the 3,263
women with a known outcome, covariates and supply score; it does not recover women
whose supply profile or outcome is unknown. Unresolved answers can be substantive
Don't know or inconsistent selections, so the observed-information selection
assumption remains untestable. Effective sample sizes describe weight concentration,
not formal power or the full survey design. Score-construction uncertainty is not
propagated by this regression sensitivity.

The entry script runs the shared `regression_sensitivity.R`,
`verify_regression_sensitivity.R`, `plot_regression_sensitivity.R` and
`render_regression_sensitivity.py` in `administrative_area/analysis/`.
Independent checks enumerate model rows, original weights, raw weighted GLM
coefficients, response probabilities and every refitted jackknife replicate.
Only aggregate results and figures enter HTML. Working respondent data and fitted
objects remain in the caller's work directory and are excluded from shared files.

Methods: [response weighting](https://pubmed.ncbi.nlm.nih.gov/21220355/) and
[cluster jackknife for binary response](https://arxiv.org/abs/2406.00650).

Partnership remains constant and Rural/Urban is unavailable: the original Sindh
adjustment set omits both. The corrected Q406 training measure is retained.
