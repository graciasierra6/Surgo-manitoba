# Lagos — updated aggregate analysis

This is the current administrative-area workflow. It replaces the earlier report
under its stable filename; separate residence-specific reports are unchanged.
Original input datasets and working outputs are external or ignored, not uploaded.

## Reproduce

Run from the existing repository root:

```sh
Rscript administrative_area/analysis/lagos/Lagos_analysis_aggregate.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --work-dir output/aggregate_reports/lagos \
  --output-dir reports
```

Or reuse a verified administrative join and extracted input bundle:

```sh
Rscript administrative_area/analysis/lagos/Lagos_analysis_aggregate.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --cache-dir "$SURGO_MANITOBA_CACHE_DIR" \
  --work-dir output/aggregate_reports/lagos \
  --output-dir reports
```

Optional `--project-root`, `--python` and `--pandoc-dir` select existing installed
resources at runtime. The adjacent R Markdown supports the same parameters.
This module uses the repository's shared plotting, input, template and map helpers;
the entry script is not standalone. Working data and fitted models stay in ignored
output folders and are not part of the code package.

## Population and facility aggregation

Keep all 4,555 original main-sample women and every original survey column.
Coverage uses all women and all 1,741 facility records across 20 LGAs. Section 2
onward uses original SURGO `fpn2 = 1`: 2,159 women. No historical need reconstruction,
additional sexual-activity rule or sterilization/hysterectomy recoding is applied.

Facility eligibility is **Q506 = 1 (Yes)**: “Does this facility provide any family
planning services at the facility?” There are 1,040 Yes, 465 No and 236 unanswered
records; 882 Yes facilities are in the 17 surveyed LGAs. Join raw data by `key`
against cleaned `fid`; raw numeric `fid` is not the same identifier. Validate raw
district names, the checked LGA assignment and the Ifako-Ijaiye/IFAKO-IJAYE alias.

Reaggregate all original supply columns and archetype counts/shares among eligible
FP providers. For each mean, retain valid zeros/FALSE, exclude unknown/undefined
values for that measure, and weight facilities equally. Do not require a common
complete-facility sample. Selected component means retain full precision; display
rounding follows scoring. Broader inherited flags retain the original pipeline's
five-decimal output convention.

Assign each woman the FP-provider profile for her own LGA, combining Rural and
Urban facilities. Preserve her own Rural/Urban residence as a model covariate.
The prior Ibeju-Lekki Urban free-product zero exception concerns a Q506 No facility
and is excluded by the eligibility rule. No undefined value is zero-filled here.

| Selected component | Facility input | LGA interpretation |
|---|---|---|
| Short + long-acting offering | `one_larc_3_sa_offer` | Share meeting the original offering criterion |
| Free-product provision | `p_free_prod` | Mean facility proportion of recorded products provided free |
| Trained-provider share | `p_train` | Mean facility proportion of recorded personnel trained |
| Daily provision | Frequency ratio below | Mean facility proportion of offered modern-method classes provided daily |
| At least one LARC in stock | `one_larc_stockin` | Share meeting the original current-stock criterion |
| Stockout burden | Original cleaned `p_any_stockout` | Mean facility share of coded product groups with a stock interruption |

Counseling and popular-product stock remain in the broader inventory but are not
selected components of this report's indices, component tables or models.

### Daily provision

Seven classes use Q507: pills (`q507ad`), injectables (`q507ag/ah/ai`), male/female
condoms (`q507ae/af`), ECP (`q507ao`), IUD (`q507aa/ab`), and implants (`q507aj`).
Collapse IUD and injectable variants to the most frequent recorded offered variant,
so a class counts once. Any Daily variant establishes Daily. If no variant is
offered, classify Not at all only when every variant says so; otherwise unknown.
Partial source information is flagged and interpreted conditionally on recorded
offering, not assumed absent. Exclude removal services, permanent methods and
other categories from this seven-class measure to retain the agreed definition.

Codes: 1 Daily, 2 Weekly, 3 Fortnightly, 4 Monthly, 5 Not at all. Within a facility,
`Daily classes / classes coded 1:4`. Code 5 is excluded from the offered denominator.
Zero offered classes produces an undefined ratio. Genuine zero daily classes with
a positive offered denominator remains zero. Average valid facility ratios within
LGA. Higher daily frequency can coexist with narrow method choice; it is not an
independently validated access standard.

### Demand

Six personal domains remain HS7C healthcare permission; PB7B/C anticipated stigma;
**PB2A only** health/side-effect beliefs; coherent MP4_a–i/MP4_99 coding of concerns
about reversible methods' future fertility effects; MC5A/B low method confidence
under the retained knowledge label; and FP1 opposition to family planning.
MC5 is confidence/self-efficacy, not objective knowledge. Special and unanswered
responses remain unresolved; zero requires known negative constituent responses.

## Geographic scores and planning tables

Demand component percentages use final woman weights among women in need and
valid domain responses. Supply components use unweighted valid FP-facility means.
All 17 linked LGAs have complete aggregate profiles. Use the same 17 LGAs as the
reference for every component within each side, giving each LGA equal contribution.

Heatmaps display raw stockout burden, higher = worse. Before every score, ranking
and PCA, replace burden with **continuity = 1 - burden**. The six supply inputs
then run higher = favorable. For each component subtract the LGA mean and divide
by the sample SD. Average six component z-scores separately by side; each final
score has mean zero. Demand > 0 flags above-average concerns; readiness < 0 flags
below-average provision. Values within 1e-10 of zero are unflagged ties. These are
relative planning signals, not validated absolute thresholds or causal constraints.

Alternative rank-average, mean-percentage and Median/IQR approaches use separate
final-score medians; median count flags at least four unfavorable component
comparisons. All methods use the same direction alignment. Top-three selected components
use component z-scores, not regression coefficients or raw percentage ordering.
Recurrence and detailed tables include all LGAs flagged on each side, including
both-flagged LGAs. The two side groups overlap; percentages describe LGAs.

Equal-weight standardization/aggregation is a recognized composite-indicator
construction, not validation of this particular component list or zero cutoff.
Correlations, offsetting strengths and weaknesses, extreme values and nonresponse
affect interpretation. Compare component values and alternative classifications.

## Individual models and inference

Five primary models have **modern contraceptive use only** as outcome:
personal demand score; assigned supply risk; both scores; six personal demand
indicators; and those indicators plus supply risk. All adjust for age, age squared,
education, partnership, fertility goal and Rural/Urban residence. Every fit uses
the same complete model sample; incomplete records remain in eligible overview
estimates wherever their required responses are valid.

Personal demand scores standardize each woman's six responses using item-specific
weighted mean and population SD among women in need, then average. Do not assign
her an LGA-level demand score. Supply risk is negative LGA readiness, so higher
values mean weaker provision; all women within an LGA share it.

Retain original `weight`, weighted logistic point estimates, LGA-clustered CR2
covariance, Satterthwaite coefficient intervals, HTZ block tests and Holm adjustment
within each joint model's two block tests. No random effects or interactions.
Official survey EA/PSU and strata remain unconfirmed; do not infer them from
household IDs or probability fields. Overview intervals are withheld. LGA Taylor
comparisons are geographic inference checks, not the full original survey design.
Seventeen contextual values and low effective degrees of freedom limit supply
inference. Convergence and VIF checks do not establish adequate power or causality.

Appendices retain the five scoring comparisons and updated weighted PCA analyses,
including the same five models. Stock continuity enters PCA before fitting.
Inspect signed coefficients, variance retained and exploratory permutation/deletion
checks. Model intervals condition on estimated scores and do not propagate their
construction uncertainty. Complete-facility/common-demand sensitivity sections are
not reintroduced.

## Verification

Checks independently recover raw survey fields, Q506 eligibility, raw LGA names,
daily frequency, original cleaned stockout values, valid-response denominators,
every woman assignment, weighted concern rates, aligned component z-scores,
zero-cutoff classifications, personal demand scores, weighted coefficients,
LGA CR2 covariance/HTZ tests and PCA through weighted SVD. HTML checks inspect
maps, section order, definitions, embedded assets and aggregate downloads.
These verify implementation; source-data quality and identification limitations
remain explicit. Shared files contain no respondent-level data or personal paths.
