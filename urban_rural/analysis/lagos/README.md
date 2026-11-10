# Lagos: updated Rural/Urban supply assignment

Build `reports/Lagos_rural_urban_updated.html` from the parameterized input bundle:

```sh
Rscript urban_rural/analysis/lagos/Lagos_rural_urban_updated.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

Alternatively, reuse a verified residence-specific join and its extracted inputs:

```sh
Rscript urban_rural/analysis/lagos/Lagos_rural_urban_updated.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --cache-dir "$SURGO_MANITOBA_CACHE_DIR"
```

Optional arguments: `--work-dir`, `--output-dir`, `--python` and `--pandoc-dir`.
Their defaults are project-relative. Inputs, working woman-level datasets, model
objects and intermediate results remain in ignored directories and must not be
uploaded. The Rmd uses the same pipeline; a prepared HTML fragment can be supplied
for rendering without refitting. This module depends on the current
`administrative_area/analysis/lagos/` helpers and shared project definitions.

## Two geographic views

- **Planning heatmap, classification map and component tables:** all confirmed FP
  providers within each LGA, combining Rural and Urban. This retains the current
  aggregate report's planning definitions and classifications. Do not take an
  unweighted mean of Rural/Urban means to obtain the LGA mean.
- **Woman-level model exposures:** equal-facility valid-response means within the
  woman's exact LGA and recorded Rural/Urban setting. Rural women receive rural
  facility summaries; urban women receive urban summaries. If the exact cell has
  zero confirmed FP providers, assign FP-provider means for the same LGA and flag
  the broader resolution. Undefined components in cells with eligible providers
  are not automatically filled.

All 4,555 original women and survey fields are retained. Coverage precedes the
need restriction; Section 2 onward uses original SURGO `fpn2 = 1` (2,159 women).
There are 18 observed residence cells in 17 LGAs: all sampled Badagry women are
Rural; only Epe has sampled women in both settings. Its rural and urban supply
profiles are calculated separately.

Ibeju-Lekki–Urban has one recorded facility, which answered Q506 No. The updated
FP-provider rule therefore leaves no eligible facility for its exact cell.
The 157 resident women, including 62 in need, receive Ibeju-Lekki's LGA-wide
FP-provider means, with `link_supply_fallback = TRUE`, the source-profile key
`IBEJULEKKI|LGA` and a broader-resolution label. Their recorded Urban residence is
unchanged. These are the LGA's recorded FP conditions, not measured Urban service
conditions. All 18 assigned profiles are complete (17 exact, one fallback), across
17 LGAs. The prior one-facility free-provision zero exception is not used. Original
source women are retained; model exclusions require missing model variables.

## Measures, scores and inference

Only facilities answering Q506 Yes contribute to analysis supply means. Retain
valid numeric zero/FALSE; exclude undefined or missing responses separately by
measure. No facility weights and no common complete-facility restriction.

The six measures are short/long-acting offering (`one_larc_3_sa_offer`), mean free
product share (`p_free_prod`), mean trained personnel share (`p_train`), daily
provision among offered modern-method classes, at least one LARC in stock
(`one_larc_stockin`), and original cleaned stockout burden (`p_any_stockout`).
Daily provision is calculated from Q507: Daily classes / recorded offered classes
(Daily, Weekly, Fortnightly or Monthly), across seven modern-method classes.
IUD/injectable variants count once, using the most frequent recorded offered
variant. No recorded offered class means an undefined ratio, not zero.

Demand uses the same six personal measures as the current aggregate report:
HS7C general healthcare permission; PB7B/C anticipated stigma; **PB2A only** for
health beliefs; coherent MP4 reversible-method fertility concerns; MC5A/B method
confidence; FP1 opposition. The retained label "Low method knowledge" refers to
confidence and is not an objective knowledge test.

Geographic planning scores average six component z-scores against the 17 linked
LGA means and sample SDs. Supply is favorable-aligned, replacing burden with
continuity = 1 − burden. Demand > 0 and readiness < 0 are flagged, with 1e-10 tie
tolerance. These are relative concern/readiness classifications.

Each woman's model demand score uses her own six responses, standardized with
item-specific final-weighted means and population SDs among valid women in need.
Her model supply risk is the negative average of her **assigned supply profile's**
six favorable components standardized using the fixed 17-LGA planning reference.
This includes the explicitly flagged LGA-wide profile where the exact cell has
zero FP providers. The common scale preserves comparability with the aggregate analysis. Assigned
residence scores are not recentered and need not average zero. No LGA demand score
is substituted for personal responses. PCA sensitivity likewise uses her own demand
responses and exact assigned supply measures; mixed signs remain disclosed.

Five models share one complete sample (1,896 women; 960 modern users in the
verified source snapshot) and current modern contraceptive use as
outcome: demand score; supply risk; both; six personal demand indicators; and six
indicators plus supply risk. All adjust for age, age squared, education,
partnership, fertility goal and Rural/Urban. Final `weight` is retained. Inference
uses **LGA-clustered CR2**, Satterthwaite intervals and HTZ block tests; Holm adjusts
the two tests within each joint model. No random effects or interactions.
Residences are not treated as new independent geographic clusters.

Official Lagos EA/PSU and stratum identifiers remain unconfirmed. Weighted
population point estimates are reported, with overview design intervals withheld.
LGA clustering supports geographic inference, not a reconstructed full survey
design. Score-construction uncertainty is not propagated into model intervals.
Limited contexts, selection into the common sample and cross-sectional measurement
prevent causal binding-constraint or investment-allocation claims.

## Verification

The pipeline checks original fields/rows, raw facility geography and residence,
Q506 eligibility, exact-cell denominators, all six assignment formulas, the
zero-provider fallback trigger and all six LGA fallback means, LGA planning scores/cutoffs, personal scores, weighted
coefficients, CR2 covariance, joint tests, independent PCA reconstruction and HTML
assets. It saves detailed checks in the ignored build directory. Validation is an
implementation check, not a formal power analysis or validation of source coding.
The existing aggregate and older residence-specific HTMLs are preserved.
