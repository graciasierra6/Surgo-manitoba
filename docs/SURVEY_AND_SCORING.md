# Survey estimation and constraint scores

## Scope

These methods apply to the four current administrative-area analyses and the
Lagos, Senegal and Côte d’Ivoire residence-specific analyses. Sindh has no usable
woman-level Rural/Urban field and remains district-only. The relevant R scripts,
shared helpers and supplementary analyses use the same documented SURGO weighting
framework. Facility aggregation, sample counts, recodes and equal-area index
references keep their distinct statistical roles.

Current complete report scripts are under `aggregate/<country>/` and
`rural_urban/<country>/`, beside their Rmd and HTML. Earlier definitions and the
previous folder layout remain in Git history. Shared statistical implementations
are in `support/`; country calculations are included in each country R script.

## SURGO weights and population

| Setting | Original weight field | Area used for regression error clustering |
|---|---|---|
| Lagos | `weight` | LGA |
| Senegal | `WEIGHT` | Département |
| Côte d’Ivoire | `WEIGHT` | Département |
| Sindh | `Normalizedweights` | District |

The prepared woman-level field `weight` retains the supplied values. Sindh's
selection matches the uploaded upstream pipeline. Its normalized weights are
scaled differently by district relative to `RawWeights`; suitability for a
particular pooled population target still requires the producer's weighting
documentation. This update does not substitute or reconstruct weights.

From Section 2 onward, retain the existing women-in-need population: original
SURGO FPN2 = 1 in Lagos, Senegal and Côte d’Ivoire, and the previously authorized
Pakistan-instrument-derived flag in Sindh. Keep the existing item definitions,
missing-response rules, outcomes, covariates and common model samples.

`support/survey_methods.R` provides:

- `survey_weighted_mean()`: a wrapper around `survey::svymean()` with the existing
  valid-response denominator. Weighted heatmap cells, use distributions,
  missingness percentages, score-reference means and marginal prediction averages
  use survey commands. An unweighted sample N is still a count of records.
- `surgo_design()`: original weights, `ids = ~1`, `strata = NULL`, no FPC.
- `fit_survey_logistic()`: `survey::svyglm()` with a quasibinomial logit link.
- `geographic_cr2()`: the same administrative-area CR2 correction used previously.

The original survey PSUs and strata are not specified in the main regression
model. The administrative error clusters are not asserted to be sampling PSUs.
A weights-only design is not a reconstruction of the full original sampling
plan. Descriptive confidence intervals follow the explicitly specified design;
changing a variance specification can change intervals without changing a mean.

## Regression inference

All five primary model specifications retain modern-method use as their outcome.
Where PCA models already exist, they use the same outcome and inference method.
Age, age squared, education and fertility goal remain controls. Partnership and
Rural/Urban are retained in Lagos, Senegal and Côte d’Ivoire; Sindh partnership
is constant in the analytical sample and its Rural/Urban field is unavailable.

Model fitting uses `svyglm()` with weights and no sampling clusters or strata.
Reported uncertainty is then calculated with CR2 at the area level, using
Satterthwaite degrees of freedom for coefficients and HTZ block tests. In
residence-specific models, cluster at the administrative area, not at the smaller
residence-profile cell. Retain the existing within-model Holm adjustments.

The adapter requests the documented CR2 estimating-equation meat and applies the
inverse weighted logistic information matrix. It does not use the already
sandwiched survey covariance as the GLM bread, change the fitted object's class,
or add two variance estimators together. Independent weighted-GLM/CR2 audit fits
remain as reference calculations in verification scripts.

The native `summary()` of the saved `svyglm` object describes its own weights-only
survey covariance. The reported clustered results are in
`model_coefficients.csv`, `model_block_tests.csv` and `model_results.json`; their
covariance is saved separately as `V`. Curves and probability gaps use that same
clustered covariance and the existing delta-method calculations.

## Area classification and score direction

The six demand inputs are survey-weighted percentages among women in need within
each area. The six supply inputs are equal-facility means of valid responses
among confirmed FP providers. Numeric zero and FALSE are included; missing,
inapplicable and undefined ratios are excluded separately for each measure.
No SURGO weight is applied to a facility record.

For the primary score, align every component so that higher means more measured
constraints. Demand indicators already have this direction. Retain stockout
burden as a risk, and use `1 - provision` for the other five supply components.
Standardize each component against the same complete-area reference mean and
sample SD used previously, with equal weight per area. Average the six component
z-scores. Raw heatmap percentages are unchanged.

The new supply score is the negative of the earlier readiness score. Thus
`readiness < 0` and `constraints > 0` give identical flags. Demand and supply
constraints are both flagged strictly above zero; numerical ties within 1e-10
are unflagged. Unknown supply remains unassessed, rather than becoming zero or
neither. The four joint categories remain demand only, supply only, both and
neither flagged.

Woman-level demand scores keep each woman's six responses and the existing
weighted need-group reference means and population SDs. Woman-level supply scores
keep the assigned administrative or residence/fallback supply profile and the
fixed area component references. The sign harmonization does not change the
supply-risk values already used in the regressions.

## SD maps and distributions

`support/risk_scores.R` calculates distributions of the final composite
scores, separately for demand and supply. Each side uses all areas with its own
finite scores; Côte d’Ivoire therefore retains different demand and supply
reference counts. Divide the distance from that side's area mean by its final
composite sample SD. This is not division by a component SD or a standard error.

The map appendix shows four rules: the primary above-zero rule, and constraints
at least 0.5, 1 or 1.5 final-composite SDs above the area mean. Distributions show
all four reference lines and area counts. These are descriptive alternatives,
not validated family-planning severity thresholds or confidence intervals.
Geometry, colors, missing-coverage categories and weighted CPR/mCPR tooltips are
preserved. Planning maps remain at the administrative-area level in both workflows;
residence-specific exposure is retained for the individual models.

## Sensitivity analyses

The complete country scripts include the implemented sensitivity analyses.
Shared score functions implement the alternatives once:

| Method | Construction | Flag rule |
|---|---|---|
| Rank average | Rank each risk-aligned component and average six ranks; average ranks for ties | Strictly above that side's score median |
| Mean percentages | Average six risk-aligned percentages | Strictly above that side's score median |
| Median/IQR | Subtract each component's area median, divide by its IQR, average six values | Strictly above that side's score median |
| Median count | Count components strictly above their own area medians | At least four of six |
| Z-score average, primary | Subtract each component's area mean, divide by its sample SD, average six values | Strictly above zero |

If a reference IQR is zero, the median/IQR alternative is undefined for that side;
no substitute scaling, component deletion or partial-index averaging is used.
PCA remains a separate sensitivity where previously implemented. Its projections
and weighted linear algebra retain their existing definitions; mixed PC1 signs
still require a pattern-score interpretation. A sign label cannot make mixed
loadings a monotonic risk index.

## Regression-completeness checks in aggregate reports

The four aggregate reports include included/excluded profiles, genuinely broader
formula-specific available-case fits and response-weighted joint models. The
supply-only available sample does not require a complete personal demand score.
Each single-indicator comparison includes supply and the agreed covariates.
All use survey commands with the original weights and administrative-area CR2
inference. Main common-sample models remain unchanged.

Response-weighted joint models use original survey weights multiplied by inverse
estimated completeness probabilities, targeting women with known outcome, controls
and supply. A weighted response model uses observed modern use, controls and area
indicators. Areas fully observed in that target have response probability one.
Delete one area at a time and refit both the response and outcome models; compare
survey-only and response-weighted fits with the same geographic jackknife and
G-minus-one t intervals. These checks retain fixed score references and rely on
observed-information selection assumptions. Substantive Don't know responses are
not automatically recoded or imputed, and this does not establish ignorability of
unrecorded concerns. The new appendix is scoped to aggregate reports, not the
three existing Rural/Urban analyses.

## Sindh training correction

Current Sindh training is reconstructed from raw Q406 topic checkboxes and text
responses using the selected Sindh A/C/D/G/H topics. Known explicit None is zero;
Don't know, contradictory or malformed responses remain unresolved. The facility
proportion uses trained known-status personnel divided by known-status personnel.
Valid facility ratios are averaged equally among confirmed FP providers within
district. Other settings retain their supplied cleaned training variables. The
original raw/cleaned files remain unchanged; reconstruction and independent
verification are included in `aggregate/sindh/Sindh_analysis.R`.

## Validation and report publication

Rebuild into a caller-selected output directory before replacing published
reports. Compare weighted percentages, valid denominators, primary categories,
model samples, scores, coefficients, clustered covariance, intervals, block tests
and adjusted predictions against the previous outputs. Display labels, the
supply sign and new SD-map sections intentionally differ. No equivalence claim
should be based only on changing the name of the fitting function.

Input surveys, extracted caches, fitted objects and validation working data remain
outside the upload. Current published HTMLs have been rebuilt with this implementation. Source and
report signatures are registered together, with prior published hashes retained
separately for provenance. Presentations were not changed.

References: [survey means](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/surveysummary.html),
[survey GLMs](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyglm.html),
[CR2 for GLMs](https://jepusto.github.io/clubSandwich/reference/vcovCR.glm.html),
[JRC normalization](https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-5-normalisation_en).
