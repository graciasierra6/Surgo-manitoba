# Lagos: LGA-wide supply analysis

This module extends the verified administrative-area join. Every woman receives
facility averages for her own LGA, combining Rural and Urban facilities. Her own
settlement remains an adjustment variable. The original residence-specific Lagos
report and all input joins are preserved.

From the repository root:

```sh
Rscript administrative_area/analysis/lagos/Lagos_analysis_aggregate.R \
  --join-dir administrative_area/output \
  --cache-dir administrative_area/data/cache \
  --work-dir output/aggregate_reports/lagos \
  --output-dir reports
```

Supply the actual verified administrative join directory at runtime. It must
contain the checked woman-level join and facility audit inputs. For a new input
bundle, `--input-zip "$SURGO_MANITOBA_INPUT_ZIP"` and `--cache-dir data/cache`
invoke the existing bundle verification and linkage preparation first. Pandoc
must be available; its directory can be supplied with `--pandoc-dir`.

`Lagos_analysis_aggregate.Rmd` renders `reports/Lagos_analysis_aggregate.html`. It can be
knitted with the same runtime parameters. Inputs, intermediate woman frames,
fitted objects and caches are excluded from shared artifacts. Internal paths
are relative; external paths are caller supplied.

## Population and measurement

- Retain all 4,555 source women. Section 2 onward uses the original `fpn2 = 1`:
  2,159 women. No reconstructed need variable or additional permanent-protection
  or sexual-activity recoding is applied.
- Demand definitions are shared explicitly with `analysis/lagos/lagos_definitions.R`.
  The six domains remain general healthcare permission, anticipated stigma,
  health/side-effect beliefs, future-fertility concerns, method confidence and
  opposition to FP. Confidence is not objective knowledge.
- Use 1,447 facilities across the 17 shared LGAs, retaining 294 facilities in
  three facility-only LGAs for coverage. The existing Ifako-Ijaiye/IFAKO-IJAYE
  spelling alias is checked explicitly against the administrative join.
- Supply domains are original UoM short/long-acting offering, free-product share,
  trained-provider share, Q702 counselor availability, three-LARC product/category
  stock breadth and most-popular-product stock. The stock-breadth rule does not
  mean three distinct clinical LARC methods.
- Average valid facility proportions equally; binary flags become valid-response
  shares. Include valid zeros and FALSE, exclude undefined responses per measure.
  Facility weights are not supplied or constructed. Preserve the previously
  authorized zero for one Ibeju-Lekki Urban facility's free-product proportion;
  the original source value stays missing. No other missing value is zero-filled.
- Main classifications average six component z-scores separately by side and
  use separate final-score medians. Normalization gives each of the 17 LGAs equal
  contribution. A missing component prevents a complete score; median ties are
  unflagged. Labels describe relative priorities, not causal binding constraints.
- Include component heatmaps, five-method classifications, interactive coverage
  and classification maps, priority tables for the six supply-only and six demand-only LGAs,
  frequency summaries, and the complete 17-LGA planning table.

## Models and inference

Five models have modern contraceptive use as the outcome. Use individual demand
scores or six personal responses, and the supply risk score from her LGA-wide
facility means. Adjust for age, age squared, education, partnership, fertility goal
and settlement. All use the same 1,865 women, including 943 modern users, so model
comparisons do not mix woman samples. Retain original `weight`, mean-one fitting
rescaling, geographic LGA-clustered CR2 covariance, Satterthwaite intervals, HTZ
block tests and Holm correction within each joint model's two tests.

Official survey EA/PSU and stratum identifiers remain unconfirmed. Overview design
intervals are withheld; the geographic model intervals are not presented as the
full original survey design. Geographic Taylor intervals are explicitly unavailable
when their residual design degrees of freedom are nonpositive. Seventeen contextual
supply values and occasionally low effective degrees of freedom require caution.
No random effects or interactions are fitted. Diagnostics do not establish power
or causality, and uncertainty in constructing scores is not propagated.

## Rendered appendices

1. Five scoring approaches: rank average, original percentages, median/IQR,
   median count and average z-scores. Median count flags four of six components.
2. Separate weighted individual-demand and assigned LGA-supply PCAs, including
   PC1 coefficients, variance, permutations, geographic deletion checks and models.
   Mixed supply signs are labeled as a pattern, not uniformly worse provision.

The complete-facility and common-woman demand sensitivity sections are omitted
from the current HTML. Existing diagnostic calculations remain internal for
reproduction and audit; they do not replace the main valid-response estimates.
The main models retain their documented woman-level complete-case requirements.
The main component, classification, frequency and detailed priority tables remain
unchanged, including FP counseling and response-coverage explanations.

## Missingness and verification

Among 2,159 women, unresolved health concerns affect 147, future fertility 129,
confidence 13 and opposition 23; agency and stigma have no unresolved responses.
Among 1,447 facilities, undefined values affect free provision 606, training 211,
counseling 272 and popular stock 819. Cleaned offering and stock-breadth flags are
complete; this does not certify complete underlying questionnaire responses.
Unknown stock is not an observed stockout or proof of no FP service.

`audit_missingness.R` diagnoses these two indicators without changing the report.
Supply `--raw`, `--facility-input`, `--uom-rdata` and `--output-dir` at runtime.
It independently recovers the original free-product counts, tabulates missingness
by LGA and FP provision, and distinguishes unidentified popular products from
missing stock responses. Of 606 undefined free proportions, 380 records report
no FP service, 184 lack a Q506 answer and 42 report FP service. Of 819 undefined
popular-stock flags, 812 have no identified popular product, five combine an
out-of-stock IUD response with missing brand status, and two concern methods
without a comparable commodity-stock item. The male-condom question is included
in Lagos. These diagnostics do not recode unknowns or revise classifications.

`verify_analysis.R` independently reconstructs facility means/denominators, woman
assignments, weighted concern estimates, standardization, thresholds, fixed-reference
sensitivities, weighted coefficients, CR2 covariance and PCA using weighted SVD.
`verify_html.py` checks tables, maps, report order, embedded assets and portability.
These checks validate reproduction, not missing-data assumptions or construct validity.
