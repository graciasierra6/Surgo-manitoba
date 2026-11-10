# Côte d’Ivoire — updated aggregate analysis

This is the current administrative-area workflow. It replaces the earlier report
under its stable filename; separate residence-specific reports are unchanged.
Original input datasets and working outputs are external or ignored, not uploaded.

## Reproduce

Run from the existing repository root with parameterized external inputs:

```sh
Rscript administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP" \
  --work-dir output/aggregate_reports/cotedivoire \
  --output-dir reports
```

A verified administrative join and extracted bundle can also be reused:

```sh
Rscript administrative_area/analysis/cotedivoire/Cote_dIvoire_analysis_aggregate.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --cache-dir "$SURGO_MANITOBA_CACHE_DIR" \
  --work-dir output/aggregate_reports/cotedivoire \
  --output-dir reports
```

The named entry script is **Cote_dIvoire_analysis_aggregate.R**.
The complete module uses the existing shared project helpers, plotting functions,
country coding, report styles and boundary assets. The entry script is not
standalone. Optional `--project-root`, `--python` and `--pandoc-dir` select the
project and installed runtimes. The Rmd accepts equivalent join/cache/bundle
parameters. No machine-specific input locations are embedded in shared files.
Working data and fitted objects remain in ignored output folders, outside uploads.

## Facility eligibility and aggregation

Restore every original facility ID and the checked geographic assignments.
Restrict to **s2_5_sp = 1**: the facility reports providing FP services. This is
not the differently numbered outreach question. It also does not require Yes
to the separate **s5_1** on-site provision question. Among the 2,723 general
FP providers, 2,614 report on-site provision and 109 report No. The latter remain
eligible under the selected general-service rule; on-site status is audited,
not used as another filter. There are 2,723 eligible
facilities among 3,078 source records, including 2,370 in the 78 surveyed areas
with facility matches. All 100 facility-side geographic groups contain FP providers.

Reaggregate all original supply columns, archetype counts/shares and other joined
summaries on that eligibility restriction. For each mean, include valid zeros and
FALSE and exclude unknown/undefined values for that measure. Facilities contribute
equally; no facility weights or pooled-product replacement is introduced.

The six selected components are:

| Component | Facility input | Département summary |
|---|---|---|
| Method offering | `one_larc_3_sa_offer` | Mean binary flag |
| Free-product provision | `p_free_prod` | Mean facility proportion |
| Trained-provider share | `p_train` | Mean facility proportion |
| Daily provision | New frequency ratio below | Mean valid facility ratio |
| LARC in stock | `one_larc_stockin` | Mean binary flag |
| Stockout burden | Original cleaned `p_any_stockout` | Mean facility proportion |

Counseling and popular-product stock remain in the broader joined inventory but
are excluded from all selected indices, component tables, classifications and models.

### Daily provision

Use `s5_2a`, `s5_2b2m`, `s5_2b3m`, `s5_2c`, `s5_2d`, `s5_2e`, `s5_2f`,
`s5_2g3a`, `s5_2g5a`. These represent seven classes: pills, injectables,
male/female condoms, ECP, IUD and implants. Combine injectable variants and implant
variants so each class counts once. Use the most frequent recorded offered variant:
any Daily variant establishes Daily for the class. A recorded offered variant plus
an unknown variant is interpreted using the recorded information and remains
flagged as partial source information; unknown variants are not assumed absent.
If no variant is recorded offered, classify Not at all only if every variant is 5;
otherwise the class is unknown.

Codes: 1 Daily; 2 Weekly; 3 Bi-monthly; 4 Monthly; 5 Not at all; -9 and other
unrecognized responses are unknown. No conversion of code 3 into days is needed.
Within each eligible facility, `count(Daily classes) / count(offered classes 1:4)`.
Code 5 enters neither count. A zero offered denominator is undefined; a genuine
zero daily numerator with a positive denominator remains a valid zero.

There are 2,249 defined ratios and 121 undefined ratios among linked FP providers.
Partial method responses are retained conditionally on recorded offered classes;
coverage is exported. High frequency can coexist with narrow method choice. Daily
provision is a reported frequency, not a validated clinical-access standard.
At-least-weekly ratios are exported for comparison but are not selected components.

### Stockout input

Preserve cleaned `p_any_stockout` without recoding; verify it directly against the
original `uom_summary_facility.csv` export. All 2,370 eligible linked facilities
have numeric values. Numeric completeness does not establish complete raw replies.
The UoM product-group and unknown-response definitions are retained, including the
CDI source-selector limitations documented in the stock appendix. No upstream
methodology repair is silently made in this revision.

## Women, geography and missing profiles

Keep all 10,859 original SURGO records and their original variables; verify the
RDS/Stata round trip. Three records lack geography, consent and weight. Assign each
woman her own département-wide FP-provider profile without a rural/urban split.
Her original rural/urban residence remains a model covariate.

There are 82 surveyed geographic groups, 78 with facility data. Bonon, Facobly,
Gohitafla and Taabo remain unmatched: 153 geographically identified source women,
including 49 in need. Daoukro has 19 general FP-service providers, all reporting No to on-site
provision. Their free-product and daily-provision proportions are undefined;
its other available components can still be displayed. Product questions are
recorded as Not applicable rather than No, consistent with routing; the exact
questionnaire skip instruction has not been independently confirmed. These five
areas remain supply-unassessed, never assigned zero or a partial supply index.
There are 4,200 women in need with all six assigned supply summaries and 92 without.

Sections 2 onward use original SURGO **FPN2 = 1**, consent and positive WEIGHT:
4,292 women. Health/side-effect beliefs use **PB2A only** (1/2 concern, 3/4 no concern,
otherwise unresolved). The other five domains retain healthcare permission,
anticipated stigma, future-fertility concerns, low method confidence under the
retained knowledge label, and opposition to FP. No historical need reconstruction
or sterilization/hysterectomy additions are applied.

## Score construction and classifications

Heatmaps display raw stockout burden, higher = worse. Before any supply scoring,
ranking or PCA, replace burden with **continuity = 1 - burden**. All six supply
inputs then run higher = favorable. Standardize each component using the
unweighted mean and sample SD across the same complete reference départements
for its side. Demand has 82 reference areas; supply has 77. This is completeness
of the six geographic means, not a common complete-facility restriction.

Average six z-scores per side. Each final index has mean zero over its own stated
reference set. Demand > 0 means above-average concerns; readiness < 0 means
below-average supply. Model supply risk is negative readiness. This is equivalent
to averaging six risk-aligned standardized inputs. Values within 1e-10 of zero
are unflagged ties. Mean-based primary thresholds are zero; alternative rank,
mean-percentage and Median/IQR methods retain separate final-score medians.
Median count requires at least four unfavorable component comparisons.

Categories are relative planning signals, not validated service-adequacy standards
or causal binding constraints. Equal weighting is explicit; favorable components
can offset weaknesses. Correlations, extreme values and sampling/nonresponse
uncertainty affect interpretation. The OECD/JRC composite-indicator handbook
supports these construction methods, not this particular indicator list or cutoff.
Component heatmaps, comparison methods and response denominators remain visible.

Top-three selected components use standardized component ordering, not raw percentages or
regression effect sizes. The supply and demand recurrence/detailed tables include
all flagged areas on that side, including both-flagged areas. Those groups overlap.

## Models and verification

Five primary models use a common 3,951-woman sample across 77 départements, including
2,262 modern users. The only outcome is current modern-method use. Compare personal
demand score, assigned supply risk, both scores, six personal demand indicators,
and those indicators plus supply risk. Personal demand is not replaced by a
contextual geographic demand score.

Adjust every model for age, age squared, education, partnership, fertility goal
and rural/urban residence. Retain WEIGHT. Use département-clustered CR2 covariance,
Satterthwaite coefficient intervals, HTZ block tests and Holm correction within
each model's two demand/supply tests. Recorded IDZD sampling clusters support
overview intervals and an inference check; original strata/FPC are unavailable.
No random effects or interactions are fitted. Low effective degrees of freedom
and weight concentration are reported; convergence/VIF checks do not prove power.

The PCA appendix uses the same updated inputs and the same common regression
sample. PC1 remains an exploratory sensitivity, with mixed signs, variance,
permutation references and geographic-deletion checks reported. Complete-facility
and common-complete-demand sensitivity sections are not added back.

Independent checks reproduce eligibility, original raw fields, frequency categories,
all geographic assignments/means, source stockout values, aligned scores, zero
thresholds, classifications, modern outcomes, coefficients, CR2 covariance, joint
tests and weighted-SVD PCA. HTML checks cover order, French labels, embedded assets,
map completeness, aggregate downloads and artifact portability. These verify
implementation, not perfect source data or causal identification.
