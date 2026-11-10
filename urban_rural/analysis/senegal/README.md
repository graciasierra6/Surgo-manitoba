# Senegal: updated Rural/Urban supply assignment

Generate `reports/Senegal_rural_urban_updated.html` with the current Senegal
definitions and exact residence matching where available:

```sh
Rscript urban_rural/analysis/senegal/Senegal_rural_urban_updated.R \
  --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

Or reuse a verified residence-specific join and extracted bundle:

```sh
Rscript urban_rural/analysis/senegal/Senegal_rural_urban_updated.R \
  --join-dir "$SURGO_MANITOBA_JOIN_DIR" \
  --cache-dir "$SURGO_MANITOBA_CACHE_DIR"
```

Optional runtime parameters are `--work-dir`, `--output-dir`, `--python` and
`--pandoc-dir`. The adjacent Rmd calls the same pipeline. External inputs are
caller-supplied; intermediate women files, facility inputs and fitted models stay
in ignored output directories. Upload source and rendered reports, not input or
woman-level datasets. This module uses the existing administrative-area Senegal
preparation, planning and map helpers plus shared project survey definitions.

## Assignment and fallback

Retain all 7,169 original SURGO records and survey fields. From Section 2 onward,
use original SURGO `FPN2 = 1` with consent and positive `WEIGHT`: 1,905 women.
No historical `need_n` additions or new need reconstruction are applied.

Require raw facility `s5_6 = 1` before aggregation. Average valid facility values
equally within each département–Rural/Urban cell, including genuine zero/FALSE and
excluding unknown or undefined values separately by measure. No facility weights.
Each woman receives the summary of facilities matching her département and her
recorded `FV_MILIEU` residence (1 Rural, 2 Urban).

When a matching residence cell has **zero confirmed FP providers**, use the
same département's FP-provider averages and explicitly flag the broader
resolution. This applies to empty cells in either residence category; in the
current Senegal snapshot all 16 affected cells are Rural. Do not fill individual
undefined components when the exact cell does have eligible facilities.

There are 49 observed département–residence groups across 33 départements:
33 exact profiles and 16 flagged département-wide fallbacks. All 49 assignments
have six defined supply components. Fallback affects 1,815 source women, including
522 in need. Some Urban and fallback groups receive identical numeric values;
49 assignment groups therefore do not imply 49 different scores or independent
contexts. The verified model sample has 39 distinct numeric supply scores across
33 départements. Models retain département clustering.

**Residence limitation:** facility residence is taken from
`SenegalURlookup.xlsx`, matched by raw health district `s1_3`. It is a
health-district proxy, not a facility-coordinate or site-level Rural/Urban
measurement. Nine facility records have unknown proxy residence and remain
unknown. These records can contribute to département-wide means but cannot be
assigned to an exact Rural/Urban facility cell. An empty rural match does not
demonstrate an absence of real-world rural FP services. The fallback represents
broader recorded provision, not directly observed service conditions for that
woman's residence category. The report lists all 16 fallback groups and coverage.

The checked health-district-to-département crosswalk is unchanged. Existing
qualifications for Diakhao, Yeumbeul and Sangalkam remain documented. Original
woman residence and geography are not changed by fallback. The working join has
requested and assigned profile keys, an exact-match flag, a fallback flag and an
assignment-resolution label.

## Measures and two geographic views

The six supply measures are short/long-acting offering (`one_larc_3_sa_offer`),
free-product proportion (`p_free_prod`), trained personnel proportion (`p_train`),
daily provision of offered modern-method classes (raw `s5_7a–g`), at least one
LARC in stock (`one_larc_stockin`), and original stockout burden
(`p_any_stockout`). Daily provision is Daily classes / recorded offered classes
(codes 1–4); code 5 Not at all is excluded. No offered denominator is undefined.
Original cleaned stockout coding is retained. Higher burden is worse and becomes
continuity = 1 − burden before every supply score/PCA. Counseling and popular
stock are not selected components.

Demand retains six personal domains: HS7C healthcare permission; PB7B/C anticipated
stigma; **PB2A only** health beliefs; coherent MP4 reversible-method fertility
concerns; MC5A/B confidence under the retained knowledge label; and FP1 opposition.
Unknown responses are not automatically absence of concern. MC5 is confidence,
not an objective knowledge test.

- **Planning heatmaps, classifications and weakest-component tables** pool all
  confirmed FP providers within each département. They retain today's aggregate
  results. Do not take an unweighted average of Rural/Urban means to reconstruct
  a département mean.
- **Woman-level models and PCA** use each woman's exact or flagged fallback supply
  assignment. They retain her own individual demand responses and personal score.

Planning z-scores use equal-département reference means and sample SDs across the
33 linked départements. Average six components on each side; flag demand > 0 and
favorable supply readiness < 0, with 1e-10 tie tolerance. These are relative
signals, not causal diagnoses or absolute adequacy thresholds.

Personal demand scores standardize each woman's six responses using item-specific
final-weighted means and population SDs among valid women in need, then average
all six. Supply risk is the negative average of her assigned favorable component
z-scores using the fixed 33-département planning references. This preserves a
common scale with the aggregate report. Assigned residence scores are not
recentered and need not average zero. PCA uses separate final-weighted correlation
analyses of personal demand and assigned supply; mixed signs remain disclosed.

## Models and checks

All five main and five PCA sensitivity models use current modern contraceptive
use, original `WEIGHT`, age, age squared, Senegal-specific education, partnership,
fertility goal and Rural/Urban. The five specifications enter demand score;
supply risk; both scores; six personal demand indicators; and six indicators plus
supply risk. They share one complete model sample. No random effects/interactions.
In the verified snapshot that sample contains 1,558 women, including 795 modern
users and 433 women with flagged fallback assignments. All 522 women in need who
require fallback receive supply values; the other 89 have incomplete demand or
adjustment information.

Available survey summaries use recorded `RB0H` census districts and `WEIGHT`,
building the full consenting positive-weight design before need-group selection.
Original strata and FPC are unavailable. Primary model uncertainty uses
département-clustered CR2 covariance, Satterthwaite intervals and HTZ block tests;
Holm adjusts the pair of block tests within each joint model. Recorded-PSU
inference is exported as a sensitivity comparison. Scores are estimated inputs;
their construction uncertainty is not propagated into the regression intervals.

Independent checks reproduce raw survey fields, eligibility, the facility
residence lookup, zero-provider fallback triggers, all assigned means, planning
scores/cutoffs, personal scores, model coefficients/covariance and PCA scores.
The HTML is checked for ordering, two maps, ten embedded figures, table counts,
valid links and portable source/assets. Diagnostics do not establish power,
causality, which side is binding, or an optimal allocation of funding. Existing
aggregate and earlier residence-specific reports remain unchanged.

## Append fallback coverage to Data checks

Preserve an existing data-check document and append or replace the Senegal
residence-fallback section with:

```sh
Rscript urban_rural/analysis/senegal/update_data_checks.R \
  --base-html "$SURGO_MANITOBA_DATA_CHECKS_HTML" \
  --data-dir "$SURGO_MANITOBA_SENEGAL_ANALYSIS_DIR" \
  --output reports/Data_checks.html
```

The analysis directory is the working `Senegal_rural_urban_updated_files` output
from this module. The script reports all-women and need-group Rural fallback
counts, sample percentages and final-weighted percentages, plus the Urban check
for both populations. It uses full source/need denominators rather than model
complete cases. An optional `--audit-json` saves aggregate calculation checks.
