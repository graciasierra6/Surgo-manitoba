# Case-specific free-provision decision

Only the Ibeju-Lekki Urban value of mean_p_free_prod is changed from missing to zero in the analysis copy of the joined dataset. No broader facility recoding is authorized or applied.

The Urban profile contains one facility, whose Q506 answer is No to providing FP services. Its original free-product and not-free-product counts are both zero, making the original proportion undefined (0/0). Zero is used as an explicit case-specific no-provision proxy so the area's women are not excluded solely because of this variable. This changes the interpretation for this one profile; it is not a newly observed product percentage or a correction to the original UoM source.

All women assigned to this Urban profile receive the same revised value, including women outside FPN2 = 1. The Rural profile and all other areas are unchanged. No other supply variables, pooled proportions, reporting denominators, SURGO values, weights, row order, geography or need classifications are modified. In particular, the original valid-response count remains zero because no additional facility answer was observed.

The original joined input is kept unchanged; the revised RDS/DTA are separate analysis copies. The one-facility basis remains an important coverage limitation. The exception does not demonstrate that free services are absent throughout Ibeju-Lekki. No Rural-to-Urban fallback is used.

Reproduce with apply_ibeju_free_provision_exception.R using --input for the original joined RDS and --output-dir for the analysis-copy folder. Both locations are supplied at runtime. The script rejects inputs where this cell is not missing, checks that only the selected values change, verifies all fields in saved RDS/DTA files, and writes a small change table, audit and decision note. It does not change either reference linkage module or historical HTML report.
