# Data coverage checks

[Open the four-geography coverage report](Surgo_UoM_checks.html).

The report compares SURGO women with Manitoba FP-provider coverage in Côte
d’Ivoire, Lagos, Senegal and Sindh. It includes all-women and women-in-need
tables, exact geographic/residence matches, flagged whole-area fallbacks,
unmatched groups and the reviewed geographic-assignment differences. Sindh
uses district linkage because the supplied women's file has no Rural/Urban
identifier.

The HTML is self-contained and opens offline. It contains aggregate tables,
not respondent or facility microdata. Counts are unweighted; women’s percentage
estimates use their survey weights. Facility counts are unweighted. A geographic
match does not guarantee a valid value for every facility indicator.

## Rebuild

The parameterized source is
[Surgo_UoM_checks.R](../analysis/Surgo_UoM_checks.R), with
[R Markdown](../analysis/Surgo_UoM_checks.Rmd) and the
[residence tabulation helper](../analysis/tabulate_women_residence.R).
It reads the existing analysis-prepared joins, the crosswalked facility file,
raw facility eligibility/residence fields, the Senegal residence lookup and the
authorized Sindh need file. These are generated from the externally supplied
input bundle and remain outside the repository.

Run `Rscript analysis/Surgo_UoM_checks.R --help` from the repository root for
the input arguments. Every external input must be supplied through arguments
or the documented environment variables. The default report destination is
`data_checks`; `--output-dir` can override it. See the
[analysis guide](../analysis/README.md#cumulative-coverage-and-completeness-tables)
for definitions and limitations.

## Composite score comparisons

[Composite score comparisons](Composite%20score_comparisons.html) contains five
demand and eight supply comparisons. It shows how similar composite scores can
conceal different component profiles. Its sources are
`analysis/plot_demand_pair_table.R` and `analysis/plot_supply_pair_table.R`, using
aggregate profiles generated from the current country results.

Both reports are included in the complete input-to-report command:

```sh
Rscript R/rebuild_all.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

See [reproduction instructions](../REPRODUCIBILITY.md) for the full inventory and
validation procedure. The separate cumulative `reports/Data_checks.html` is also
recalculated by that command.
