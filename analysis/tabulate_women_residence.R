# Tabulate source women by administrative area and residence, optionally selecting
# a caller-specified survey category without recoding the source definition.
# Counts retain every source row; percentages require a positive final weight and,
# when supplied, the requested consent code. No original inputs are modified.
args <- commandArgs(trailingOnly = TRUE)
arg <- function(k, default = NULL) {
  i <- match(k, args)
  if (is.na(i)) { if (is.null(default)) stop("Supply ", k); return(default) }
  if (i == length(args)) stop("Supply ", k)
  args[i + 1L]
}
if ("--help" %in% args) {
  cat("Rscript analysis/tabulate_women_residence.R --input JOIN --weight-var FIELD --output-file TABLE [--geo-var link_geo_join] [--residence-var link_rurb] [--consent-var FIELD] [--consent-code 1] [--subset-var FIELD] [--subset-code 1]\n")
  quit(status = 0)
}
input <- arg("--input"); output <- arg("--output-file")
geo_var <- arg("--geo-var", "link_geo_join")
residence_var <- arg("--residence-var", "link_rurb")
weight_var <- arg("--weight-var"); consent_var <- arg("--consent-var", "")
consent_code <- as.numeric(arg("--consent-code", "1"))
subset_var <- arg("--subset-var", "")
subset_code <- as.numeric(arg("--subset-code", "1"))
if (!requireNamespace("haven", quietly = TRUE)) stop("Install haven")
x <- switch(tolower(tools::file_ext(input)), rds = readRDS(input),
  dta = haven::read_dta(input), csv = read.csv(input, check.names = FALSE),
  stop("Use an RDS, DTA or CSV input"))
fields <- c(geo_var, residence_var, weight_var, if (nzchar(consent_var)) consent_var,
            if (nzchar(subset_var)) subset_var)
stopifnot(all(fields %in% names(x)))
if (nzchar(subset_var)) x <- x[as.numeric(haven::zap_labels(x[[subset_var]])) %in% subset_code, , drop = FALSE]
if (!nrow(x)) stop("No source rows meet the requested population definition")
geo <- as.character(x[[geo_var]]); residence <- as.character(x[[residence_var]])
weight <- as.numeric(haven::zap_labels(x[[weight_var]]))
stopifnot(!anyNA(geo), all(nzchar(geo)), !anyNA(residence), all(residence %in% c("Rural", "Urban")))
eligible <- is.finite(weight) & weight > 0
if (nzchar(consent_var)) eligible <- eligible & as.numeric(haven::zap_labels(x[[consent_var]])) %in% consent_code
percentage <- function(selected, denominator) {
  denominator <- denominator & eligible
  if (!any(denominator)) return(NA_real_)
  100 * sum(weight[selected & denominator]) / sum(weight[denominator])
}
tabulate <- function(selected, name) {
  rural <- selected & residence == "Rural"; urban <- selected & residence == "Urban"
  data.frame(area = name, rural_n = sum(rural), rural_weighted_column_pct = percentage(rural, residence == "Rural"),
    urban_n = sum(urban), urban_weighted_column_pct = percentage(urban, residence == "Urban"),
    total_n = sum(selected), total_weighted_column_pct = percentage(selected, rep(TRUE, nrow(x))),
    rural_weighted_within_area_pct = percentage(rural, selected),
    urban_weighted_within_area_pct = percentage(urban, selected),
    valid_weighted_n = sum(selected & eligible), excluded_from_weighted_n = sum(selected & !eligible),
    stringsAsFactors = FALSE)
}
areas <- sort(unique(geo))
table <- do.call(rbind, lapply(areas, function(g) tabulate(geo == g, g)))
stopifnot(sum(table$total_n) == nrow(x), sum(table$rural_n) + sum(table$urban_n) == nrow(x),
  all(table$rural_n + table$urban_n == table$total_n),
  abs(sum(table$total_weighted_column_pct) - 100) < 1e-8)
for (k in c("rural", "urban")) if (any(residence == tools::toTitleCase(k) & eligible))
  stopifnot(abs(sum(table[[paste0(k, "_weighted_column_pct")]]) - 100) < 1e-8)
table <- rbind(table, tabulate(rep(TRUE, nrow(x)), "Total"))
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.csv(table, output, row.names = FALSE, na = "")
print(table[c("area", "rural_n", "rural_weighted_column_pct", "urban_n", "urban_weighted_column_pct", "total_n", "total_weighted_column_pct")], row.names = FALSE, digits = 5)
cat("Counts:", nrow(x), "selected source rows. Weighted percentages:", sum(eligible), "eligible rows;", sum(!eligible), "excluded.",
    if (nzchar(subset_var)) paste0(" Population: ", subset_var, " = ", subset_code, ".") else " No population restriction.", "\n")
