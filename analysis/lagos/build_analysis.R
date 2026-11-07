# Data for the standalone Lagos analysis: supplied FPN2, LGA planning summaries.
# All locations are runtime parameters. No source data or historical report changes.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k, args); if (is.na(i) || i == length(args)) stop("Supply ", k); args[i + 1L] }
optional <- function(k) { i <- match(k, args); if (is.na(i)) return(NULL); if (i == length(args)) stop("Supply a value for ", k); args[i + 1L] }
input <- opt("--input"); facility_input <- opt("--facility-input"); out <- opt("--output-dir")
psu_var <- optional("--psu-var"); strata_var <- optional("--strata-var")
if (!is.null(strata_var) && is.null(psu_var)) stop("--strata-var requires a confirmed --psu-var")
for (pkg in c("survey", "jsonlite")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
hash_before <- tools::md5sum(c(input, facility_input))
x <- readRDS(input); fac <- readRDS(facility_input); fac <- fac[fac$country %in% "lagos", ]
nv <- function(k) as.numeric(x[[k]])
key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
wm <- function(v, w) { ok <- is.finite(v) & is.finite(w) & w > 0; if (any(ok)) weighted.mean(v[ok], w[ok]) else NA_real_ }
mean_valid <- function(v) { v <- as.numeric(v); if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_ }
bool <- function(v) { a <- tolower(as.character(v)); ifelse(a %in% c("true", "1", "yes"), 1, ifelse(a %in% c("false", "0", "no"), 0, NA_real_)) }
flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
any_concern <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
D <- c("agency", "stigma", "health", "fertility", "confidence", "opposes")
DL <- c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs", "Future-fertility concerns", "Low method knowledge", "Opposes family planning")
S <- c("offering", "free", "training", "counselor", "larc_stock", "popular_stock")
SL <- c("Short + long-acting offering", "Free-product provision", "Trained-provider share", "FP counselor availability", "Three-LARC stock breadth", "Most-popular product in stock")
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- dirname(gsub("~+~", " ", sub("^--file=", "", script_arg[1]), fixed=TRUE))
source(file.path(script_dir, "lagos_definitions.R"))
d <- lagos_derive_women(x)
valid_weight <- is.finite(d$weight) & d$weight > 0
stopifnot(all(valid_weight[d$need]), sum(d$need) == 2159)
options(survey.lonely.psu = "adjust")
# Supplied weights identify point estimates. Do not infer official PSUs or strata
# from household-ID digits or probability values. Intervals are enabled only when
# confirmed design fields are explicitly supplied at run time.
design_confirmed <- !is.null(psu_var)
if (design_confirmed) {
  stopifnot(psu_var %in% names(x))
  d$.psu <- as.character(x[[psu_var]])
  stopifnot(all(!is.na(d$.psu[valid_weight]) & nzchar(d$.psu[valid_weight])))
  strata_formula <- NULL
  if (!is.null(strata_var)) {
    stopifnot(strata_var %in% names(x))
    d$.stratum <- as.character(x[[strata_var]])
    stopifnot(all(!is.na(d$.stratum[valid_weight]) & nzchar(d$.stratum[valid_weight])))
    strata_formula <- ~.stratum
  }
  full <- survey::svydesign(ids = ~.psu, strata = strata_formula, weights = ~weight,
                            data = d[valid_weight, ], nest = TRUE)
} else {
  full <- survey::svydesign(ids = ~1, weights = ~weight, data = d[valid_weight, ])
}
need_design <- subset(full, need)
estimate <- function(des, field) {
  v <- des$variables[[field]]
  p <- weighted.mean(as.numeric(v), des$variables$weight)
  ci <- c(NA_real_, NA_real_)
  if (design_confirmed && p > 0 && p < 1 && survey::degf(des) > 0) {
    des$variables$.event <- as.numeric(v)
    ci <- as.numeric(confint(survey::svyciprop(~.event, des, method = "logit", df = survey::degf(des))))
  }
  list(n = sum(v), denominator = length(v), estimate = p, lo = ci[1], hi = ci[2])
}
bar_defs <- data.frame(field = c("user", "wanted_start", "other_start", "modern", "modern_other_start", "traditional", "traditional_modern_switch", "intender", "no_intent", "unresolved"),
  label = c("Current contraceptive users", "Using the method wanted at start*", "Not using the method wanted at start*", "Current modern-method users", "Wanted another method at start*", "Traditional methods (subset of users)", "Wants to switch to a modern method", "Nonusers with intention", "Nonusers without stated intention", "Use / intention unresolved"),
  indent = c(0,1,1,0,1,0,1,0,0,0), y = c(0,1,2,3.8,4.8,6.6,7.6,9.4,11,12.6),
  color = c("#0a7d89", "#579b90", "#ba8743", "#307e60", "#ba8743", "#8c8981", "#b57a32", "#446da4", "#89639a", "#9caab0"), stringsAsFactors = FALSE)
bars <- lapply(seq_len(nrow(bar_defs)), function(i) c(as.list(bar_defs[i, ]), estimate(need_design, bar_defs$field[i])))
if (tail(bars, 1)[[1]]$n == 0) bars <- head(bars, -1)
dn <- d[d$need, ]; areas <- sort(unique(dn$geo))
rates <- lapply(areas, function(g) {
  a <- subset(need_design, geo == g)
  list(geo = g, n = nrow(a$variables), cpr = estimate(a, "user"), mcpr = estimate(a, "modern"),
       intender = estimate(a, "intender"), no_intent = estimate(a, "no_intent"))
})
fac$geo <- ifelse(!is.na(fac$geo_join_pipeline), key(fac$geo_join_pipeline), key(fac$source_geo_name))
stopifnot(all(!is.na(fac$geo)), length(unique(fac$geo)) == 20)
# Reconcile joined zero exception at exactly one original facility, then combine
# all Rural/Urban facility records within LGA. No other facility value changes.
fac$free_original <- as.numeric(fac$p_free_prod)
fac$free_analysis <- fac$free_original
case <- fac$geo %in% "IBEJULEKKI" & as.character(fac$rurb) %in% "Urban"
joined_case <- d$geo %in% "IBEJULEKKI" & as.character(x$link_rurb) %in% "Urban"
stopifnot(sum(case) == 1, all(is.na(fac$free_original[case])), all(nv("mean_p_free_prod")[joined_case] == 0))
fac$free_analysis[case] <- 0
fac$offering <- bool(fac$one_larc_3_sa_offer)
fac$training <- as.numeric(fac$p_train)
fac$counselor <- as.numeric(fac$counselor_available)
fac$larc_stock <- bool(fac$three_larc_stockin)
fac$popular_stock <- bool(fac$mp_in_stock)
fac$free <- fac$free_analysis
cells <- list(); meta <- list()
display_areas <- sort(unique(fac$geo))
for (g in c("LAGOS", display_areas)) {
  wi <- if (g == "LAGOS") rep(TRUE, nrow(dn)) else dn$geo %in% g
  fi <- if (g == "LAGOS") fac$geo %in% areas else fac$geo %in% g
  meta[[g]] <- list(geo = g, n_need = sum(wi), facility_n = sum(fi), linked = g == "LAGOS" || g %in% areas)
  for (j in seq_along(D)) {
    k <- D[j]; ok <- wi & is.finite(dn[[k]])
    cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Demand", variable = k, label = DL[j],
      value = if (any(ok)) wm(dn[[k]][ok], dn$weight[ok]) * 100 else NA_real_,
      n_valid = sum(ok), n_total = sum(wi), numerator = sum(dn[[k]][ok] == 1))
  }
  for (j in seq_along(S)) {
    k <- S[j]; ok <- fi & is.finite(fac[[k]])
    cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Supply", variable = k, label = SL[j],
      value = mean_valid(fac[[k]][fi]) * 100, n_valid = sum(ok), n_total = sum(fi), numerator = sum(fac[[k]][ok]))
  }
}
cells <- do.call(rbind, cells)
savecsv <- function(z, name, rows = FALSE) write.csv(z, file.path(out, name), row.names = rows, na = "")
savecsv(cells, "heatmap_values_and_denominators.csv")
M <- list()
for (side in c("Demand", "Supply")) {
  cols <- if (side == "Demand") D else S
  m <- sapply(cols, function(k) {
    z <- cells[cells$side == side & cells$variable == k, ]
    z$value[match(areas, z$geo)]
  })
  rownames(m) <- areas; stopifnot(all(is.finite(m)), all(apply(m, 2, sd) > 0), all(apply(m, 2, IQR) > 0))
  M[[side]] <- list(raw = m, rank = apply(m, 2, rank, ties.method = "average"),
    robust = sweep(sweep(m, 2, apply(m, 2, median), "-"), 2, apply(m, 2, IQR), "/"),
    zscore = sweep(sweep(m, 2, colMeans(m), "-"), 2, apply(m, 2, sd), "/"))
}
methods <- c(rank = "Rank average", raw = "Mean percentages", robust = "Median/IQR", median_count = "Median count", zscore = "Z-score average")
scores <- list(); thresholds <- list(); transforms <- list()
for (id in c("rank", "raw", "robust", "zscore")) {
  ds <- rowMeans(M$Demand[[id]]); ss <- rowMeans(M$Supply[[id]])
  dc <- median(ds); sc <- median(ss); df <- ds > dc + 1e-10; sf <- ss < sc - 1e-10
  scores[[id]] <- data.frame(method = id, geo = areas, demand_score = ds, supply_score = ss,
    demand_priority_rank = rank(-ds, ties.method = "average"), supply_priority_rank = rank(ss, ties.method = "average"),
    demand_flag = df, supply_flag = sf, category = ifelse(df & sf, "both", ifelse(df, "demand", ifelse(sf, "supply", "neither"))))
  thresholds[[id]] <- data.frame(method = id, label = methods[[id]], demand_cutoff = dc, supply_cutoff = sc,
    demand_rule = "strictly above final-score median", supply_rule = "strictly below final-readiness-score median")
}
dc <- rowSums(sweep(M$Demand$raw, 2, apply(M$Demand$raw, 2, median), "-") > 1e-10)
sc <- rowSums(sweep(M$Supply$raw, 2, apply(M$Supply$raw, 2, median), "-") < -1e-10)
df <- dc >= 4; sf <- sc >= 4
scores$median_count <- data.frame(method = "median_count", geo = areas, demand_score = dc, supply_score = sc,
  demand_priority_rank = rank(-dc, ties.method = "average"), supply_priority_rank = rank(-sc, ties.method = "average"),
  demand_flag = df, supply_flag = sf, category = ifelse(df & sf, "both", ifelse(df, "demand", ifelse(sf, "supply", "neither"))))
thresholds$median_count <- data.frame(method = "median_count", label = methods[["median_count"]], demand_cutoff = 4, supply_cutoff = 4,
  demand_rule = "at least 4 of 6 above component medians", supply_rule = "at least 4 of 6 below component medians")
scores <- do.call(rbind, scores); thresholds <- do.call(rbind, thresholds)
for (side in names(M)) for (k in colnames(M[[side]]$raw)) {
  m <- M[[side]]$raw
  transforms[[length(transforms) + 1L]] <- data.frame(geo = areas, side = side, variable = k,
    original_pct = m[, k], indicator_mean = mean(m[, k]), indicator_SD = sd(m[, k]),
    indicator_median = median(m[, k]), indicator_IQR = IQR(m[, k]),
    component_rank = M[[side]]$rank[, k], zscore = M[[side]]$zscore[, k], robust = M[[side]]$robust[, k])
}
trace <- do.call(rbind, transforms)
exact <- cells[cells$geo %in% areas, c("geo", "side", "variable", "value")]
exact$original_pct_exact <- sprintf("%.17g", exact$value)
savecsv(exact[c("geo", "side", "variable", "original_pct_exact")], "scoring_inputs_exact.csv")
savecsv(scores, "all_method_scores_and_classifications.csv"); savecsv(thresholds, "classification_thresholds.csv")
savecsv(trace, "component_score_trace.csv")
planning <- lapply(areas, function(g) {
  q <- scores[scores$method == "zscore" & scores$geo == g, ]
  dt <- trace[trace$geo == g & trace$side == "Demand", ]; st <- trace[trace$geo == g & trace$side == "Supply", ]
  dt <- dt[order(-dt$zscore, match(dt$variable, D)), ]; st <- st[order(st$zscore, match(st$variable, S)), ]
  list(geo = g, category = q$category, demand_flag = q$demand_flag, supply_flag = q$supply_flag,
       demand_score = q$demand_score, supply_score = q$supply_score, n = meta[[g]]$n_need,
       top_demand = head(dt$variable, 3), top_supply = head(st$variable, 3))
})
coverage <- lapply(display_areas, function(g) list(geo = g, women = sum(d$geo %in% g), facilities = sum(fac$geo %in% g)))
bar_table <- do.call(rbind, lapply(bars, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
savecsv(bar_table, "use_distribution_fpn2.csv")
lga_rates <- do.call(rbind, lapply(rates, function(r) data.frame(geo = r$geo, n_need = r$n,
  cpr = r$cpr$estimate, mcpr = r$mcpr$estimate, intender = r$intender$estimate, no_intent = r$no_intent$estimate)))
savecsv(lga_rates, "lga_use_rates_fpn2.csv")
audit <- list(source_rows = nrow(x), positive_weight_rows = sum(valid_weight), need_n = nrow(dn),
  need_eligibility = "Supplied SURGO fpn2 == 1, unchanged; no additional fertility/sexual-activity or sterilization filter.",
  women_weight = "weight", weighted_need_share = wm(as.numeric(d$need[valid_weight]), d$weight[valid_weight]),
  linked_lgas = length(areas), supply_lgas = length(display_areas), all_facility_records = nrow(fac),
  linked_lga_facility_records = sum(fac$geo %in% areas),
  supply_aggregation = "All included facility records within LGA, combining Rural and Urban; valid-response means; no facility weights. Not a woman-weighted mean of attached residence profiles.",
  free_exception = "Only the original Ibeju-Lekki Urban facility's undefined free proportion is set to zero, consistent with the authorized joined-data exception; its 45 Rural facilities retain original values. The LGA mean includes this one zero with other valid facility proportions.",
  popular_stock = "Mean of cleaned UoM mp_in_stock over TRUE/FALSE responses, equivalent to true/(true+false); unknowns excluded and coverage retained. This is the facility's most-popular product, not a woman's preferred method.",
  chart_denominator = "Every bar and sub-bar uses all 2159 SURGO-in-need women, with final weights. n is unweighted; overlapping subsets do not sum to 100. Preference concerns initiation, not necessarily present preference.",
  use_definition = "CU1 yes/sometimes or CU5 current-use prompts. Modern: CU4 1-8, prompted condom/ECP, or explicitly contraceptive LAM. Traditional-only: confirmed traditional practices without a confirmed modern method. Unresolved outcomes retained separately.",
  design_fields_confirmed = design_confirmed,
  survey_variance = if (design_confirmed) paste("Taylor-linearized logit intervals using the explicitly supplied confirmed PSU field", psu_var, "and stratum field", if (is.null(strata_var)) "none" else strata_var, "; no FPC. Need subgroup selected after creating the full positive-weight design.") else "Final-weight point estimates only. Official EA/PSU and stratum identifiers remain unconfirmed, so design-based confidence intervals and tests are withheld. No clusters are inferred from household IDs or sampling probabilities.",
  four_category_labels = "Relative concern/service flags; not causal identification of binding constraints or absolute service-adequacy thresholds.",
  scoring_reference = "17 linked LGAs, equal LGA contribution to normalization/cutoffs. Six equally weighted components per side; all supply components favorable. Sample SD uses n-1; IQR uses R type 7. Strict cutoffs with 1e-10 tie tolerance.",
  main_method = "zscore", sources = list(joined_file = basename(input), facility_file = basename(facility_input)),
  source_md5 = as.list(unname(hash_before)), source_files_unchanged = identical(unname(hash_before), unname(tools::md5sum(c(input, facility_input)))))
stopifnot(audit$source_files_unchanged, sum(vapply(bars, function(b) if (b$field %in% c("user", "intender", "no_intent", "unresolved")) b$n else 0, numeric(1))) == nrow(dn))
payload <- list(audit = audit, demand_variables = D, demand_labels = DL, supply_variables = S, supply_labels = SL,
  bars = bars, rates = rates, heatmap = cells, area_metadata = unname(meta), scores = scores, thresholds = thresholds,
  component_trace = trace, planning = planning, coverage = coverage)
jsonlite::write_json(payload, file.path(out, "report_data.json"), auto_unbox = TRUE, dataframe = "rows", pretty = FALSE, na = "null", digits = 12)
jsonlite::write_json(audit, file.path(out, "analysis_audit.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("Built data:", nrow(dn), "women in need;", length(areas), "linked LGAs;", nrow(fac), "facility records.\n")
print(bar_table[, c("field", "n", "estimate")], row.names = FALSE)
print(thresholds, row.names = FALSE)
print(table(scores$method, scores$category))
