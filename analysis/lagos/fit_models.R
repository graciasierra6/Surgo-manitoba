# Modern-use associations with the report's LGA z-scores and individual concerns.
# Runtime paths only; original joins and the historical summary are not modified.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k, args); if (is.na(i) || i == length(args)) stop("Supply ", k); args[i + 1L] }
optional <- function(k, default) { i <- match(k, args); if (is.na(i)) return(default); if (i == length(args)) stop("Supply ", k); args[i + 1L] }
input <- opt("--input"); data_dir <- opt("--data-dir"); out <- opt("--output-dir")
resolution <- optional("--supply-resolution", "urban_rural")
demand_resolution <- optional("--demand-resolution", "individual")
score_method <- optional("--score-method", "zscore")
pca_score_file <- optional("--pca-score-file", "")
missing_popular <- optional("--missing-popular", "error")
if ("--outcome" %in% args) stop("This analysis models modern contraceptive use only; omit --outcome.")
stopifnot(resolution %in% c("lga", "urban_rural"), demand_resolution %in% c("individual", "lga"), score_method %in% c("zscore", "pca"), missing_popular %in% c("error", "lga_one_component"))
if (score_method == "pca" && !nzchar(pca_score_file)) stop("Supply --pca-score-file for the PCA sensitivity")
for (pkg in c("survey", "clubSandwich", "jsonlite", "ggplot2", "svglite"))
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
script_dir <- dirname(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "lagos_definitions.R"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
source_hash <- unname(tools::md5sum(input))
report_path <- file.path(data_dir, "report_data.json")
report_hash <- unname(tools::md5sum(report_path))
p <- jsonlite::fromJSON(report_path)
stopifnot(source_hash == p$audit$source_md5[1])
x <- readRDS(input); d <- lagos_derive_women(x)
D <- p$demand_variables
z <- p$scores[p$scores$method == "zscore", ]
stopifnot(!anyDuplicated(z$geo), nrow(z) == 17)
d$demand_score_lga <- z$demand_score[match(d$geo, z$geo)]
d$demand_score <- d$demand_score_lga
demand_reference <- list()
if (demand_resolution == "individual" && score_method == "zscore") {
  demand_z <- sapply(D, function(k) {
    ok <- d$need & is.finite(d[[k]]) & is.finite(d$weight) & d$weight > 0
    mu <- weighted.mean(d[[k]][ok], d$weight[ok])
    sd <- sqrt(weighted.mean((d[[k]][ok] - mu)^2, d$weight[ok]))
    stopifnot(sd > 0)
    demand_reference[[k]] <<- data.frame(variable = k, weighted_mean = mu, weighted_SD = sd,
      n_valid_need = sum(ok), n_need = sum(d$need))
    (d[[k]] - mu) / sd
  })
  # A personal score requires all six observed responses, with no missing=zero.
  d$demand_score <- rowMeans(demand_z)
  write.csv(do.call(rbind, demand_reference), file.path(out, "individual_demand_score_reference.csv"), row.names = FALSE)
}
d$supply_readiness_lga <- z$supply_score[match(d$geo, z$geo)]
d$supply_readiness <- d$supply_readiness_lga
d$supply_component_fallback <- FALSE
supply_map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
  counselor = "share_fp_counselor", larc_stock = "share_stocked_3plus_larc")
observed_supply <- as.data.frame(lapply(supply_map, function(k) as.numeric(x[[k]])))
yes <- as.numeric(x$n_mp_in_stock_true); no <- as.numeric(x$n_mp_in_stock_false)
observed_supply$popular_stock <- ifelse(yes + no > 0, yes / (yes + no), NA_real_)
analysis_supply <- observed_supply
if (resolution == "urban_rural") {
  unknown <- !is.finite(analysis_supply$popular_stock)
  if (any(unknown)) {
    if (missing_popular == "error") stop("Missing most-popular-product stock status. Resolve explicitly with --missing-popular; no automatic imputation.")
    # Explicitly approved, component-specific fallback; no raw value is recoded.
    stopifnot(all(d$geo[unknown] == "IBEJULEKKI" & d$residence[unknown] == "Urban"))
    hm <- p$heatmap[p$heatmap$side == "Supply" & p$heatmap$variable == "popular_stock", ]
    analysis_supply$popular_stock[unknown] <- hm$value[match(d$geo[unknown], hm$geo)] / 100
    d$supply_component_fallback[unknown] <- TRUE
  }
  standardized <- sapply(p$supply_variables, function(k) {
    ref <- p$component_trace[p$component_trace$side == "Supply" & p$component_trace$variable == k, ]
    mu <- unique(ref$indicator_mean); sd <- unique(ref$indicator_SD)
    stopifnot(length(mu) == 1, length(sd) == 1, sd > 0)
    (100 * analysis_supply[[k]] - mu) / sd
  })
  d$supply_readiness <- rowMeans(standardized)
}
d$supply_risk <- -d$supply_readiness
if (score_method == "pca") {
  pca_input <- readRDS(pca_score_file)
  stopifnot(identical(pca_input$scores$row, d$row), pca_input$audit$input_md5 == source_hash,
    pca_input$audit$report_md5 == report_hash, resolution == "urban_rural", demand_resolution == "individual",
    identical(as.logical(pca_input$scores$fallback), d$supply_component_fallback))
  d$demand_score <- pca_input$scores$demand_score
  d$supply_risk <- pca_input$scores$supply_risk
  d$supply_readiness <- -d$supply_risk
}
stopifnot(all(is.finite(d$demand_score[d$need & complete.cases(d[D])])), all(is.finite(d$supply_risk[d$need])), sum(d$need) == 2159)
profile_key <- paste(d$geo, d$residence, sep = "::")
profiles <- data.frame(geo = d$geo, residence = d$residence, observed_supply,
  popular_stock_used = analysis_supply$popular_stock, fallback = d$supply_component_fallback,
  demand_score_lga = d$demand_score_lga, supply_readiness_lga = d$supply_readiness_lga,
  supply_risk = d$supply_risk, n_facilities = as.numeric(x$n_facilities))
stopifnot(!anyDuplicated(unique(data.frame(profile_key, profiles))$profile_key))
profiles <- profiles[!duplicated(profile_key), ]
profiles$n_women <- as.integer(table(factor(profile_key, levels = paste(profiles$geo, profiles$residence, sep = "::"))))
profiles$n_need <- as.integer(table(factor(profile_key[d$need], levels = paste(profiles$geo, profiles$residence, sep = "::"))))
write.csv(profiles, file.path(out, "model_supply_profiles.csv"), row.names = FALSE)
# Verify the report's weighted demand inputs against the same individual records.
max_prevalence_error <- 0
for (g in z$geo) for (k in D) {
  a <- d[d$need & d$geo == g & is.finite(d[[k]]), ]
  q <- p$heatmap$value[p$heatmap$geo == g & p$heatmap$side == "Demand" & p$heatmap$variable == k]
  max_prevalence_error <- max(max_prevalence_error, abs(100 * weighted.mean(a[[k]], a$weight) - q))
}
stopifnot(max_prevalence_error < 1e-8)
controls <- c("age10", "age10_sq", "education", "partnered", "fertility_goal", "urban")
specs <- list(score_demand = c(controls, "demand_score"),
              supply = c(controls, "supply_risk"),
              score_joint = c(controls, "demand_score", "supply_risk"),
              components_demand = c(controls, D),
              components_joint = c(controls, D, "supply_risk"))
positive <- is.finite(d$weight) & d$weight > 0
common_vars <- unique(c("y_modern", controls, D, "demand_score", "supply_risk"))
d$eligible <- d$need & positive & complete.cases(d[common_vars])
cc <- d[d$eligible, ]
stopifnot(length(unique(cc$geo)) == 17, all(cc$y_modern %in% 0:1))
# LGA is a known geographic cluster, not an asserted official survey PSU.
# No official EA/stratum identifiers or finite-population correction are inferred.
full_design <- survey::svydesign(ids = ~geo, weights = ~weight, data = d[positive, ], nest = TRUE)
des <- subset(full_design, eligible)
fit_glm <- function(fo, a) {
  a$.fit_wt <- a$weight / mean(a$weight)
  warnings <- character()
  f <- withCallingHandlers(glm(fo, data = a, weights = .fit_wt, family = quasibinomial(),
    control = glm.control(maxit = 100)), warning = function(e) {
      warnings <<- c(warnings, conditionMessage(e)); invokeRestart("muffleWarning")
    })
  stopifnot(f$converged, f$rank == length(coef(f)), all(is.finite(coef(f))))
  attr(f, "fit_warnings") <- unique(warnings)
  f
}
coef_table <- function(f, V) {
  a <- as.data.frame(clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite"))
  lo <- a$beta - qt(.975, a$df_Satt) * a$SE; hi <- a$beta + qt(.975, a$df_Satt) * a$SE
  data.frame(term = rownames(a), estimate = a$beta, se = a$SE, df = a$df_Satt,
    p = a$p_Satt, lo = lo, hi = hi, odds_ratio = exp(a$beta), or_lo = exp(lo), or_hi = exp(hi))
}
block_test <- function(f, V, fields) {
  ix <- match(fields, names(coef(f))); stopifnot(!anyNA(ix))
  K <- diag(length(coef(f)))[ix, , drop = FALSE]
  t <- clubSandwich::Wald_test(f, constraints = K, vcov = V, test = "HTZ")
  data.frame(F = t$Fstat, df_num = t$df_num, df_denom = t$df_denom, p = t$p_val)
}
vifs <- function(f, a) {
  X <- model.matrix(f)[, -1, drop = FALSE]
  do.call(rbind, lapply(seq_len(ncol(X)), function(j) {
    r <- lm.wfit(cbind(1, X[, -j, drop = FALSE]), X[, j], w = a$weight)
    total <- sum(a$weight * (X[, j] - weighted.mean(X[, j], a$weight))^2)
    data.frame(term = colnames(X)[j], vif = 1 / (sum(a$weight * r$residuals^2) / total))
  }))
}
fits <- coefficients <- samples <- tests <- diagnostics <- leaveout <- list()
for (id in names(specs)) {
  fo <- reformulate(specs[[id]], "y_modern"); f <- fit_glm(fo, cc)
  V <- clubSandwich::vcovCR(f, cluster = cc$geo, type = "CR2", inverse_var = FALSE)
  sf <- survey::svyglm(fo, des, family = quasibinomial())
  point_error <- max(abs(coef(f) - coef(sf))); stopifnot(point_error < 1e-7)
  a <- coef_table(f, V); a$model <- id; coefficients[[id]] <- a
  vv <- vifs(f, cc); vv$model <- id; diagnostics[[id]] <- vv
  for (block in c(if (id != "supply") "Demand", if (id %in% c("supply", "score_joint", "components_joint")) "Supply")) {
    fields <- if (block == "Supply") "supply_risk" else if (grepl("^score", id)) "demand_score" else D
    t <- block_test(f, V, fields); t$model <- id; t$block <- block
    tests[[length(tests) + 1L]] <- t
  }
  samples[[id]] <- data.frame(model = id, n = nrow(cc), events = sum(cc$y_modern),
    non_events = sum(cc$y_modern == 0), weighted_modern_use = weighted.mean(cc$y_modern, cc$weight),
    lgas = length(unique(cc$geo)), parameters = length(coef(f)), design_rank = f$rank,
    max_vif = max(vv$vif), minimum_df = min(a$df[a$term != "(Intercept)"]),
    min_fitted = min(fitted(f)), max_fitted = max(fitted(f)), converged = f$converged,
    coefficient_error_vs_svyglm = point_error,
    warnings = paste(attr(f, "fit_warnings"), collapse = " | "))
  fields <- intersect(c("demand_score", "supply_risk", D), names(coef(f)))
  for (g in sort(unique(cc$geo))) {
    ff <- fit_glm(fo, cc[cc$geo != g, ])
    leaveout[[length(leaveout) + 1L]] <- data.frame(model = id, omitted_lga = g, term = fields,
      estimate = coef(ff)[fields], full_estimate = coef(f)[fields], row.names = NULL)
  }
  fits[[id]] <- list(fit = f, V = V, data = cc, formula = fo)
  cat(id, ": N =", nrow(cc), "; events =", sum(cc$y_modern), "; max VIF =", round(max(vv$vif), 2), "\n")
}
bind <- function(a) do.call(rbind, a)
coefficients <- bind(coefficients); samples <- bind(samples); tests <- bind(tests)
tests$holm_p <- ave(tests$p, tests$model, FUN = function(v) p.adjust(v, "holm"))
diagnostics <- bind(diagnostics); leaveout <- bind(leaveout)
savecsv <- function(a, name) write.csv(a, file.path(out, name), row.names = FALSE, na = "")
savecsv(coefficients, "model_coefficients.csv"); savecsv(samples, "model_samples.csv")
savecsv(tests, "model_block_tests.csv"); savecsv(diagnostics, "model_vifs.csv")
savecsv(leaveout, "leave_one_lga_out.csv")
missing <- data.frame(variable = common_vars,
  missing_in_need = vapply(d[d$need, common_vars], function(a) sum(is.na(a)), integer(1)))
savecsv(missing, "model_missingness.csv")
savecsv(z[, c("geo", "demand_score", "supply_score")], "model_lga_scores.csv")
# Broader available-case score-only checks isolate the effect of requiring the
# six individual demand responses for common-sample comparisons.
available <- d$need & positive & complete.cases(d[c("y_modern", controls, "demand_score", "supply_risk")])
sensitivity <- list()
for (id in c("score_demand", "supply", "score_joint")) {
  aa <- d[available, ]; f <- fit_glm(fits[[id]]$formula, aa)
  V <- clubSandwich::vcovCR(f, cluster = aa$geo, type = "CR2", inverse_var = FALSE)
  t <- coef_table(f, V); t$model <- id; t$n <- nrow(aa); sensitivity[[id]] <- t
}
sensitivity <- bind(sensitivity); savecsv(sensitivity, "broader_score_sample_sensitivity.csv")
Xmat <- function(f, a) model.matrix(delete.response(terms(f)), a,
  contrasts.arg = f$contrasts, xlev = f$xlevels)[, names(coef(f)), drop = FALSE]
margin <- function(ob, changes = list()) {
  a <- ob$data
  for (k in names(changes)) a[[k]] <- changes[[k]]
  X <- Xmat(ob$fit, a); pp <- plogis(as.numeric(X %*% coef(ob$fit))); w <- a$weight / sum(a$weight)
  list(estimate = sum(w * pp), gradient = colSums(X * (w * pp * (1 - pp))))
}
ci <- function(ob, estimate, gradient, probability = FALSE) {
  g <- matrix(gradient, nrow = 1)
  lc <- as.data.frame(clubSandwich::linear_contrast(ob$fit, vcov = ob$V, contrasts = g, test = "Satterthwaite"))
  df <- as.numeric(lc[[4]][1]); se <- sqrt(max(0, as.numeric(g %*% ob$V %*% t(g))))
  q <- qt(.975, df)
  if (probability) {
    lo <- plogis(qlogis(estimate) - q * se / (estimate * (1 - estimate)))
    hi <- plogis(qlogis(estimate) + q * se / (estimate * (1 - estimate)))
  } else { lo <- estimate - q * se; hi <- estimate + q * se }
  data.frame(estimate = estimate, se = se, df = df, lo = lo, hi = hi)
}
curves <- list()
for (k in c("demand_score", "supply_risk")) {
  v <- if (k == "supply_risk") unique(cc[, c("geo", "residence", k)])[[k]] else if (demand_resolution == "individual") unique(cc[[k]]) else unique(cc[, c("geo", k)])[[k]]
  grid <- seq(min(v), max(v), length.out = 41)
  for (id in c(if (k == "demand_score") "score_demand" else "supply", "score_joint")) for (value in grid) {
    ob <- fits[[id]]; a <- margin(ob, setNames(list(value), k))
    t <- ci(ob, a$estimate, a$gradient, TRUE)
    t$model <- id; t$term <- k; t$value <- value
    curves[[length(curves) + 1L]] <- t
  }
}
curves <- bind(curves); savecsv(curves, "score_predicted_probabilities.csv")
gaps <- list(); max_prediction_error <- max_gradient_error <- 0
for (k in D) {
  ob <- fits$components_joint
  low <- margin(ob, setNames(list(0), k)); high <- margin(ob, setNames(list(1), k))
  grad <- high$gradient - low$gradient; gap <- high$estimate - low$estimate
  t <- ci(ob, gap, grad); t$term <- k; t$probability_absent <- low$estimate; t$probability_present <- high$estimate
  gaps[[k]] <- t
  for (value in 0:1) {
    aa <- ob$data; aa[[k]] <- value
    direct <- weighted.mean(predict(ob$fit, newdata = aa, type = "response"), aa$weight)
    max_prediction_error <- max(max_prediction_error, abs(direct - if (value == 0) low$estimate else high$estimate))
  }
  a0 <- ob$data; a1 <- ob$data; a0[[k]] <- 0; a1[[k]] <- 1
  X0 <- Xmat(ob$fit, a0); X1 <- Xmat(ob$fit, a1); b <- coef(ob$fit); w <- ob$data$weight / sum(ob$data$weight)
  for (j in unique(c(1, match(k, names(b))))) {
    up <- down <- b; up[j] <- up[j] + 1e-6; down[j] <- down[j] - 1e-6
    fun <- function(bb) sum(w * (plogis(as.numeric(X1 %*% bb)) - plogis(as.numeric(X0 %*% bb))))
    max_gradient_error <- max(max_gradient_error, abs((fun(up) - fun(down)) / 2e-6 - grad[j]))
  }
}
gaps <- bind(gaps); savecsv(gaps, "individual_demand_probability_gaps.csv")
stopifnot(max_prediction_error < 1e-10, max_gradient_error < 1e-7)
source(file.path(script_dir, "..", "common", "plot_models.R"))
labels <- render_modern_use_plots(coefficients, curves, gaps, cc, p$demand_labels,
  D, out, score_method, demand_resolution, resolution, "lagos")
influence <- do.call(rbind, lapply(split(leaveout, interaction(leaveout$model, leaveout$term, drop = TRUE)), function(a)
  data.frame(model = a$model[1], term = a$term[1], full_estimate = a$full_estimate[1],
    deletion_min = min(a$estimate), deletion_max = max(a$estimate),
    deletion_sign_changes = sum(sign(a$estimate) != sign(a$full_estimate)))))
savecsv(influence, "influence_summary.csv")
audit <- list(need_n = sum(d$need), common_n = nrow(cc), common_events = sum(cc$y_modern),
  eligible_weight_share = sum(cc$weight) / sum(d$weight[d$need]),
  need_definition = "Original supplied SURGO fpn2 == 1; unchanged.",
  outcome = "Confirmed modern use = 1; confirmed nonuse or traditional-only use = 0. Unresolved current-use or method classification excluded from models, retained in overview denominators.",
  unresolved_modern_in_need = sum(is.na(d$y_modern[d$need])),
  known_modern_and_covariates_n = sum(d$need & positive & complete.cases(d[c("y_modern", controls)])),
  common_complete_cases = common_vars, controls = controls, model_specs = specs,
  supply_resolution = resolution,
  score_method = score_method,
  pca_score_md5 = if (score_method == "pca") unname(tools::md5sum(pca_score_file)) else NULL,
  demand_resolution = demand_resolution,
  demand_distinct_values_model = length(unique(cc$demand_score)),
  demand_distinct_values_need = length(unique(d$demand_score[d$need & is.finite(d$demand_score)])),
  supply_profiles = length(unique(profile_key[d$need])),
  demand_score_definition = if (score_method == "pca") "Separate weighted individual-demand correlation PCA on six binary responses among complete FPN2=1 women. Oriented PC1 enters continuously. The LGA planning index is not used." else if (demand_resolution == "individual") "Each woman's six binary demand responses are standardized using the SURGO-weighted item mean and weighted population SD among valid FPN2=1 respondents, then averaged with equal weights. All six responses required. The LGA planning index is retained separately and is not a model predictor." else "LGA contextual prevalence z-score average from the planning map; historical sensitivity only.",
  score_origin = if (score_method == "pca") "Separate weighted supply PCA using the six supply values attached to each FPN2=1 woman by exact LGA-rural/urban match. Equivalent to PCA of the 18 profiles weighted by summed SURGO woman weights. No facility sampling weights; map/heatmap unchanged." else if (resolution == "urban_rural") "Supply uses each woman's exact LGA-rural/urban profile, standardized using the fixed component means and SDs from the 17-LGA map reference; equal averaging over six components. Map/heatmap remain LGA-wide." else "LGA-wide supply-risk exposure; geographic sensitivity.",
  supply_risk = if (score_method == "pca") "Negative of favorably oriented supply PC1; loadings may have mixed signs, so this is a risk-oriented component pattern rather than an all-domain monotonic readiness index. No final-score restandardization." else "Minus the average favorable standardized supply components. No restandardization of the final index, rank transformation, dichotomization or interquartile scaling in any model.",
  supply_fallback = if (resolution == "urban_rural") "Explicitly approved Ibeju-Lekki Urban fallback: only the missing most-popular-product component uses its LGA mean. The other five components retain exact Urban-profile values. Source missing value remains missing; no zero recoding." else "Not applicable; LGA-wide exposure.",
  fallback_women = sum(d$supply_component_fallback), fallback_need = sum(d$supply_component_fallback & d$need),
  fallback_model_n = sum(cc$supply_component_fallback),
  weights = "Original final SURGO weight; one common rescaling to mean 1 for numerical stability. No facility weights.",
  inference = "Weighted logistic regression with CR2 LGA-clustered covariance and coefficient-specific Satterthwaite intervals; HTZ joint block tests, Holm within each joint model. 17 geographic clusters.",
  design_limitation = "LGA clustering is a working geographic inference approach, not a verified official survey-PSU specification. Official EA/stratum identifiers unavailable; no inferred household-ID clusters or FPC. Full positive-weight frame is constructed before need/complete-case domain selection for svyglm coefficient checks.",
  score_uncertainty = "Intervals condition on the estimated indices and observed standardization population; score-construction sampling uncertainty is not propagated.",
  power = "No formal prospective power analysis. Common sample, events, convergence, full rank, weighted VIF, fitted probabilities, leave-one-LGA-out influence and coefficient-specific df are checked. 17 LGA contexts limit precision regardless of the woman count.",
  probability_checks = list(maximum_direct_prediction_error = max_prediction_error, maximum_numeric_gradient_error = max_gradient_error),
  maximum_report_prevalence_error = max_prevalence_error,
  broader_score_sample_n = sum(available), input_file = basename(input), input_md5 = source_hash,
  report_data_md5 = report_hash, source_files_unchanged = source_hash == unname(tools::md5sum(input)) && report_hash == unname(tools::md5sum(report_path)))
stopifnot(audit$source_files_unchanged)
payload <- list(audit = audit, coefficients = coefficients, samples = samples, tests = tests,
  vifs = diagnostics, curves = curves, gaps = gaps, influence = influence,
  broader_sample_coefficients = sensitivity, labels = as.list(labels))
jsonlite::write_json(payload, file.path(out, "model_results.json"), dataframe = "rows", auto_unbox = TRUE,
  pretty = FALSE, digits = 12, na = "null")
jsonlite::write_json(audit, file.path(out, "model_audit.json"), auto_unbox = TRUE, pretty = TRUE, digits = 10)
saveRDS(fits, file.path(out, "fitted_models.rds"))
print(samples); print(tests)
print(coefficients[coefficients$term %in% c("demand_score", "supply_risk"), ])
