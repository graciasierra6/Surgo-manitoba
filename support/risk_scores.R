# Shared constraint orientation, composite scores and scoring sensitivities
#
# Required R packages (restore once: Rscript setup.R):
#   jsonlite.
# Exact package versions: renv.lock. Project dependencies: DESCRIPTION.
# setup.R uses renv to restore the recorded project package environment.
# stats, utils, tools, grid, graphics and grDevices are bundled with R where used.
#
# Data contract (prepared aggregate objects, supplied by callers):
#   report_data.json payload: component values, score references and area scores
#   Prepared supply-component data frames and saved aggregate score objects
#
# No original SURGO/UoM file is read directly here. The caller prepares weighted demand estimates
#   and unweighted FP-facility summaries.
# Used automatically by country/report scripts; outputs include alternative scoring and
#   final-composite SD map data.
# Full filename inventory: docs/DATASET_FILENAMES.md; roles/checksums: docs/INPUTS.md.
# Input locations and credentials are runtime parameters, never saved personal paths.

risk_category <- function(demand, supply) {
  ifelse(is.na(demand) | is.na(supply), "unassessed",
    ifelse(demand & supply, "both",
      ifelse(demand, "demand", ifelse(supply, "supply", "neither"))))
}

supply_constraint_score <- function(supply, component_trace, constraints_reference = TRUE) {
  fields <- unique(component_trace$variable[component_trace$side == "Supply"])
  standardized <- sapply(fields, function(k) {
    ref <- component_trace[component_trace$side == "Supply" & component_trace$variable == k, ]
    mu <- unique(ref$indicator_mean); s <- unique(ref$indicator_SD)
    if (!constraints_reference) mu <- 100 - mu
    stopifnot(length(mu) == 1, length(s) == 1, is.finite(s), s > 0)
    risk <- if (k == "stockout_burden") supply[[k]] else 1 - supply[[k]]
    (100 * risk - mu) / s
  })
  rowMeans(standardized)
}

orient_supply_constraints <- function(payload) {
  previous <- payload$scores
  if (!identical(payload$audit$supply_score_orientation, "constraints")) {
  ii <- payload$scores$method != "median_count"
  payload$scores$supply_score[ii] <- -payload$scores$supply_score[ii]
  ti <- payload$thresholds$method != "median_count"
  payload$thresholds$supply_cutoff[ti] <- -payload$thresholds$supply_cutoff[ti]
  payload$thresholds$supply_rule[ti] <- ifelse(
    payload$thresholds$method[ti] == "zscore",
    "strictly above constraints-score mean (zero)",
    "strictly above constraints-score median")
  tr <- payload$component_trace
  si <- tr$side == "Supply"
  tr$aligned_pct[si] <- 100 - tr$aligned_pct[si]
  tr$indicator_mean[si] <- 100 - tr$indicator_mean[si]
  if ("indicator_median" %in% names(tr)) tr$indicator_median[si] <- 100 - tr$indicator_median[si]
  tr$zscore[si] <- -tr$zscore[si]
  if ("robust" %in% names(tr)) tr$robust[si] <- -tr$robust[si]
  if ("component_rank" %in% names(tr)) {
    for (k in unique(tr$variable[si])) {
      j <- si & tr$variable == k
      n <- if ("reference_n" %in% names(tr)) unique(tr$reference_n[j]) else sum(is.finite(tr$component_rank[j]))
      tr$component_rank[j] <- n + 1 - tr$component_rank[j]
    }
  }
  payload$component_trace <- tr
  if (is.data.frame(payload$planning)) {
    payload$planning$supply_score <- -payload$planning$supply_score
  } else for (j in seq_along(payload$planning)) {
    payload$planning[[j]]$supply_score <- -payload$planning[[j]]$supply_score
  }
  }
  primary <- payload$scores$method == "zscore"
  # A sign reversal, with the inequality reversed, must preserve every flag.
  stopifnot(identical(payload$scores$category, previous$category),
    identical(payload$scores$supply_flag[primary],
              payload$scores$supply_score[primary] > 1e-10))
  payload$audit$supply_score_orientation <- "constraints"
  payload$audit$supply_alignment <- paste(
    "All six scored supply components increase with constraints.",
    "Stockout burden retains its risk direction; the other five use 1 - provision.",
    "Risk equals negative readiness. Raw heatmap percentages are unchanged.")
  payload$audit$scoring_direction <- "Demand > 0 and supply constraints > 0; strict zero ties are unflagged."
  payload$audit$scoring_reference <- paste(
    "Each side uses its complete-area reference set with equal weight per area.",
    "Center each risk-aligned component on its area mean and divide by its sample SD; average six z-scores.",
    "Demand percentages use SURGO weights; facility summaries use equal-facility valid-response means.")
  payload$audit$four_category_labels <- "Relative above-average demand barriers and supply constraints; neither flagged does not mean no barriers or adequate services."
  payload$audit$survey_design <- "SURGO weights with ids=~1, no strata or FPC. Original sampling PSUs are not specified. Regression uncertainty is separately clustered by administrative area using CR2 and Satterthwaite tests."
  payload$audit$sign_reversal_flags_unchanged <- TRUE
  payload
}

# Sensitivity analyses ---------------------------------------------------------
# The primary index is the equal-weight mean of six component z-scores. These
# alternatives use the same inputs and retain their earlier decision rules.
alternative_area_scores <- function(payload) {
  areas <- payload$scores$geo[payload$scores$method == "zscore"]
  matrices <- references <- list()
  for (side in c("Demand", "Supply")) {
    fields <- if (side == "Demand") payload$demand_variables else payload$supply_variables
    tr <- payload$component_trace[payload$component_trace$side == side, ]
    m <- sapply(fields, function(k) {
      a <- tr[tr$variable == k, ]; a$aligned_pct[match(areas, a$geo)]
    })
    rownames(m) <- areas
    ref <- complete.cases(m)
    med <- apply(m[ref, , drop = FALSE], 2, median)
    iqr <- apply(m[ref, , drop = FALSE], 2, IQR)
    ranks <- matrix(NA_real_, nrow(m), ncol(m), dimnames = dimnames(m))
    ranks[ref, ] <- apply(m[ref, , drop = FALSE], 2, rank, ties.method = "average")
    robust <- if (all(iqr > 0)) rowMeans(sweep(sweep(m, 2, med, "-"), 2, iqr, "/")) else rep(NA_real_, nrow(m))
    matrices[[side]] <- list(rank = rowMeans(ranks), raw = rowMeans(m),
      robust = robust, median_count = rowSums(sweep(m, 2, med, "-") > 1e-10))
    references[[side]] <- sum(ref)
  }
  z <- payload$scores[payload$scores$method == "zscore", ]
  result <- list(z); cuts <- list(payload$thresholds[payload$thresholds$method == "zscore", ])
  for (id in c("rank", "raw", "robust", "median_count")) {
    ds <- matrices$Demand[[id]]; ss <- matrices$Supply[[id]]
    dc <- if (id == "median_count") 4 else if (any(is.finite(ds))) median(ds, na.rm = TRUE) else NA_real_
    sc <- if (id == "median_count") 4 else if (any(is.finite(ss))) median(ss, na.rm = TRUE) else NA_real_
    df <- if (id == "median_count") ds >= dc else ds > dc + 1e-10
    sf <- if (id == "median_count") ss >= sc else ss > sc + 1e-10
    result[[id]] <- data.frame(method = id, geo = areas, demand_score = ds,
      supply_score = ss, demand_flag = df, supply_flag = sf,
      demand_priority_rank = rank(-ds, ties.method = "average", na.last = "keep"),
      supply_priority_rank = rank(-ss, ties.method = "average", na.last = "keep"),
      category = risk_category(df, sf))
    cuts[[id]] <- data.frame(method = id, demand_cutoff = dc, supply_cutoff = sc,
      demand_rule = if (id == "median_count") "4 or more of six above component medians" else "strictly above final-score median",
      supply_rule = if (id == "median_count") "4 or more of six constraint components above medians" else "strictly above constraints-score median",
      demand_reference_n = sum(is.finite(ds)), supply_reference_n = sum(is.finite(ss)))
  }
  payload$scores <- do.call(rbind, result)
  payload$thresholds <- do.call(rbind, cuts)
  payload
}

add_sd_thresholds <- function(payload, cutoffs = c(0, 0.5, 1, 1.5)) {
  z <- payload$scores[payload$scores$method == "zscore", ]
  dm <- mean(z$demand_score, na.rm = TRUE); ds <- sd(z$demand_score, na.rm = TRUE)
  sm <- mean(z$supply_score, na.rm = TRUE); ss <- sd(z$supply_score, na.rm = TRUE)
  stopifnot(ds > 0, ss > 0, abs(dm) < 1e-8, abs(sm) < 1e-8)
  du <- (z$demand_score - dm) / ds; su <- (z$supply_score - sm) / ss
  payload$risk_distribution <- data.frame(geo = z$geo, demand_score = z$demand_score,
    supply_score = z$supply_score, demand_sd = du, supply_sd = su)
  payload$score_distribution_reference <- list(
    demand = list(n = sum(is.finite(z$demand_score)), mean = dm, SD = ds),
    supply = list(n = sum(is.finite(z$supply_score)), mean = sm, SD = ss),
    definition = "Each side uses its own finite area scores, with equal weight per area. SD refers to the final composite, not an individual component or a standard error.")
  rows <- lapply(cutoffs, function(t) {
    df <- if (t == 0) z$demand_score > 1e-10 else du >= t
    sf <- if (t == 0) z$supply_score > 1e-10 else su >= t
    data.frame(geo = z$geo, threshold_sd = t, demand_score = z$demand_score,
      supply_score = z$supply_score, demand_sd = du, supply_sd = su,
      demand_flag = df, supply_flag = sf, category = risk_category(df, sf))
  })
  payload$sd_thresholds <- do.call(rbind, rows)
  stopifnot(identical(payload$sd_thresholds$category[payload$sd_thresholds$threshold_sd == 0], z$category))
  payload
}

export_risk_scores <- function(payload, out) {
  payload <- orient_supply_constraints(payload)
  payload <- alternative_area_scores(payload)
  payload <- add_sd_thresholds(payload)
  write <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
  write(payload$scores, "all_method_scores_and_classifications.csv")
  write(payload$thresholds, "classification_thresholds.csv")
  write(payload$component_trace, "component_score_trace.csv")
  write(payload$sd_thresholds, "sd_threshold_classifications.csv")
  write(payload$risk_distribution, "area_risk_score_distributions.csv")
  jsonlite::write_json(payload, file.path(out, "report_data.json"), dataframe = "rows",
                       auto_unbox = TRUE, na = "null", digits = 12)
  jsonlite::write_json(payload$audit, file.path(out, "analysis_audit.json"),
                       auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(payload)
}
