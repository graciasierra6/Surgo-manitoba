# Weighted PCA sensitivity using individual concerns and each woman's exact
# LGA-residence supply exposure. Main geographic classifications are untouched.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k, args); if (is.na(i) || i == length(args)) stop("Supply ", k); args[i + 1L] }
input <- opt("--input"); data_dir <- opt("--data-dir"); out <- opt("--output-dir")
for (pkg in c("jsonlite", "ggplot2", "svglite")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
script_dir <- dirname(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE))
source(file.path(script_dir, "lagos_definitions.R"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
source_hash <- unname(tools::md5sum(input)); report_file <- file.path(data_dir, "report_data.json")
report_hash <- unname(tools::md5sum(report_file)); report <- jsonlite::fromJSON(report_file)
stopifnot(source_hash == report$audit$source_md5[1])
x <- readRDS(input); d <- lagos_derive_women(x); D <- report$demand_variables; S <- report$supply_variables
field_map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
  counselor = "share_fp_counselor", larc_stock = "share_stocked_3plus_larc")
sm <- as.data.frame(lapply(field_map, function(k) as.numeric(x[[k]])))
yes <- as.numeric(x$n_mp_in_stock_true); no <- as.numeric(x$n_mp_in_stock_false)
sm$popular_stock <- ifelse(yes + no > 0, yes / (yes + no), NA_real_)
observed_popular <- sm$popular_stock; fallback <- !is.finite(observed_popular)
stopifnot(all(d$geo[fallback] == "IBEJULEKKI" & d$residence[fallback] == "Urban"))
hm <- report$heatmap[report$heatmap$side == "Supply" & report$heatmap$variable == "popular_stock", ]
sm$popular_stock[fallback] <- hm$value[match(d$geo[fallback], hm$geo)] / 100
eligible <- d$need & is.finite(d$weight) & d$weight > 0
dm <- as.matrix(d[D]); sm <- as.matrix(sm[S])
di <- eligible & complete.cases(dm); si <- eligible & complete.cases(sm)
stopifnot(sum(eligible) == 2159, sum(di) == 1891, sum(si) == 2159)
weighted_pca <- function(m, weights, anchor = NULL) {
  w <- weights / sum(weights); center <- colSums(m * w)
  centered <- sweep(m, 2, center, "-"); scale <- sqrt(colSums(centered^2 * w))
  stopifnot(all(scale > 0)); Z <- sweep(centered, 2, scale, "/")
  R <- crossprod(Z, Z * w); ee <- eigen(R, symmetric = TRUE)
  rotation <- ee$vectors; rownames(rotation) <- colnames(m); colnames(rotation) <- paste0("PC", 1:ncol(m))
  for (j in seq_len(ncol(rotation))) {
    orient <- if (j == 1 && !is.null(anchor)) sum(rotation[, j] * anchor) else sum(rotation[, j])
    if (orient < 0) rotation[, j] <- -rotation[, j]
  }
  list(center = center, scale = scale, rotation = rotation, eigenvalues = ee$values,
    correlation = R, scores = Z %*% rotation)
}
project <- function(f, m) sweep(sweep(m, 2, f$center, "-"), 2, f$scale, "/") %*% f$rotation
fd <- weighted_pca(dm[di, , drop = FALSE], d$weight[di])
fs <- weighted_pca(sm[si, , drop = FALSE], d$weight[si])
scores <- data.frame(row = d$row, demand_score = project(fd, dm)[, 1],
  supply_risk = -project(fs, sm)[, 1], fallback = fallback)
stopifnot(all(is.finite(scores$demand_score[di])), all(is.finite(scores$supply_risk[si])))
key <- paste(d$geo, d$residence, sep = "::")
profiles <- data.frame(profile = key, geo = d$geo, residence = d$residence, sm,
  popular_stock_observed = observed_popular, fallback = fallback, supply_PC1 = -scores$supply_risk,
  supply_risk = scores$supply_risk, n_facilities = as.numeric(x$n_facilities))
stopifnot(!anyDuplicated(unique(profiles)$profile))
profiles <- profiles[!duplicated(key), ]
profiles$women_in_need <- as.integer(table(factor(key[si], levels = profiles$profile)))
profiles$SURGO_weight <- vapply(profiles$profile, function(k) sum(d$weight[si & key == k]), numeric(1))
profiles$weight_share <- profiles$SURGO_weight / sum(profiles$SURGO_weight)
stopifnot(nrow(profiles) == 18)
# Collapse repeated supply exposures with summed woman weights: exactly the same PCA.
collapsed <- weighted_pca(as.matrix(profiles[S]), profiles$SURGO_weight, fs$rotation[, 1])
collapse_error <- max(abs(collapsed$rotation[, 1] - fs$rotation[, 1]),
  abs(collapsed$center - fs$center), abs(collapsed$scale - fs$scale))
stopifnot(collapse_error < 1e-10)
savecsv <- function(a, name) write.csv(a, file.path(out, name), row.names = FALSE, na = "")
savecsv(profiles, "pca_matched_supply_profiles.csv")
fits <- list(Demand = fd, Supply = fs); loads <- variance <- moments <- list()
set.seed(20261106); B <- 2000L
for (side in names(fits)) {
  f <- fits[[side]]; fields <- if (side == "Demand") D else S
  for (j in 1:6) loads[[length(loads) + 1L]] <- data.frame(side = side, variable = fields, PC = j,
    loading = f$rotation[, j], squared_loading_share = f$rotation[, j]^2,
    variable_PC_correlation = f$rotation[, j] * sqrt(f$eigenvalues[j]))
  moments[[side]] <- data.frame(side = side, variable = fields, center = f$center, scale = f$scale)
  if (side == "Demand") {
    m <- dm[di, , drop = FALSE]; ww <- d$weight[di]; groups <- split(seq_len(nrow(m)), d$geo[di])
    null <- replicate(B, {
      perm <- m
      for (j in 1:6) for (ii in groups) perm[ii, j] <- sample(m[ii, j], length(ii), replace = FALSE)
      weighted_pca(perm, ww)$eigenvalues
    })
  } else {
    m <- as.matrix(profiles[S]); ww <- profiles$SURGO_weight
    null <- replicate(B, {
      perm <- apply(m, 2, sample)
      weighted_pca(perm, ww)$eigenvalues
    })
  }
  cutoff <- apply(null, 1, quantile, probs = .95, type = 7, names = FALSE)
  variance[[side]] <- data.frame(side = side, PC = 1:6, eigenvalue = f$eigenvalues,
    variance_pct = 100 * f$eigenvalues / sum(f$eigenvalues),
    cumulative_pct = 100 * cumsum(f$eigenvalues) / sum(f$eigenvalues),
    permutation_95 = cutoff, above_permutation_95 = f$eigenvalues > cutoff)
}
loads <- do.call(rbind, loads); variance <- do.call(rbind, variance); moments <- do.call(rbind, moments)
savecsv(loads, "pca_loadings.csv"); savecsv(variance, "pca_variance.csv"); savecsv(moments, "pca_centers_scales.csv")
savecsv(data.frame(variable = D, fd$correlation, check.names = FALSE), "pca_demand_correlations.csv")
savecsv(data.frame(variable = S, fs$correlation, check.names = FALSE), "pca_supply_correlations.csv")
stability <- list()
for (g in sort(unique(d$geo[eligible]))) for (side in names(fits)) {
  m <- if (side == "Demand") dm else sm; use <- if (side == "Demand") di else si
  fit <- fits[[side]]; train <- use & d$geo != g
  f <- weighted_pca(m[train, , drop = FALSE], d$weight[train], fit$rotation[, 1])
  full_score <- project(fit, m[use, , drop = FALSE])[, 1]
  alt_score <- project(f, m[use, , drop = FALSE])[, 1]
  stability[[length(stability) + 1L]] <- data.frame(side = side, omitted_lga = g,
    loading_cosine = sum(f$rotation[, 1] * fit$rotation[, 1]),
    score_rank_correlation = cor(full_score, alt_score, method = "spearman"),
    pc1_variance_pct = 100 * f$eigenvalues[1] / sum(f$eigenvalues))
}
stability <- do.call(rbind, stability); savecsv(stability, "pca_leave_one_lga_out.csv")
pc1 <- loads[loads$PC == 1, ]
audit <- list(method = "Separate survey-weighted correlation-matrix PCAs; first component on each side, prespecified as the score sensitivity.",
  need_n = sum(eligible), demand_PCA_n = sum(di), supply_PCA_women_n = sum(si), supply_profiles = nrow(profiles),
  demand_source = "Each woman's six binary indicators; all six valid required; original SURGO FPN2=1 and final weights.",
  supply_source = "Each woman's six exact LGA-rural/urban facility measures, with the approved Ibeju Urban popular-stock component fallback. No LGA-wide substitution for other components.",
  PCA_weighting = "Both PCAs describe the final-weighted distribution of women in need. Supply profile weights are sums of their women's SURGO weights, not facility sampling weights.",
  sign = "Demand PC1 oriented toward the average standardized concern; favorable supply PC1 toward average standardized provision, then negated for the model risk-oriented supply PC1. Mixed signs remain unchanged and are disclosed.",
  negative_demand_PC1_loadings = pc1$variable[pc1$side == "Demand" & pc1$loading < 0],
  negative_favorable_supply_PC1_loadings = pc1$variable[pc1$side == "Supply" & pc1$loading < 0],
  demand_PC1_variance = variance$variance_pct[variance$side == "Demand" & variance$PC == 1],
  supply_PC1_variance = variance$variance_pct[variance$side == "Supply" & variance$PC == 1],
  fallback_need = sum(fallback & eligible), supply_collapse_equivalence_error = collapse_error,
  classification = "No new demand/supply/both/neither cutoffs or map. Existing LGA planning classifications remain unchanged; continuous individual/profile PC1 scores enter models.",
  dimensionality = "PC1 is a variance-maximizing pattern, not automatically a unidimensional or monotonic barrier index. PC2 and all component loadings/variance remain available.",
  permutation = list(seed = 20261106, repetitions = B,
    demand = "Permute each item among complete women within LGA, retaining women weights; preserves unweighted LGA item margins but removes within-LGA item association.",
    supply = "Permute each component across 18 profiles retaining summed woman weights. Exploratory null benchmark, not a complex-survey significance test or proof of dimensionality."),
  stability = "Leave one whole LGA out, refit weighted normalization and PCA, align PC1 signs, project all complete cases. Fit stability, not sampling confidence or outcome prediction.",
  input_md5 = source_hash, report_md5 = report_hash,
  source_unchanged = source_hash == unname(tools::md5sum(input)) && report_hash == unname(tools::md5sum(report_file)))
stopifnot(audit$source_unchanged)
obj <- list(audit = audit, scores = scores, demand_fit = fd, supply_fit = fs, profiles = profiles,
  loadings = loads, variance = variance, moments = moments, stability = stability)
saveRDS(obj, file.path(out, "pca_scores.rds"))
jsonlite::write_json(obj[setdiff(names(obj), c("scores", "demand_fit", "supply_fit"))],
  file.path(out, "pca_results.json"), dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 12)
jsonlite::write_json(audit, file.path(out, "pca_audit.json"), auto_unbox = TRUE, pretty = TRUE, digits = 12)
labels <- setNames(c(report$demand_labels, report$supply_labels), c(D, S))
base <- ggplot2::theme_minimal(base_family = "sans", base_size = 11) + ggplot2::theme(
  panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
  plot.title = ggplot2::element_text(face = "bold", size = 18, color = "#17374a"),
  plot.subtitle = ggplot2::element_text(size = 10, color = "#607985", lineheight = 1.2),
  plot.caption = ggplot2::element_text(hjust = 0, color = "#607985", size = 9),
  strip.background = ggplot2::element_rect(fill = "#edf4f6", color = NA),
  strip.text = ggplot2::element_text(face = "bold", color = "#17374a"),
  panel.spacing = grid::unit(1.5, "lines"), legend.position = "bottom",
  plot.margin = ggplot2::margin(14, 18, 12, 12))
saveplot <- function(g, name, height) {
  ggplot2::ggsave(file.path(out, paste0(name, ".png")), g, width = 14, height = height, dpi = 170, bg = "white")
  ggplot2::ggsave(file.path(out, paste0(name, ".svg")), g, width = 14, height = height, device = svglite::svglite, bg = "white")
}
pc1$row <- factor(pc1$variable, levels = rev(c(D, S)))
pc1$direction <- ifelse(pc1$loading < 0, "Negative coefficient", pc1$side)
pc1$panel <- ifelse(pc1$side == "Demand", sprintf("Individual demand PC1 · %.1f%% of variation", audit$demand_PC1_variance),
  sprintf("Favorable supply PC1 · %.1f%% of variation", audit$supply_PC1_variance))
pc1$panel <- factor(pc1$panel, levels = c(sprintf("Individual demand PC1 · %.1f%% of variation", audit$demand_PC1_variance),
  sprintf("Favorable supply PC1 · %.1f%% of variation", audit$supply_PC1_variance)))
g <- ggplot2::ggplot(pc1, ggplot2::aes(loading, row, fill = direction)) +
  ggplot2::geom_vline(xintercept = 0, color = "#8da2ad") + ggplot2::geom_col(width = .55) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.2f", loading), hjust = ifelse(loading < 0, 1.15, -.15)), size = 3.5) +
  ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") + ggplot2::scale_y_discrete(labels = labels) +
  ggplot2::scale_fill_manual(values = c(Demand = "#087f8a", Supply = "#be6b23", "Negative coefficient" = "#8b99a3")) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .2)) +
  ggplot2::labs(title = "What the PCA scores emphasize", subtitle = "Signed coefficients on standardized inputs; demand uses individual responses, supply uses matched rural/urban exposures",
    x = "PC1 scoring coefficient (not an association with contraceptive use)", y = NULL, fill = NULL,
    caption = "Supply coefficients here describe favorable PC1; the model uses its negative. All first-component coefficients are positive here.\nWeights describe variation in the inputs, not program importance or causal effects.") + base
saveplot(g, "01_pca_loadings", 6)
g <- ggplot2::ggplot(variance, ggplot2::aes(PC, eigenvalue, fill = side)) + ggplot2::geom_col(width = .55) +
  ggplot2::geom_line(ggplot2::aes(y = permutation_95), color = "#718997", linewidth = .7) +
  ggplot2::geom_point(ggplot2::aes(y = permutation_95), color = "#718997", size = 1.5) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", variance_pct)), vjust = -.4, size = 3.5) +
  ggplot2::facet_wrap(~side, nrow = 1) + ggplot2::scale_x_continuous(breaks = 1:6) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
  ggplot2::scale_fill_manual(values = c(Demand = "#087f8a", Supply = "#be6b23"), guide = "none") +
  ggplot2::labs(title = "How much of each side's variation does PCA retain?", subtitle = "Bars: observed eigenvalues and variance shares · gray line: exploratory permutation 95th percentile",
    x = "Principal component", y = "Eigenvalue", caption = "2,000 permutations per side. Null constructions differ by level; these are descriptive dimensionality diagnostics, not survey-design tests.") + base
saveplot(g, "02_pca_variance", 5.5)
print(audit); print(pc1[, c("side", "variable", "loading")]); print(variance)
