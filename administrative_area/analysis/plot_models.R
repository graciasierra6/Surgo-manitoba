# Plot saved modern-use estimates without changing model fits or predictions.
render_modern_use_plots <- function(coefficients, curves, gaps, cc, demand_labels,
                                   demand_variables, out, score_method,
                                   demand_resolution, resolution, country, supply_pca_mixed = NULL) {
  cluster <- switch(country, lagos = "LGA", sindh = "district", senegal = "department", cotedivoire = "department")
  adjustment <- switch(country,
    lagos = "All models adjust for age + age squared, education, partnership, fertility goal and urban/rural residence.",
    sindh = "All models adjust for age + age squared, country-specific education and fertility goal.",
    senegal = "All models adjust for age + age squared, Senegal-specific education, partnership, fertility goal and Rural/Urban residence.",
    cotedivoire = "All models adjust for age + age squared, Côte d’Ivoire-specific education, partnership, fertility goal and Rural/Urban residence.")
  pca_note <- switch(country,
    lagos = "Higher scores follow the average concern / weaker-provision direction; PC1 captures only part of each side's variation.",
    sindh = "Demand PC1 has mixed signs and describes a contrast; supply risk runs higher = weaker provision.",
    senegal = "Interpret PCA using its signed coefficients; supply PC1 is negated for modeling.",
    cotedivoire = "Supply PC1 has mixed signs: its negative is a pattern score, not a monotonic service-risk index. Interpret signed coefficients.")
  inference <- switch(country,
    lagos = "Geographic small-sample inference; official EA/stratum design remains unconfirmed. No random effects or interactions.",
    sindh = "District CR2 intervals; sampling-cluster design sensitivity is available. No urban/rural variable. No random effects or interactions.",
    senegal = "Department CR2 intervals; sampling-cluster design sensitivity is available. Residence-specific supply where available; flagged department fallback otherwise. No random effects or interactions.",
    cotedivoire = "Department CR2 intervals; sampling-cluster design sensitivity is available. Exact department-residence supply; no geographic fallback. No random effects or interactions.")
  supply_note <- switch(country,
    lagos = if (resolution == "urban_rural") "Supply uses matched LGA-rural/urban profiles; " else "",
    sindh = "Supply uses matched district profiles; ",
    senegal = "Supply uses assigned department-residence / fallback profiles; ",
    cotedivoire = "Supply uses assigned department-residence profiles; ")
  if (country %in% c("senegal", "cotedivoire") && resolution == "department") {
    inference <- "Department CR2 intervals; sampling-cluster sensitivity is available. Department-wide supply for every woman. No random effects or interactions."
    supply_note <- "Supply uses matched department-wide profiles; "
  }
  mixed_supply <- score_method == "pca" && if (is.null(supply_pca_mixed)) country == "cotedivoire" else isTRUE(supply_pca_mixed)
  if (mixed_supply) pca_note <- "Supply PC1 has mixed signs; its negative is a pattern, not uniformly weaker provision."
  D <- demand_variables
  labels <- setNames(demand_labels, D)
  labels <- c(labels, demand_score = if (score_method == "pca" && country != "lagos") switch(country,
    sindh = "Demand pattern score (PCA)", senegal = "Demand risk score (PCA)", cotedivoire = "Demand score (PCA)") else
    if (demand_resolution == "individual") "Demand risk score" else "LGA demand risk score",
    supply_risk = if (mixed_supply) "Supply pattern score (PCA)" else "Supply risk score")
  colors <- c(Demand = "#087f8a", Supply = "#be6b23")
  base_theme <- ggplot2::theme_minimal(base_family = "sans", base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#244354"),
      plot.title = ggplot2::element_text(face = "bold", size = 17, color = "#17374a"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#5f7886", lineheight = 1.2),
      strip.text = ggplot2::element_text(face = "bold", color = "#17374a", size = 11),
      strip.background = ggplot2::element_rect(fill = "#edf4f6", color = NA),
      legend.position = "bottom", legend.title = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.4, "lines"),
      plot.caption = ggplot2::element_text(hjust = 0, color = "#607a87", size = 9, lineheight = 1.2),
      plot.margin = ggplot2::margin(15, 18, 12, 12))
  saveplot <- function(g, name, h) {
    ggplot2::ggsave(file.path(out, paste0(name, ".png")), g, width = 14, height = h, dpi = 170, bg = "white")
    ggplot2::ggsave(file.path(out, paste0(name, ".svg")), g, width = 14, height = h, device = svglite::svglite, bg = "white")
  }
  forest <- function(ids, titles, fields, title, name, height, caption) {
    a <- coefficients[coefficients$model %in% ids & coefficients$term %in% fields, ]
    a$panel <- factor(titles[match(a$model, ids)], levels = titles)
    a$row <- factor(a$term, levels = rev(fields)); a$source <- ifelse(a$term == "supply_risk", "Supply", "Demand")
    stopifnot(all(is.finite(a$or_lo)), all(is.finite(a$or_hi)), all(a$or_lo > 0))
    g <- ggplot2::ggplot(a, ggplot2::aes(odds_ratio, row, color = source)) +
      ggplot2::geom_vline(xintercept = 1, color = "#8095a0", linetype = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = or_lo, xmax = or_hi), orientation = "y", width = .08, linewidth = .8) +
      ggplot2::geom_point(size = 3) + ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") +
      ggplot2::scale_x_log10(breaks = if (country == "lagos") c(.01, .025, .05, .1, .25, .5, .75, 1, 1.25, 1.5, 2, 4, 10, 25, 50, 100) else c(.01, .05, .1, .25, .5, 1, 2, 4, 10, 25, 50, 100)) +
      ggplot2::scale_y_discrete(labels = labels) + ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(title = title,
        subtitle = paste0("Modern contraceptive use · same ", format(nrow(cc), big.mark = ","), " women · SURGO weights · 95% ", cluster, "-clustered CR2/Satterthwaite intervals"),
        x = "Adjusted odds ratio (log scale; 1 = no association)", y = NULL, caption = caption) + base_theme
    saveplot(g, name, height)
  }
  forest(c("score_demand", "supply", "score_joint"), if (mixed_supply) c("Demand PCA + covariates", "Supply PCA + covariates", "Both PCA scores + covariates") else c("Demand risk + covariates", "Supply risk + covariates", "Both scores + covariates"),
    c("demand_score", "supply_risk"), if (mixed_supply) "PCA sensitivity: demand and supply pattern scores" else if (score_method == "pca") "PCA sensitivity: demand and supply risk scores" else "Adjusted associations: demand and supply risk scores", "01_score_models", 5.2,
    paste0(if (score_method == "pca") paste0("Odds ratios are per 1 oriented PC1 score unit, with no final-score restandardization.\n", pca_note, "\n") else "Each odds ratio is per 1-point increase in the original average component z-score; the final index is not restandardized.\nHigher demand and higher supply risk both represent less favorable conditions. ", adjustment,
      paste0("\n", inference)))
  curves$panel <- factor(curves$term, levels = c("demand_score", "supply_risk"), labels = c(unname(labels["demand_score"]), unname(labels["supply_risk"])))
  curves$source <- ifelse(curves$term == "supply_risk", "Supply", "Demand")
  curves$spec <- ifelse(curves$model == "score_joint", "Both-score model", "Single-score model")
  g <- ggplot2::ggplot(curves, ggplot2::aes(value, estimate * 100, color = source, linetype = spec)) +
    ggplot2::geom_ribbon(data = curves[curves$model == "score_joint", ], ggplot2::aes(ymin = lo * 100, ymax = hi * 100, fill = source), alpha = .12, color = NA) +
    ggplot2::geom_line(linewidth = .95) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_x") + ggplot2::scale_color_manual(values = colors, guide = "none") +
    ggplot2::scale_fill_manual(values = colors, guide = "none") + ggplot2::scale_linetype_manual(values = c("Both-score model" = "solid", "Single-score model" = "dashed")) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    ggplot2::labs(title = if (mixed_supply) "PCA sensitivity: adjusted modern use across demand and supply patterns" else "How adjusted modern-use probabilities vary with demand and local supply risk",
      subtitle = "Weighted average predictions over the same women's observed covariates; the other score stays at its observed value",
      x = if (score_method == "pca") if (mixed_supply) "PCA score (interpret using the signed coefficients)" else if (country == "lagos") "PCA score (higher = greater concern or weaker provision)" else "PCA score (interpret using the signed coefficients)" else "Risk score (average standardized components)", y = "Predicted modern use (%)",
      caption = paste0("Solid lines: both-score model, with pointwise 95% ", cluster, "-clustered CR2 intervals. Dashed lines: single-score models.\nCurves span the observed score ranges. ",
        supply_note, "these are associations, not intervention effects.")) + base_theme +
    ggplot2::scale_x_continuous(breaks = function(limits) pretty(limits, n = 5)) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e8eff2", linewidth = .3))
  if (country != "lagos") g <- g + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(), panel.border = ggplot2::element_blank())
  saveplot(g, "02_score_probabilities", 6)
  forest(c("components_demand", "supply", "components_joint"), if (mixed_supply) c("Six demand indicators", "Supply PCA pattern", "Six indicators + supply PCA") else c("Six demand indicators", "Supply risk score", "Six indicators + supply risk"),
    c(D, "supply_risk"), if (mixed_supply) "PCA sensitivity: individual concerns alongside the local supply pattern" else if (score_method == "pca") "PCA sensitivity: individual concerns alongside local supply PC1" else "Adjusted associations: individual concerns alongside local supply risk", "03_component_models", 7.4,
    paste0(if (mixed_supply) "Demand indicators: concern present versus absent. Supply: per 1 negative-PC1 pattern point; mixed coefficients preclude an overall risk interpretation.\n" else if (score_method == "pca") "Demand indicators: concern present versus absent. Supply: per 1 risk-oriented PC1 point.\n" else "Demand indicators: concern present versus absent. Supply risk: per 1 original index point higher.\n", adjustment,
      "\nThe supply-only model is identical to the preceding section. The MC5 label denotes confidence, not an objective knowledge test."))
  gaps$row <- factor(gaps$term, levels = rev(D))
  g <- ggplot2::ggplot(gaps, ggplot2::aes(estimate * 100, row)) +
    ggplot2::geom_vline(xintercept = 0, color = "#8095a0", linetype = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo * 100, xmax = hi * 100), orientation = "y", width = .18, linewidth = .85, color = colors["Demand"]) +
    ggplot2::geom_point(size = 3, color = colors["Demand"]) + ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::labs(title = "Adjusted modern-use differences for individual reported concerns",
      subtitle = "Six-indicator + supply-risk model; same women and covariates as the coefficient plot",
      x = "Percentage-point difference: concern present minus absent (0 = no gap)", y = NULL,
      caption = "For each concern, predict every woman's modern use with that indicator set to 1 and then 0; average with SURGO weights.\nCapped 95% intervals include covariance between the paired predictions. Other predictors remain observed; differences are not causal effects.") + base_theme
  saveplot(g, "04_component_probability_gaps", 6)
  labels
}
