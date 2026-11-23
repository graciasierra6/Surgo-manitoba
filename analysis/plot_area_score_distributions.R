# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: ggplot2, grid, jsonlite.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Diagnostic/helper.
# Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
# Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Plot stored area scores without refitting indicators, indices or models.
args <- commandArgs(trailingOnly = TRUE)
value <- function(k, default = NULL) {
  i <- match(k, args)
  if (is.na(i)) {
    if (is.null(default)) stop("Supply ", k)
    return(default)
  }
  if (i == length(args)) stop("Supply a value for ", k)
  args[i + 1L]
}
script <- gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
reports <- value("--reports-dir", file.path(root, "reports"))
out <- value("--output-dir")
for (package in c("jsonlite", "ggplot2")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Install ", package)
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
read_scores <- function(file, geography, expected_n) {
  html <- paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  matched <- regmatches(html, regexpr("(?s)const DATA=(\\{.*?\\});", html, perl = TRUE))
  if (!length(matched) || !nzchar(matched)) stop("Stored map data missing: ", basename(file))
  payload <- sub(";$", "", sub("^const DATA=", "", matched))
  data <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  rows <- lapply(names(data), function(key) {
    row <- data[[key]]
    if (is.null(row$demand) || is.null(row$supply)) return(NULL)
    stopifnot(is.finite(row$demand), is.finite(row$supply))
    constraints <- if (grepl('data-supply-score-orientation="constraints"', html, fixed=TRUE)) row$supply else -row$supply
    sf <- constraints > 1e-10
    stopifnot(identical(sf, -(-constraints) > 1e-10))
    data.frame(geography = geography, area = row$name,
               demand = row$demand, supply_constraints = constraints)
  })
  result <- do.call(rbind, rows)
  stopifnot(nrow(result) == expected_n)
  result
}
wide <- rbind(
  read_scores(file.path(reports, "Lagos_analysis_aggregate.html"), "Lagos", 17),
  read_scores(file.path(reports, "Senegal_analysis_aggregate.html"), "Senegal", 33)
)
long <- rbind(
  data.frame(wide[c("geography", "area")], score = "Demand concerns", value = wide$demand),
  data.frame(wide[c("geography", "area")], score = "Supply constraints", value = wide$supply_constraints)
)
summary <- do.call(rbind, lapply(split(long, interaction(long$geography, long$score)), function(x) {
  data.frame(geography = x$geography[1], score = x$score[1], areas = nrow(x),
             mean = mean(x$value), median = median(x$value), SD = sd(x$value),
             minimum = min(x$value), maximum = max(x$value),
             above_zero = sum(x$value > 1e-10),
             above_zero_pct = 100 * mean(x$value > 1e-10))
}))
summary <- summary[order(match(summary$geography, c("Lagos", "Senegal")),
                         match(summary$score, c("Demand concerns", "Supply constraints"))), ]
rownames(summary) <- NULL
stopifnot(all(abs(summary$mean) < 1e-10))
write.csv(wide, file.path(out, "area_scores.csv"), row.names = FALSE)
write.csv(summary, file.path(out, "score_distribution_summary.csv"), row.names = FALSE)

bin_width <- .25
left <- floor(min(long$value) / bin_width) * bin_width
right <- ceiling(max(long$value) / bin_width) * bin_width
breaks <- seq(left, right, by = bin_width)
bins <- do.call(rbind, lapply(split(long, interaction(long$geography, long$score)), function(x) {
  bin <- cut(x$value, breaks, include.lowest = TRUE, right = FALSE)
  count <- as.numeric(table(bin))
  s <- summary[summary$geography == x$geography[1] & summary$score == x$score[1], ]
  data.frame(geography = x$geography[1], score = x$score[1],
             x = head(breaks, -1) + bin_width / 2, count = count,
             panel = paste0(s$geography, " · ", s$score, "\n", s$above_zero,
                            " of ", s$areas, " areas above zero"))
}))
panel_levels <- vapply(seq_len(nrow(summary)), function(i) {
  s <- summary[i, ]
  paste0(s$geography, " · ", s$score, "\n", s$above_zero, " of ", s$areas, " areas above zero")
}, character(1))
bins$panel <- factor(bins$panel, levels = panel_levels)
plot <- ggplot2::ggplot(bins, ggplot2::aes(x = x, y = count, fill = score)) +
  ggplot2::annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
                   fill = "#f0f4f6") +
  ggplot2::geom_col(width = .225, show.legend = FALSE) +
  ggplot2::geom_text(data = bins[bins$count > 0, ], ggplot2::aes(label = count),
                     vjust = -.45, size = 3.8, color = "#17374a") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#17374a", linewidth = .5) +
  ggplot2::facet_wrap(~panel, ncol = 2, scales = "free_y") +
  ggplot2::scale_fill_manual(values = c("Demand concerns" = "#047f8b", "Supply constraints" = "#cd7729")) +
  ggplot2::scale_x_continuous(breaks = seq(-1.5, 1.5, .5), limits = c(left, right),
                             expand = ggplot2::expansion(mult = .015)) +
  ggplot2::scale_y_continuous(breaks = function(limits) seq(0, ceiling(limits[2]), by = 1),
                             expand = ggplot2::expansion(mult = c(0, .2))) +
  ggplot2::labs(title = "Area-level demand and supply score distributions",
    subtitle = "Higher scores indicate more challenges; the dashed zero line is the flag threshold",
    x = "Average of six component z-scores", y = "Number of areas",
    caption = "Each area contributes once. Shaded side: score > 0. Bars use 0.25-point intervals.\nDemand indicators use survey-weighted women in need; supply indicators use FP-provider means.") +
  ggplot2::theme_minimal(base_size = 12, base_family = "Arial") +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#17374a", size = 16),
    plot.subtitle = ggplot2::element_text(color = "#536c7b", size = 11),
    plot.caption = ggplot2::element_text(hjust = 0, color = "#536c7b", size = 10),
    strip.text = ggplot2::element_text(face = "bold", color = "#17374a", size = 12, hjust = 0),
    panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#e6edf0", linewidth = .3),
    panel.spacing = grid::unit(1.3, "lines"), plot.margin = ggplot2::margin(14, 18, 14, 14))
ggplot2::ggsave(file.path(out, "score_distributions.png"), plot, width = 9.5, height = 7.2, dpi = 180, bg = "white")
ggplot2::ggsave(file.path(out, "score_distributions.svg"), plot, width = 9.5, height = 7.2, bg = "white")
print(summary, digits = 5)
