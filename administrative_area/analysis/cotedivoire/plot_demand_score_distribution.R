# Plot the saved department-level demand index without refitting any analysis.
# All input and output locations are supplied by the caller.
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(key) {
  pos <- match(key, args)
  if (is.na(pos) || pos == length(args)) stop("Supply ", key)
  args[pos + 1L]
}
data_dir <- arg_value("--data-dir")
output_dir <- arg_value("--output-dir")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(ggplot2))
report <- jsonlite::fromJSON(file.path(data_dir, "report_data.json"))
scores <- report$scores[report$scores$method == "zscore", ]
stopifnot(nrow(scores) == 82L, !anyNA(scores$demand_score))
x <- scores$demand_score
mu <- mean(x)
med <- median(x)
quartiles <- as.numeric(quantile(x, c(.25, .75), type = 7))
summary <- list(
  departments = length(x), mean = mu, median = med,
  SD = sd(x), minimum = min(x), q25 = quartiles[1],
  q75 = quartiles[2], maximum = max(x),
  above_mean = sum(x > mu), below_mean = sum(x < mu),
  above_median = sum(x > med), below_median = sum(x < med),
  same_mean_and_median_flags = identical(x > mu, x > med),
  interpretation = paste(
    "Equal-weight average of six concern z-scores across departments.",
    "Underlying prevalences use final woman weights and valid-domain responses.",
    "Departments contribute equally to normalization and this distribution.",
    "The mean is zero by construction; the composite SD need not equal one.",
    "Classification uses the median, not an absolute clinical threshold."
  )
)
stopifnot(abs(mu) < 1e-10)

reference <- data.frame(
  cutoff = c(mu, med),
  reference = factor(c("Mean: 0.000", sprintf("Median: %.3f", med)),
                     levels = c("Mean: 0.000", sprintf("Median: %.3f", med)))
)
plot <- ggplot(scores, aes(x = demand_score)) +
  geom_histogram(binwidth = .2, boundary = 0, fill = "#087d89",
                 color = "white", linewidth = .7, closed = "left") +
  geom_rug(sides = "b", alpha = .5, linewidth = .4, color = "#254957") +
  geom_vline(data = reference, aes(xintercept = cutoff, color = reference,
                                  linetype = reference), linewidth = .85) +
  scale_color_manual(values = c("#183d52", "#c77526")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, .5),
                     expand = expansion(mult = c(.01, .01))) +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  scale_y_continuous(breaks = function(limits) pretty(limits, n = 6),
                     expand = expansion(mult = c(0, .07))) +
  labs(
    title = "Demand concerns vary across departments",
    subtitle = paste0("Côte d’Ivoire · 82 surveyed departments · ",
                      sum(x < mu), " below the mean; ", sum(x > mu), " above"),
    x = "Department demand score · average of six component z-scores",
    y = "Number of departments", color = NULL, linetype = NULL,
    caption = paste(
      "Mean = 0.000 by construction; median = −0.017. The two reference lines almost coincide.",
      "Higher scores mean more reported concerns overall, relative to other surveyed departments.",
      "Each rug mark represents one department. Scores use weighted concern rates among women with SURGO FPN2 = 1.",
      "Departments have equal weight here. Small local samples can make scores imprecise.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12.5, base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e4ecef", linewidth = .4),
    plot.title = element_text(size = 22, face = "bold", color = "#183d52",
                              margin = margin(b = 9)),
    plot.subtitle = element_text(size = 12.5, color = "#526e7b", margin = margin(b = 18)),
    axis.text = element_text(color = "#365563"),
    axis.title = element_text(color = "#254957"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.caption = element_text(hjust = 0, size = 9.5, lineheight = 1.25,
                                color = "#5b7480", margin = margin(t = 14)),
    legend.position = "bottom", legend.justification = "left",
    plot.margin = margin(22, 25, 18, 20)
  )

prefix <- "Cote_dIvoire_demand_score_distribution"
ggsave(file.path(output_dir, paste0(prefix, ".png")), plot,
       width = 11, height = 7.1, dpi = 180, bg = "white")
ggsave(file.path(output_dir, paste0(prefix, ".svg")), plot,
       width = 11, height = 7.1, device = svglite::svglite, bg = "white")
write.csv(scores[, c("geo", "demand_score", "category")],
          file.path(output_dir, paste0(prefix, ".csv")), row.names = FALSE)
jsonlite::write_json(summary, file.path(output_dir, paste0(prefix, "_summary.json")),
                     auto_unbox = TRUE, pretty = TRUE, digits = 15)
cat("Saved department demand-score distribution and summary statistics.\n")
