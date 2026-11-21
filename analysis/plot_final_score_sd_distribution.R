# Plot each stored area score in final-composite SD units, without refitting.
# Requires the maps report produced by build_threshold_map_report.py.
args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default = NULL) {
  i <- match(name, args)
  if (is.na(i)) {
    if (is.null(default)) stop("Supply ", name)
    return(default)
  }
  if (i == length(args)) stop("Supply a value for ", name)
  args[i + 1L]
}
script <- gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
input <- option("--input-report", file.path(root, "reports", "Demand_supply_threshold_maps.html"))
country <- option("--geography", "lagos")
output <- option("--output-dir")
for (pkg in c("jsonlite", "ggplot2", "svglite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
}
html <- paste(readLines(input, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
payload <- regmatches(html, regexpr('(?s)<script id="map-data" type="application/json">.*?</script>', html, perl = TRUE))
if (length(payload) != 1L || !nzchar(payload)) stop("Map scores not found")
payload <- sub('</script>$', '', sub('^<script[^>]*>', '', payload))
all_scores <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
if (!country %in% names(all_scores)) stop("Unknown --geography")
source <- all_scores[[country]]
rows <- source$areas
long <- do.call(rbind, lapply(names(rows), function(key) {
  r <- rows[[key]]
  result <- list()
  if (!is.null(r$demand_u)) result[[1]] <- data.frame(area = r$name, domain = "Demand concerns", score = r$demand_u)
  if (!is.null(r$supply_u)) result[[2]] <- data.frame(area = r$name, domain = "Supply constraints", score = r$supply_u)
  do.call(rbind, result)
}))
stopifnot(all(is.finite(long$score)))
long$domain <- factor(long$domain, levels = c("Demand concerns", "Supply constraints"))
left <- floor(min(long$score) * 2) / 2 - .25
right <- ceiling(max(long$score) * 2) / 2 + .25

# Deterministically separate overlapping dots vertically; horizontal scores stay exact.
stack <- function(x, min_dx = .11, min_dy = .14) {
  ordering <- order(x)
  y <- rep(NA_real_, length(x))
  slots <- c(0, as.vector(rbind(seq_len(100), -seq_len(100)))) * min_dy
  placed <- integer()
  for (i in ordering) {
    nearby <- placed[abs(x[placed] - x[i]) < min_dx]
    for (candidate in slots) {
      if (!length(nearby) || all(abs(y[nearby] - candidate) >= min_dy - 1e-10)) {
        y[i] <- candidate
        break
      }
    }
    placed <- c(placed, i)
  }
  y
}
long$y <- NA_real_
for (domain in levels(long$domain)) {
  ix <- which(long$domain == domain)
  long$y[ix] <- stack(long$score[ix])
}
stopifnot(all(is.finite(long$y)))
line_bottom <- min(-.35, min(long$y) - .1)
line_top <- max(.35, max(long$y) + .1)
stopifnot(sum(long$domain == "Demand concerns") == source$reference$demand_n,
          sum(long$domain == "Supply constraints") == source$reference$supply_n)
summaries <- do.call(rbind, lapply(split(long, long$domain), function(d) {
  data.frame(domain = d$domain[1], x = left, y = line_top + .32,
             label = paste0("Above average: ", sum(d$score > 1e-10), " / ", nrow(d),
                            "     ≥ +0.5 SD: ", sum(d$score >= .5), " / ", nrow(d),
                            "     ≥ +1 SD: ", sum(d$score >= 1), " / ", nrow(d),
                            "     ≥ +1.5 SD: ", sum(d$score >= 1.5), " / ", nrow(d)))
}))
thresholds <- data.frame(x = c(0, .5, 1, 1.5), label = c("Area average\n0", "+0.5 SD", "+1 SD", "+1.5 SD"),
                         line = c("Average", "Cutoff", "Cutoff", "Cutoff"))
plot <- ggplot2::ggplot(long, ggplot2::aes(x = score, y = y)) +
  ggplot2::annotate("segment", x = left, xend = right, y = 0, yend = 0, color = "#d9e3e8", linewidth = .35) +
  ggplot2::geom_segment(data = thresholds, ggplot2::aes(x = x, xend = x, y = line_bottom, yend = line_top, linetype = line),
                         inherit.aes = FALSE, color = "#718795", linewidth = .45, show.legend = FALSE) +
  ggplot2::geom_text(data = thresholds, ggplot2::aes(x = x, y = line_top + .1, label = label),
                     inherit.aes = FALSE, color = "#17374a", size = 3.6, lineheight = 1) +
  ggplot2::geom_point(ggplot2::aes(color = domain), size = 4.4, show.legend = FALSE) +
  ggplot2::geom_text(data = summaries, ggplot2::aes(x = x, y = y, label = label),
                     inherit.aes = FALSE, hjust = 0, color = "#536c7b", size = 3.6) +
  ggplot2::facet_wrap(~domain, ncol = 1) +
  ggplot2::scale_color_manual(values = c("Demand concerns" = "#047f8b", "Supply constraints" = "#cd7729")) +
  ggplot2::scale_linetype_manual(values = c("Average" = "solid", "Cutoff" = "dashed")) +
  ggplot2::scale_x_continuous(limits = c(left, right), breaks = sort(unique(c(seq(ceiling(left), floor(right)), .5, 1.5))),
                            expand = ggplot2::expansion(mult = .015)) +
  ggplot2::scale_y_continuous(limits = c(line_bottom - .05, line_top + .43), breaks = NULL) +
  ggplot2::labs(title = paste0(source$title, " · Distribution of area scores"),
                subtitle = "Each dot represents one area; higher values indicate more measured challenges",
                x = "Distance from the area average (standard deviations)", y = NULL) +
  ggplot2::theme_minimal(base_size = 12, base_family = "Arial") +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#17374a", size = 19),
                 plot.subtitle = ggplot2::element_text(color = "#536c7b", margin = ggplot2::margin(b = 14)),
                 strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 15, color = "#17374a"),
                 panel.grid = ggplot2::element_blank(), panel.spacing = grid::unit(1.7, "lines"),
                 axis.text.x = ggplot2::element_text(color = "#536c7b", size = 11),
                 axis.title.x = ggplot2::element_text(color = "#17374a", margin = ggplot2::margin(t = 13)),
                 plot.margin = ggplot2::margin(20, 24, 18, 20))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
base <- file.path(output, paste0(country, "_final_score_sd_distribution"))
ggplot2::ggsave(paste0(base, ".png"), plot, width = 11, height = 6.1, dpi = 180, bg = "white")
ggplot2::ggsave(paste0(base, ".svg"), plot, width = 11, height = 6.1, bg = "white")
print(summaries[c("domain", "label")], row.names = FALSE)
