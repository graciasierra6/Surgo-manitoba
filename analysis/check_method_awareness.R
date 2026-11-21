# Shared survey-weighted estimation and geographic regression inference.
.survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
.survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
while (!file.exists(file.path(.survey_root, "analysis/common/survey_methods.R"))) {
  .survey_parent <- dirname(.survey_root)
  if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
  .survey_root <- .survey_parent
}
source(file.path(.survey_root, "analysis/common/survey_methods.R"), local = TRUE)

# Separate awareness diagnostic; source joins and working reports remain unchanged.
args <- commandArgs(trailingOnly = TRUE)
arg <- function(k, default = NULL) {
  i <- match(k, args)
  if (is.na(i)) { if (is.null(default)) stop("Supply ", k); return(default) }
  if (i == length(args)) stop("Supply ", k)
  args[i + 1L]
}
if ("--help" %in% args) {
  cat("Rscript analysis/check_method_awareness.R --lagos-dir ANALYSIS --lagos-women-file JOIN --senegal-dir ANALYSIS --senegal-women-file JOIN [--output-dir output/method_awareness_check] [--report-dir reports] [--pandoc-dir DIRECTORY]\n")
  quit(status = 0)
}
for (pkg in c("haven", "survey", "clubSandwich", "jsonlite", "ggplot2", "rmarkdown", "base64enc"))
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
script <- gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
resolve <- function(p) if (grepl("^(/|[A-Za-z]:)", p)) p else file.path(root, p)
out <- resolve(arg("--output-dir", "output/method_awareness_check"))
report_dir <- resolve(arg("--report-dir", "reports"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
controls <- c("age10", "age10_sq", "education", "partnered", "fertility_goal", "urban")
other_demand <- c("agency", "stigma", "health", "fertility", "opposes")
method_names <- c("Condoms", "Injectables", "Daily pill", "Emergency pill", "IUD", "Implant", "Male sterilization", "Female sterilization")
ka1_order <- c(1, 2, 3, 8, 4, 5, 7, 6)
wm <- function(v, w) { ok <- is.finite(v) & is.finite(w) & w > 0; survey_weighted_mean(v[ok], w[ok]) }
wcor <- function(x, y, w) {
  ok <- complete.cases(x, y, w); x <- x[ok]; y <- y[ok]; w <- w[ok]
  mx <- wm(x, w); my <- wm(y, w)
  wm((x - mx) * (y - my), w) / sqrt(wm((x - mx)^2, w) * wm((y - my)^2, w))
}
fit_model <- function(fo, d) fit_survey_logistic(fo, d)
coef_table <- function(f, V) {
  z <- as.data.frame(clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite"))
  data.frame(term = rownames(z), estimate = z$beta, se = z$SE, df = z$df_Satt, p = z$p_Satt,
    odds_ratio = exp(z$beta), lo = exp(z$beta - qt(.975, z$df_Satt) * z$SE), hi = exp(z$beta + qt(.975, z$df_Satt) * z$SE), row.names = NULL)
}
vifs <- function(f, d) {
  X <- model.matrix(f)[, -1, drop = FALSE]
  data.frame(term = colnames(X), vif = vapply(seq_len(ncol(X)), function(j) {
    a <- lm.wfit(cbind(1, X[, -j, drop = FALSE]), X[, j], w = d$weight)
    total <- sum(d$weight * (X[, j] - wm(X[, j], d$weight))^2)
    total / sum(d$weight * a$residuals^2)
  }, numeric(1)))
}
margin <- function(f, V, d, changes) {
  a <- d
  for (k in names(changes)) a[[k]] <- changes[[k]]
  X <- model.matrix(delete.response(terms(f)), a, contrasts.arg = f$contrasts, xlev = f$xlevels)[, names(coef(f)), drop = FALSE]
  pp <- plogis(drop(X %*% coef(f))); w <- a$weight / sum(a$weight)
  estimate <- survey_weighted_mean(pp, a$weight); gradient <- colSums(X * (w * pp * (1 - pp)))
  direct <- survey_weighted_mean(predict(f, a, type = "response"), w)
  stopifnot(abs(estimate - direct) < 1e-10)
  list(estimate = estimate, gradient = gradient)
}
contrast_ci <- function(f, V, estimate, gradient) {
  G <- matrix(gradient, nrow = 1)
  z <- as.data.frame(clubSandwich::linear_contrast(f, vcov = V, contrasts = G, test = "Satterthwaite"))
  df <- as.numeric(z[[4]][1]); se <- sqrt(max(0, drop(G %*% V %*% t(G))))
  data.frame(estimate = estimate, se = se, df = df, lo = estimate - qt(.975, df) * se, hi = estimate + qt(.975, df) * se)
}
results <- list(); all_hashes <- list()
for (country in c("lagos", "senegal")) {
  data_dir <- resolve(arg(paste0("--", country, "-dir")))
  women_file <- resolve(arg(paste0("--", country, "-women-file")))
  source_files <- c(file.path(data_dir, c("analysis_frame.rds", "report_data.json")), women_file)
  hashes <- unname(tools::md5sum(source_files))
  frame <- readRDS(source_files[1]); d <- frame$women
  p <- jsonlite::fromJSON(source_files[2]); x <- readRDS(women_file); names(x) <- tolower(names(x))
  nv <- function(v) as.numeric(haven::zap_labels(v))
  stopifnot(nrow(d) == nrow(x), identical(d$row, seq_len(nrow(x))), all(d$need <= d$positive),
    all(d$need == (d$positive & nv(x$fpn2) %in% 1)))
  raw_weight <- nv(x$weight); stopifnot(max(abs(raw_weight - d$weight), na.rm = TRUE) < 1e-12)
  a <- sapply(ka1_order, function(k) nv(x[[paste0("ka1_", k)]]))
  b <- sapply(letters[1:8], function(k) nv(x[[paste0("ka2", k)]]))
  all_named <- sapply(1:15, function(k) nv(x[[paste0("ka1_", k)]]))
  contradiction <- nv(x$ka1_16) %in% 1 & rowSums(all_named == 1, na.rm = TRUE) > 0
  recognized <- matrix(NA_real_, nrow(d), 8)
  for (j in 1:8) recognized[, j] <- ifelse(a[, j] %in% 1 | b[, j] %in% 1:2, 1,
    ifelse(a[, j] %in% 0 & b[, j] %in% 3, 0, NA_real_))
  raw_count <- rowSums(recognized)
  raw_recall <- ifelse(apply(a, 1, function(v) all(v %in% 0:1)), rowSums(a), NA_real_)
  d$awareness_count <- raw_count; d$recall_count <- raw_recall
  d$awareness_count[contradiction] <- NA_real_; d$recall_count[contradiction] <- NA_real_
  d$conflict <- contradiction
  # Existing MC5 coding is checked independently rather than relabeled as knowledge.
  mc <- sapply(c("mc5a", "mc5b"), function(k) { v <- nv(x[[k]]); ifelse(v %in% 1:4, as.numeric(v %in% 3:4), NA_real_) })
  mc_flag <- ifelse(rowSums(mc == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(mc)) > 0, NA_real_, 0))
  stopifnot(identical(as.numeric(mc_flag), as.numeric(d$confidence)))
  # Retain the exact assigned residence/fallback supply score on the risk scale.
  source(file.path(root, "analysis/common/risk_scores.R"))
  d$supply_risk <- supply_constraint_score(frame$supply, p$component_trace)
  d$eligible_description <- d$need
  full <- surgo_design(d[d$positive, ])
  need <- subset(full, eligible_description)
  prevalence <- function(v) {
    aa <- need; aa$variables$.v <- v[d$positive][d$eligible_description[d$positive]]
    aa <- subset(aa, is.finite(.v))
    if (!nrow(aa$variables)) return(c(estimate = NA, lo = NA, hi = NA))
    m <- survey::svymean(~.v, aa); se <- sqrt(vcov(m)[1, 1]); q <- qt(.975, survey::degf(aa))
    c(estimate = unname(coef(m)[1]), lo = unname(coef(m)[1]) - q * se, hi = unname(coef(m)[1]) + q * se)
  }
  summary <- list(country = if (country == "lagos") "Lagos" else "Senegal", need_n = sum(d$need),
    awareness_valid = sum(d$need & is.finite(d$awareness_count)), confidence_valid = sum(d$need & is.finite(d$confidence)),
    contradictions = sum(d$need & contradiction), raw_count_unresolved = sum(d$need & is.na(raw_count)),
    skipped_unnamed_method_cells = sum(a == 0 & b == 95 & d$need, na.rm = TRUE),
    named_but_prompted_cells = sum(a == 1 & b != 95 & d$need, na.rm = TRUE),
    weighted_awareness_mean = prevalence(d$awareness_count), weighted_recall_mean = prevalence(d$recall_count),
    weighted_confidence_pct = 100 * wm(d$confidence[d$need], d$weight[d$need]),
    awareness_confidence_correlation = wcor(d$awareness_count[d$need], d$confidence[d$need], d$weight[d$need]),
    descriptive_psus = length(unique(d$psu[d$need])), geographic_clusters = length(unique(d$geo[d$need])))
  dist <- do.call(rbind, lapply(c("awareness_count", "recall_count"), function(k) do.call(rbind, lapply(0:8, function(v) {
    valid <- d$need & is.finite(d[[k]]); vv <- ifelse(is.finite(d[[k]]), as.numeric(d[[k]] == v), NA_real_)
    e <- prevalence(vv); data.frame(country = summary$country, measure = k, count = v, n = sum(d[[k]][valid] == v), denominator = sum(valid), pct = 100 * e[1], lo = max(0, 100 * e[2]), hi = min(100, 100 * e[3]))
  }))))
  method_distribution <- do.call(rbind, lapply(1:8, function(j) {
    v <- recognized[, j]; v[contradiction] <- NA_real_; e <- prevalence(v)
    data.frame(country = summary$country, method = method_names[j], n_valid = sum(d$need & is.finite(v)), pct = 100 * e[1], lo = max(0, 100 * e[2]), hi = min(100, 100 * e[3]))
  }))
  overlap <- do.call(rbind, lapply(0:8, function(v) {
    ii <- d$need & d$awareness_count %in% v & is.finite(d$confidence)
    data.frame(country = summary$country, count = v, n = sum(ii), low_confidence_n = sum(d$confidence[ii] == 1),
      low_confidence_pct = 100 * wm(d$confidence[ii], d$weight[ii]), weighted_sample_pct = 100 * wm(as.numeric(ii[d$need & complete.cases(d[c("awareness_count", "confidence")])]), d$weight[d$need & complete.cases(d[c("awareness_count", "confidence")])]))
  }))
  mean_by_confidence <- do.call(rbind, lapply(0:1, function(v) {
    vv <- d$awareness_count; vv[!d$confidence %in% v] <- NA_real_; e <- prevalence(vv)
    data.frame(country = summary$country, confidence = v, n = sum(d$need & d$confidence %in% v & is.finite(vv)), mean = e[1], lo = e[2], hi = e[3])
  }))
  base <- c(controls, other_demand, "supply_risk")
  specs <- list(base = base, confidence = c(base, "confidence"), awareness = c(base, "awareness_count"), both = c(base, "confidence", "awareness_count"))
  all_vars <- unique(c("y_modern", unlist(specs)))
  d$eligible <- d$need & complete.cases(d[all_vars]); cc <- droplevels(d[d$eligible, ])
  des <- subset(full, eligible_description)
  # Re-create after adding the common-sample flag to preserve need-domain and common-sample selection.
  full_model <- surgo_design(d[d$positive, ])
  des <- subset(full_model, eligible)
  coefficients <- diagnostics <- cv <- samples <- tests <- fits <- list()
  for (id in names(specs)) {
    fo <- reformulate(specs[[id]], "y_modern"); f <- fit_survey_logistic(fo, cc, design=des)
    V <- geographic_cr2(f, cc$geo)
    ct <- coef_table(f, V); ct$model <- id; ct$country <- summary$country; coefficients[[id]] <- ct
    vi <- vifs(f, cc); vi$model <- id; vi$country <- summary$country; diagnostics[[id]] <- vi
    samples[[id]] <- data.frame(country = summary$country, model = id, n = nrow(cc), events = sum(cc$y_modern), clusters = length(unique(cc$geo)), parameters = length(coef(f)), max_vif = max(vi$vif), warnings = paste(attr(f, "fit_warnings"), collapse = " | "))
    for (k in intersect(c("confidence", "awareness_count"), names(coef(f)))) {
      K <- diag(length(coef(f)))[match(k, names(coef(f))), , drop = FALSE]
      tt <- clubSandwich::Wald_test(f, constraints = K, vcov = V, test = "HTZ")
      tests[[paste(id, k)]] <- data.frame(country = summary$country, model = id, term = k, F = tt$Fstat, df_num = tt$df_num, df_denom = tt$df_denom, p = tt$p_val)
    }
    pred <- rep(NA_real_, nrow(cc))
    for (geo in unique(cc$geo)) {
      train <- cc[cc$geo != geo, ]; test <- cc[cc$geo == geo, ]; ff <- fit_model(fo, train)
      pred[cc$geo == geo] <- predict(ff, newdata = test, type = "response")
    }
    stopifnot(all(is.finite(pred)), all(pred >= 0 & pred <= 1)); clipped <- pmax(1e-8, pmin(1 - 1e-8, pred))
    cv[[id]] <- data.frame(country = summary$country, model = id, brier = wm((cc$y_modern - pred)^2, cc$weight), log_loss = wm(-cc$y_modern * log(clipped) - (1 - cc$y_modern) * log(1 - clipped), cc$weight))
    fits[[id]] <- list(fit = f, V = V)
  }
  ob <- fits$both; gaps <- curves <- list()
  for (k in c("confidence", "awareness_count")) {
    if (k == "confidence") { lo <- margin(ob$fit, ob$V, cc, list(confidence = 0)); hi <- margin(ob$fit, ob$V, cc, list(confidence = 1)) } else {
      lo <- margin(ob$fit, ob$V, cc, list(awareness_count = cc$awareness_count))
      # One extra recognized method among women not already at the ceiling.
      aa <- cc[cc$awareness_count < 8, ]; lo <- margin(ob$fit, ob$V, aa, list(awareness_count = aa$awareness_count))
      hi <- margin(ob$fit, ob$V, aa, list(awareness_count = aa$awareness_count + 1))
    }
    tt <- contrast_ci(ob$fit, ob$V, hi$estimate - lo$estimate, hi$gradient - lo$gradient)
    tt$term <- k; tt$country <- summary$country; tt$probability_from <- lo$estimate; tt$probability_to <- hi$estimate; tt$n_standardized <- if(k == "confidence") nrow(cc) else nrow(aa)
    gaps[[k]] <- tt
  }
  for (v in sort(unique(cc$awareness_count))) for (c in 0:1) {
    a1 <- margin(ob$fit, ob$V, cc, list(awareness_count = v, confidence = c))
    tt <- contrast_ci(ob$fit, ob$V, a1$estimate, a1$gradient); tt$lo <- max(0, tt$lo); tt$hi <- min(1, tt$hi)
    tt$count <- v; tt$confidence <- c; tt$country <- summary$country
    tt$observed_cell_n <- sum(cc$awareness_count == v & cc$confidence == c); curves[[length(curves) + 1L]] <- tt
  }
  sensitivity <- list()
  # Check the spontaneous count and whether the five contradictory Senegal rows matter.
  for (check in c("recall_count", "retain_named_conflicts", "quadratic_awareness")) {
    dd <- d; fo <- reformulate(c(base, "confidence", "awareness_count"), "y_modern")
    if (check == "recall_count") dd$awareness_count <- dd$recall_count
    if (check == "retain_named_conflicts") dd$awareness_count <- raw_count
    if (check == "quadratic_awareness") { dd$awareness_sq <- dd$awareness_count^2; fo <- reformulate(c(base, "confidence", "awareness_count", "awareness_sq"), "y_modern") }
    ii <- dd$need & complete.cases(dd[unique(c("y_modern", all.vars(fo)))]); aa <- droplevels(dd[ii, ])
    ff <- fit_model(fo, aa); VV <- geographic_cr2(ff, aa$geo)
    ct <- coef_table(ff, VV); ct <- ct[ct$term %in% c("confidence", "awareness_count", "awareness_sq"), ]
    ct$check <- check; ct$n <- nrow(aa); ct$country <- summary$country; sensitivity[[check]] <- ct
  }
  summary$model_n <- nrow(cc); summary$model_events <- sum(cc$y_modern); summary$model_excluded <- sum(d$need) - nrow(cc)
  summary$model_awareness_confidence_correlation <- wcor(cc$awareness_count, cc$confidence, cc$weight)
  bind <- function(z) do.call(rbind, z)
  result <- list(summary = summary, distribution = dist, methods = method_distribution, overlap = overlap,
    means = mean_by_confidence, coefficients = bind(coefficients), vifs = bind(diagnostics), tests = bind(tests),
    samples = bind(samples), cv = bind(cv), gaps = bind(gaps), curves = bind(curves), sensitivity = bind(sensitivity))
  for (k in names(result)[-1]) write.csv(result[[k]], file.path(out, paste(country, k, "csv", sep = ".")), row.names = FALSE, na = "")
  stopifnot(identical(hashes, unname(tools::md5sum(source_files))))
  result$summary$input_hashes <- as.list(setNames(hashes, c("analysis_frame", "report_data", "woman_join")))
  result$summary$main_sources_unchanged <- TRUE
  results[[country]] <- result
  cat(summary$country, ": need N=", summary$need_n, "; awareness valid=", summary$awareness_valid,
    "; common model N=", summary$model_n, "; correlation=", round(summary$awareness_confidence_correlation, 3), "\n", sep = "")
}
joint_tests <- do.call(rbind, lapply(results, function(r) r$tests[r$tests$model == "both", ]))
joint_tests$holm_p_four_tests <- p.adjust(joint_tests$p, "holm")
write.csv(joint_tests, file.path(out, "joint_model_tests.csv"), row.names = FALSE)
summary <- lapply(results, `[[`, "summary")
jsonlite::write_json(summary, file.path(out, "verification.json"), auto_unbox = TRUE, pretty = TRUE, digits = 12)
saveRDS(results, file.path(out, "aggregate_results.rds"))
plot_theme <- ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#173d4d"),
  panel.grid.minor = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold"), legend.position = "bottom")
cols <- c(Lagos = "#087d8a", Senegal = "#ba6634")
save_plot <- function(plot, name, height = 5.1) ggplot2::ggsave(file.path(out, name), plot, width = 11, height = height, dpi = 180, bg = "white")
dist <- do.call(rbind, lapply(results, `[[`, "distribution")); dist <- dist[dist$measure == "awareness_count", ]
g <- ggplot2::ggplot(dist, ggplot2::aes(count, pct, fill = country)) + ggplot2::geom_col(width = .72, show.legend = FALSE) +
  ggplot2::facet_wrap(~country) + ggplot2::scale_fill_manual(values = cols) + ggplot2::scale_x_continuous(breaks = 0:8) +
  ggplot2::labs(title = "Women recognize different numbers of modern methods", subtitle = "Spontaneous naming plus picture recognition; before an explanation", x = "Modern methods recognized (0–8)", y = "Weighted share of women in need (%)") + plot_theme
save_plot(g, "awareness_distribution.png")
means <- do.call(rbind, lapply(results, `[[`, "means")); means$group <- ifelse(means$confidence == 1, "Low confidence", "Somewhat / very confident on both items")
g <- ggplot2::ggplot(means, ggplot2::aes(mean, group, color = country)) + ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), orientation = "y", width = .16, linewidth = .7) +
  ggplot2::geom_point(size = 3.5) + ggplot2::facet_wrap(~country) + ggplot2::scale_color_manual(values = cols) +
  ggplot2::labs(title = "Awareness and confidence are related, but distinct", subtitle = "Survey-weighted mean recognized methods and 95% confidence intervals", x = "Modern methods recognized (0–8)", y = NULL, color = NULL) + plot_theme + ggplot2::theme(legend.position = "none")
save_plot(g, "awareness_confidence_overlap.png", 3.8)
ct <- do.call(rbind, lapply(results, `[[`, "coefficients")); ct <- ct[ct$model %in% c("confidence", "awareness", "both") & ct$term %in% c("confidence", "awareness_count"), ]
ct$label <- ifelse(ct$term == "confidence", "Low confidence (yes vs no)", "One additional method recognized")
ct$spec <- factor(ct$model, levels = c("confidence", "awareness", "both"), labels = c("Confidence only", "Awareness only", "Both included"))
pd <- ggplot2::position_dodge(width = .55)
g <- ggplot2::ggplot(ct, ggplot2::aes(odds_ratio, label, color = spec)) + ggplot2::geom_vline(xintercept = 1, color = "#879aa4", linetype = 2) +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), orientation = "y", width = .14, position = pd) + ggplot2::geom_point(position = pd, size = 2.8) +
  ggplot2::facet_wrap(~country) + ggplot2::scale_x_log10(breaks = c(.5, .75, 1, 1.25, 1.5), labels = c("0.50", "0.75", "1.00", "1.25", "1.50")) + ggplot2::scale_color_manual(values = c("#8b939c", "#ba6634", "#087d8a")) +
  ggplot2::labs(title = "Adjusted associations with modern contraceptive use", subtitle = "All comparisons use the same women within each setting; geographic CR2 95% intervals", x = "Adjusted odds ratio (log scale)", y = NULL, color = NULL) + plot_theme
save_plot(g, "awareness_adjusted_associations.png", 4.4)
prepared <- normalizePath(out, mustWork = TRUE); report_dir <- normalizePath(report_dir, mustWork = TRUE)
pandoc <- arg("--pandoc-dir", Sys.getenv("RSTUDIO_PANDOC", "")); if (nzchar(pandoc)) Sys.setenv(RSTUDIO_PANDOC = pandoc)
rmarkdown::render(file.path(root, "analysis/check_method_awareness.Rmd"), output_file = "Method_awareness_check_Senegal_Lagos.html", output_dir = report_dir,
  params = list(prepared_dir = prepared), quiet = TRUE, envir = new.env(parent = globalenv()))
cat("Saved separate Method_awareness_check_Senegal_Lagos.html; main reports were not modified.\n")
