# Separate weighted individual-demand and department-supply PCA sensitivity.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
data_dir<-opt("--data-dir");out<-opt("--output-dir")
for(pkg in c("jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
report_file<-file.path(data_dir,"report_data.json");report<-jsonlite::fromJSON(report_file)
report_hash<-unname(tools::md5sum(report_file));frame<-readRDS(file.path(data_dir,"analysis_frame.rds"))
script_dir<-dirname(gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE))
source(file.path(script_dir,"..","..","..","analysis","cotedivoire","cdi_definitions.R"))
source(file.path(script_dir,"supply_definitions.R"))
d<-frame$women;D<-report$demand_variables;S<-report$supply_variables
eligible<-d$need;dm<-as.matrix(d[D]);sm<-as.matrix(cdi_favorable_supply(frame$supply[S]))
di<-eligible & complete.cases(dm);si<-eligible & complete.cases(sm)
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

fd<-weighted_pca(dm[di,,drop=FALSE],d$weight[di]);fs<-weighted_pca(sm[si,,drop=FALSE],d$weight[si])
scores<-data.frame(row=d$row,demand_score=project(fd,dm)[,1],supply_risk=-project(fs,sm)[,1])
stopifnot(all(is.finite(scores$demand_score[di])),all(is.finite(scores$supply_risk[si])))
key<-d$source_profile
profiles<-unique(data.frame(profile=key,geo=d$geo,sm,supply_PC1=-scores$supply_risk,supply_risk=scores$supply_risk))
stopifnot(!anyDuplicated(profiles$profile));profiles<-profiles[complete.cases(profiles[S]),]
profiles$women_in_need<-as.integer(table(factor(key[si],levels=profiles$profile)))
profiles$SURGO_weight<-vapply(profiles$profile,function(k)sum(d$weight[si & key==k]),numeric(1))
profiles<-profiles[profiles$SURGO_weight>0,]
profiles$weight_share<-profiles$SURGO_weight/sum(profiles$SURGO_weight)
collapsed<-weighted_pca(as.matrix(profiles[S]),profiles$SURGO_weight,fs$rotation[,1])
collapse_error<-max(abs(collapsed$rotation[,1]-fs$rotation[,1]),abs(collapsed$center-fs$center),abs(collapsed$scale-fs$scale))
stopifnot(collapse_error<1e-10)
savecsv<-function(a,name)write.csv(a,file.path(out,name),row.names=FALSE,na="")
savecsv(profiles,"pca_matched_supply_profiles.csv")
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
  stability[[length(stability) + 1L]] <- data.frame(side = side, omitted_department = g,
    loading_cosine = sum(f$rotation[, 1] * fit$rotation[, 1]),
    score_rank_correlation = cor(full_score, alt_score, method = "spearman"),
    pc1_variance_pct = 100 * f$eigenvalues[1] / sum(f$eigenvalues))
}
stability <- do.call(rbind, stability); savecsv(stability, "pca_leave_one_department_out.csv")
pc1 <- loads[loads$PC == 1, ]
audit<-list(method="Separate final-weighted correlation PCAs on individual demand and assigned department-wide supply; PC1 prespecified as sensitivity.",
 need_n=sum(eligible),demand_PCA_n=sum(di),supply_PCA_women_n=sum(si),supply_profiles=nrow(profiles),
 demand_PC1_variance=variance$variance_pct[variance$side=="Demand" & variance$PC==1],
 supply_PC1_variance=variance$variance_pct[variance$side=="Supply" & variance$PC==1],
 negative_demand_PC1_loadings=pc1$variable[pc1$side=="Demand" & pc1$loading<0],
 negative_supply_PC1_loadings=pc1$variable[pc1$side=="Supply" & pc1$loading<0],
 supply_collapse_equivalence_error=collapse_error,report_md5=report_hash,
 weighting="SURGO WEIGHT on both sides. Supply PCA is equivalent to assigned department profiles weighted by summed weights of represented women; these are not facility weights.",
 missingness="Require all six indicators for each side; no zero coding or partial scores. Exact department-wide assignment only; missing supply and demand stay missing, with no zero filling or fallback.",
 alignment="Before PCA, burden becomes continuity = 1 - burden so every supply input has favorable direction.",
 sign="Demand PC1 oriented toward average concern; supply PC1 toward average favorable provision and negated for modeling. Mixed signs remain and are explicitly reported.",
 uncertainty="Permutation and department-deletion checks assess exploratory construction stability, not validated survey-design inference. Regression intervals condition on estimated scores.",
 source_unchanged=report_hash==unname(tools::md5sum(report_file)))
obj<-list(audit=audit,scores=scores,demand_fit=fd,supply_fit=fs,profiles=profiles,loadings=loads,variance=variance,moments=moments,stability=stability)
saveRDS(obj,file.path(out,"pca_scores.rds"))
jsonlite::write_json(obj[setdiff(names(obj),c("scores","demand_fit","supply_fit"))],file.path(out,"pca_results.json"),dataframe="rows",auto_unbox=TRUE,na="null",digits=12)
jsonlite::write_json(audit,file.path(out,"pca_audit.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
labels <- setNames(c(report$demand_labels, report$supply_labels), c(D, S))
labels["stockout_burden"]<-"Stock continuity (1 - stockout burden)"
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
  sprintf("Supply PC1 pattern · %.1f%% of variation", audit$supply_PC1_variance))
pc1$panel <- factor(pc1$panel, levels = c(sprintf("Individual demand PC1 · %.1f%% of variation", audit$demand_PC1_variance),
  sprintf("Supply PC1 pattern · %.1f%% of variation", audit$supply_PC1_variance)))
g <- ggplot2::ggplot(pc1, ggplot2::aes(loading, row, fill = direction)) +
  ggplot2::geom_vline(xintercept = 0, color = "#8da2ad") + ggplot2::geom_col(width = .55) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.2f", loading), hjust = ifelse(loading < 0, 1.15, -.15)), size = 3.5) +
  ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") + ggplot2::scale_y_discrete(labels = labels) +
  ggplot2::scale_fill_manual(values = c(Demand = "#087f8a", Supply = "#be6b23", "Negative coefficient" = "#8b99a3")) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .2)) +
  ggplot2::labs(title = "What the PCA scores emphasize", subtitle = "Signed coefficients on standardized inputs; demand uses individual responses, supply uses matched department-wide exposures",
    x = "PC1 scoring coefficient (not an association with contraceptive use)", y = NULL, fill = NULL,
    caption = "Supply coefficients here describe oriented PC1; the model uses its negative. Inspect signed coefficients: a negative value indicates a contrast within PC1.\nWeights describe variation in the inputs, not program importance or causal effects.") + base
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
