# Department-only Senegal models; selected supply components and personal demand.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L] }
optional <- function(k,default) {i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
data_dir<-opt("--data-dir");out<-opt("--output-dir");score_method<-optional("--score-method","zscore")
pca_file<-optional("--pca-score-file","");stopifnot(score_method%in%c("zscore","pca"))
for(pkg in c("survey","clubSandwich","jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
script_dir<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
source(file.path(script_dir,"..","..","..","analysis","senegal","senegal_definitions.R"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
source(file.path(script_dir,"supply_definitions.R"))
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
favorable_supply<-senegal_favorable_supply(frame$supply)
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables;S<-p$supply_variables
report_hash<-unname(tools::md5sum(file.path(data_dir,"report_data.json")))
refs<-list()
zz<-sapply(D,function(k){ok<-d$need & is.finite(d[[k]]);mu<-weighted.mean(d[[k]][ok],d$weight[ok]);sd<-sqrt(weighted.mean((d[[k]][ok]-mu)^2,d$weight[ok]));stopifnot(sd>0)
 refs[[k]]<<-data.frame(variable=k,weighted_mean=mu,weighted_SD=sd,n_valid_need=sum(ok));(d[[k]]-mu)/sd})
d$demand_score<-rowMeans(zz)
write.csv(do.call(rbind,refs),file.path(out,"individual_demand_score_reference.csv"),row.names=FALSE)
zs<-sapply(S,function(k){ref<-p$component_trace[p$component_trace$side=="Supply" & p$component_trace$variable==k,]
 mu<-unique(ref$indicator_mean);sd<-unique(ref$indicator_SD);stopifnot(length(mu)==1,length(sd)==1,sd>0)
 (100*favorable_supply[[k]]-mu)/sd})
d$supply_risk<- -rowMeans(zs)
sref<-do.call(rbind,lapply(S,function(k){q<-p$component_trace[p$component_trace$side=="Supply"&p$component_trace$variable==k,]
 data.frame(variable=k,reference_mean_pct=unique(q$indicator_mean),reference_SD_pct=unique(q$indicator_SD),reference_departments=length(unique(q$geo)))}))
write.csv(sref,file.path(out,"supply_score_reference.csv"),row.names=FALSE)

if(score_method=="pca"){
 pp<-readRDS(pca_file);stopifnot(identical(pp$scores$row,d$row),pp$audit$report_md5==report_hash)
 d$demand_score<-pp$scores$demand_score;d$supply_risk<-pp$scores$supply_risk
}
resolution<-"department";demand_resolution<-"individual"
z<-p$scores[p$scores$method=="zscore",]
profiles<-unique(cbind(d[c("geo","district","source_profile","fallback")],frame$supply,supply_risk=d$supply_risk))
stopifnot(nrow(profiles)==33,!anyDuplicated(profiles$geo))
stopifnot(all(vapply(split(d$supply_risk,d$geo),function(v)length(unique(v))==1,logical(1))))
write.csv(profiles,file.path(out,"model_supply_profiles.csv"),row.names=FALSE,na="")
controls<-c("age10","age10_sq","education","partnered","fertility_goal","urban")
specs<-list(score_demand=c(controls,"demand_score"),supply=c(controls,"supply_risk"),
 score_joint=c(controls,"demand_score","supply_risk"),components_demand=c(controls,D),components_joint=c(controls,D,"supply_risk"))
positive<-d$positive
common_vars<-unique(c("y_modern",controls,D,"demand_score","supply_risk"))
d$eligible<-d$need & positive & complete.cases(d[common_vars]);cc<-droplevels(d[d$eligible,])
stopifnot(all(cc$y_modern%in%0:1),all(cc$need),length(unique(cc$geo))>=20,
 all(cc$urban%in%0:1),length(unique(cc$partnered))==2)
full_design<-survey::svydesign(ids=~psu,weights=~weight,data=d[positive,],nest=TRUE)
des<-subset(full_design,eligible)
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
fits <- coefficients <- samples <- tests <- diagnostics <- leaveout <- survey_tests <- list()
for (id in names(specs)) {
  fo <- reformulate(specs[[id]], "y_modern"); f <- fit_glm(fo, cc)
  V <- clubSandwich::vcovCR(f, cluster = cc$geo, type = "CR2", inverse_var = FALSE)
  sf <- survey::svyglm(fo, des, family = quasibinomial())
  point_error <- max(abs(coef(f) - coef(sf))); stopifnot(point_error < 1e-7)
  sm <- as.data.frame(summary(sf)$coefficients)
  sdf <- sf$df.residual
  survey_tests[[id]] <- data.frame(model = id, term = rownames(sm), estimate = sm[,1], se = sm[,2],
    df = sdf, lo = sm[,1] - qt(.975,sdf)*sm[,2], hi = sm[,1] + qt(.975,sdf)*sm[,2], p = sm[,4])
  a <- coef_table(f, V); a$model <- id; coefficients[[id]] <- a
  vv <- vifs(f, cc); vv$model <- id; diagnostics[[id]] <- vv
  for (block in c(if (id != "supply") "Demand", if (id %in% c("supply", "score_joint", "components_joint")) "Supply")) {
    fields <- if (block == "Supply") "supply_risk" else if (grepl("^score", id)) "demand_score" else D
    t <- block_test(f, V, fields); t$model <- id; t$block <- block
    tests[[length(tests) + 1L]] <- t
  }
  samples[[id]] <- data.frame(model = id, n = nrow(cc), events = sum(cc$y_modern),
    non_events = sum(cc$y_modern == 0), weighted_modern_use = weighted.mean(cc$y_modern, cc$weight),
    districts = length(unique(cc$geo)), parameters = length(coef(f)), design_rank = f$rank,
    max_vif = max(vv$vif), minimum_df = min(a$df[a$term != "(Intercept)"]),
    min_fitted = min(fitted(f)), max_fitted = max(fitted(f)), converged = f$converged,
    coefficient_error_vs_svyglm = point_error,
    warnings = paste(attr(f, "fit_warnings"), collapse = " | "))
  fields <- intersect(c("demand_score", "supply_risk", D), names(coef(f)))
  for (g in sort(unique(cc$geo))) {
    ff <- fit_glm(fo, cc[cc$geo != g, ])
    leaveout[[length(leaveout) + 1L]] <- data.frame(model = id, omitted_department = g, term = fields,
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
savecsv(leaveout, "leave_one_department_out.csv")
missing <- data.frame(variable = common_vars,
  missing_in_need = vapply(d[d$need, common_vars], function(a) sum(is.na(a)), integer(1)))
savecsv(missing, "model_missingness.csv")
savecsv(z[, c("geo", "demand_score", "supply_score")], "model_department_scores.csv")
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
  v <- if (k == "supply_risk") unique(cc[, c("geo", "residence", k)])[[k]] else unique(cc[[k]])
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

source(file.path(script_dir,"..","plot_models.R"))
labels<-render_modern_use_plots(coefficients,curves,gaps,cc,p$demand_labels,D,out,score_method,demand_resolution,resolution,"senegal",
 supply_pca_mixed=score_method=="pca" && length(pp$audit$negative_supply_PC1_loadings)>0)
influence<-do.call(rbind,lapply(split(leaveout,interaction(leaveout$model,leaveout$term,drop=TRUE)),function(a)
 data.frame(model=a$model[1],term=a$term[1],full_estimate=a$full_estimate[1],deletion_min=min(a$estimate),deletion_max=max(a$estimate),deletion_sign_changes=sum(sign(a$estimate)!=sign(a$full_estimate)))))
savecsv(influence,"influence_summary.csv");savecsv(bind(survey_tests),"sampling_cluster_model_sensitivity.csv")
saveRDS(fits,file.path(out,"fitted_models.rds"))
audit<-list(country="Senegal",outcome="Current modern contraceptive use only: confirmed modern=1; traditional-only or confirmed nonuse=0; unresolved status remains missing.",
 need_definition="Original SURGO FPN2=1, unchanged. No reconstructed need flag or permanent-protection additions.",
 need_n=sum(d$need),common_n=nrow(cc),common_events=sum(cc$y_modern),score_method=score_method,
 supply_components=length(S),counseling_included=FALSE,daily_provision_included=TRUE,stockout_burden_included=TRUE,
 demand_resolution="individual",supply_resolution="Department only: every woman receives the summary of all included facilities in her department",districts=length(unique(cc$geo)),
 survey_psus=length(unique(cc$psu)),demand_distinct_values=length(unique(cc$demand_score)),supply_distinct_values=length(unique(cc$supply_risk)),
 assigned_context_profiles=length(unique(cc$geo)),woman_residence_cells=nrow(unique(cc[c("geo","residence")])),
 fallback_model_women=sum(cc$fallback),fallback_need_women=sum(d$fallback&d$need),
 women_residence_control="FV_MILIEU, retained regardless of facility source level",
 controls=controls,omitted_controls="None of the agreed adjustment domains omitted; all model designs checked for full rank.",
 weights="Original Senegal WEIGHT; common mean-one fitting rescaling. No facility weights.",
 inference="Weighted logistic coefficients with department-clustered CR2 covariance, coefficient-specific Satterthwaite intervals, HTZ block tests and Holm adjustment within each joint model. No random effects or interactions.",
 design="Available sampling design: recorded RB0H census districts and final WEIGHT; original strata and FPC unavailable. Survey-PSU coefficient/interval sensitivity exported; point estimates identical to svyglm. Department CR2 primary for shared contextual supply exposures.",
 score_definition=if(score_method=="zscore")"Demand: mean of six personal binary indicators standardized with item-specific weighted mean/population SD in the need group. Supply: transform burden to continuity = 1 - burden, then negative mean of six favorable assigned department-wide measures standardized against fixed 33-department component references. Scores enter per one original average-z point." else "Separate weighted demand and supply PCAs; personal demand PC1 and negative assigned department-wide supply PC1. Mixed supply signs make it a pattern score, not a uniformly ordered risk index. Scores enter per one original PC1 unit.",
 supply_missing="All 33 departments have complete selected department-level supply inputs. No missing indicator is coded zero; no geographic fallback or residence-specific supply is used.",
 excluded_need=sum(d$need)-nrow(cc),eligible_weight_share=sum(cc$weight)/sum(d$weight[d$need]),
 excluded_departments=setdiff(unique(d$geo[d$need]),unique(cc$geo)),
 uncertainty="Intervals condition on estimated scores and do not propagate their construction uncertainty; full original sampling design is not available. Diagnostics are not a formal power analysis or causal identification.",
 probability_checks=list(maximum_direct_prediction_error=max_prediction_error,maximum_numeric_gradient_error=max_gradient_error),
 report_md5=report_hash,source_hashes=p$audit$source_md5)
payload<-list(audit=audit,coefficients=coefficients,samples=samples,tests=tests,vifs=diagnostics,curves=curves,gaps=gaps,influence=influence,labels=as.list(labels))
jsonlite::write_json(payload,file.path(out,"model_results.json"),dataframe="rows",auto_unbox=TRUE,digits=12,na="null")
jsonlite::write_json(audit,file.path(out,"model_audit.json"),auto_unbox=TRUE,pretty=TRUE,digits=12,na="null")
print(samples);print(tests);print(coefficients[coefficients$term%in%c("demand_score","supply_risk"),])
