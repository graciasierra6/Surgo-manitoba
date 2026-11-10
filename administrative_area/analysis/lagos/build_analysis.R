# Data for the standalone Lagos analysis: supplied FPN2, LGA planning summaries.
# All locations are runtime parameters. No source data or historical report changes.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k, args); if (is.na(i) || i == length(args)) stop("Supply ", k); args[i + 1L] }
optional <- function(k) { i <- match(k, args); if (is.na(i)) return(NULL); if (i == length(args)) stop("Supply a value for ", k); args[i + 1L] }
input <- opt("--input"); facility_input <- opt("--facility-input"); out <- opt("--output-dir"); raw_facility <- opt("--raw-facility")
psu_var <- optional("--psu-var"); strata_var <- optional("--strata-var")
facility_sample <- optional("--facility-sample"); if (is.null(facility_sample)) facility_sample <- "all"
demand_sample <- optional("--demand-sample"); if (is.null(demand_sample)) demand_sample <- "available"
reference_dir <- optional("--reference-dir"); if (is.null(reference_dir)) reference_dir <- ""
stopifnot(facility_sample == "all", demand_sample == "available", reference_dir == "")
if (!is.null(strata_var) && is.null(psu_var)) stop("--strata-var requires a confirmed --psu-var")
for (pkg in c("survey", "jsonlite")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
hash_before <- tools::md5sum(c(input, facility_input))
x <- readRDS(input); fac <- readRDS(facility_input); fac <- fac[fac$country %in% "lagos", ]
nv <- function(k) as.numeric(x[[k]])
key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
wm <- function(v, w) { ok <- is.finite(v) & is.finite(w) & w > 0; if (any(ok)) weighted.mean(v[ok], w[ok]) else NA_real_ }
mean_valid <- function(v) { v <- as.numeric(v); if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_ }
bool <- function(v) { a <- tolower(as.character(v)); ifelse(a %in% c("true", "1", "yes"), 1, ifelse(a %in% c("false", "0", "no"), 0, NA_real_)) }
flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
any_concern <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
D <- c("agency", "stigma", "health", "fertility", "confidence", "opposes")
DL <- c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs", "Future-fertility concerns", "Low method knowledge", "Opposes family planning")
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- dirname(gsub("~+~", " ", sub("^--file=", "", script_arg[1]), fixed=TRUE))
source(file.path(script_dir, "..", "..", "..", "analysis", "lagos", "lagos_definitions.R"))
source(file.path(script_dir,"supply_definitions.R"))
domains<-lagos_domains();S<-domains$S;SL<-domains$SL
stopifnot(nrow(x) == 4555, !anyDuplicated(x$uid), all(x$link_has_supply))
x$link_rurb <- as.character(x$settlement)
d <- lagos_derive_women(x)
pb2a<-as.numeric(x$pb2a);d$health<-ifelse(pb2a%in%1:4,as.numeric(pb2a%in%1:2),NA_real_)
d$id <- as.character(x$uid); d$district <- d$lga
# LGA is used only as the documented geographic inference cluster, not a survey EA.
d$positive <- is.finite(d$weight) & d$weight > 0
d$psu <- d$geo; d$fallback <- FALSE; d$source_profile <- d$geo
valid_weight <- is.finite(d$weight) & d$weight > 0
stopifnot(all(valid_weight[d$need]), sum(d$need) == 2159)
options(survey.lonely.psu = "adjust")
# Supplied weights identify point estimates. Do not infer official PSUs or strata
# from household-ID digits or probability values. Intervals are enabled only when
# confirmed design fields are explicitly supplied at run time.
design_confirmed <- !is.null(psu_var)
if (design_confirmed) {
  stopifnot(psu_var %in% names(x))
  d$.psu <- as.character(x[[psu_var]])
  stopifnot(all(!is.na(d$.psu[valid_weight]) & nzchar(d$.psu[valid_weight])))
  strata_formula <- NULL
  if (!is.null(strata_var)) {
    stopifnot(strata_var %in% names(x))
    d$.stratum <- as.character(x[[strata_var]])
    stopifnot(all(!is.na(d$.stratum[valid_weight]) & nzchar(d$.stratum[valid_weight])))
    strata_formula <- ~.stratum
  }
  full <- survey::svydesign(ids = ~.psu, strata = strata_formula, weights = ~weight,
                            data = d[valid_weight, ], nest = TRUE)
} else {
  full <- survey::svydesign(ids = ~1, weights = ~weight, data = d[valid_weight, ])
}
need_design <- subset(full, need)
estimate <- function(des, field) {
  v <- des$variables[[field]]
  p <- weighted.mean(as.numeric(v), des$variables$weight)
  ci <- c(NA_real_, NA_real_)
  if (design_confirmed && p > 0 && p < 1 && survey::degf(des) > 0) {
    des$variables$.event <- as.numeric(v)
    ci <- as.numeric(confint(survey::svyciprop(~.event, des, method = "logit", df = survey::degf(des))))
  }
  list(n = sum(v), denominator = length(v), estimate = p, lo = ci[1], hi = ci[2])
}
bar_defs <- data.frame(field = c("user", "wanted_start", "other_start", "modern", "modern_other_start", "traditional", "traditional_modern_switch", "intender", "no_intent", "unresolved"),
  label = c("Current contraceptive users", "Using the method wanted at start*", "Not using the method wanted at start*", "Current modern-method users", "Wanted another method at start*", "Traditional methods (subset of users)", "Wants to switch to a modern method", "Nonusers with intention", "Nonusers without stated intention", "Use / intention unresolved"),
  indent = c(0,1,1,0,1,0,1,0,0,0), y = c(0,1,2,3.8,4.8,6.6,7.6,9.4,11,12.6),
  color = c("#0a7d89", "#579b90", "#ba8743", "#307e60", "#ba8743", "#8c8981", "#b57a32", "#446da4", "#89639a", "#9caab0"), stringsAsFactors = FALSE)
bars <- lapply(seq_len(nrow(bar_defs)), function(i) c(as.list(bar_defs[i, ]), estimate(need_design, bar_defs$field[i])))
if (tail(bars, 1)[[1]]$n == 0) bars <- head(bars, -1)
dn <- d[d$need, ]; areas <- sort(unique(dn$geo))
rates <- lapply(areas, function(g) {
  a <- subset(need_design, geo == g)
  list(geo = g, n = nrow(a$variables), cpr = estimate(a, "user"), mcpr = estimate(a, "modern"),
       intender = estimate(a, "intender"), no_intent = estimate(a, "no_intent"))
})
fac$geo <- ifelse(!is.na(fac$geo_join_pipeline), key(fac$geo_join_pipeline), key(fac$source_geo_name))
stopifnot(all(!is.na(fac$geo)), length(unique(fac$geo)) == 20)
# The original inventory remains the coverage universe. Analysis uses Q506 Yes.
inventory<-fac
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","q506",lagos_frequency_fields())))
ri<-match(fac$fid,as.character(r$key));stopifnot(!anyNA(ri))
frequency<-lagos_frequency(r[ri,]);fac<-cbind(fac,frequency)
fp<-as.numeric(r$q506[ri]);fac<-fac[fp%in%1,]
fac$offering<-bool(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
fac$training<-as.numeric(fac$p_train);fac$larc_stock<-bool(fac$one_larc_stockin)
fac$stockout_burden<-as.numeric(fac$p_any_stockout)
stopifnot(nrow(fac)==1040,sum(fac$geo%in%areas)==882)
savecsv0<-function(z,name)write.csv(z,file.path(out,name),row.names=FALSE,na="")
savecsv0(fac[c("fid","geo",names(frequency))],"daily_provision_facility_audit.csv")
# Validate the single naming alias used by the existing join.
assigned_key <- key(fac$assigned_join_area)
assigned_key[assigned_key == "IFAKOIJAIYE"] <- "IFAKOIJAYE"
stopifnot(all(assigned_key == fac$geo))
fac$complete_supply <- complete.cases(fac[S])
fac$included <- if (facility_sample == "complete") fac$complete_supply else TRUE
savecsv <- function(z, name, rows = FALSE) write.csv(z, file.path(out, name), row.names = rows, na = "")
savecsv(data.frame(geo=areas,total_facilities=vapply(areas,function(g)sum(fac$geo==g),integer(1)),
 complete_facilities=vapply(areas,function(g)sum(fac$geo==g & fac$complete_supply),integer(1))),"facility_complete_case_coverage.csv")
dn_complete <- complete.cases(dn[D])
savecsv(do.call(rbind,lapply(c("LAGOS",areas),function(g){ii<-if(g=="LAGOS")rep(TRUE,nrow(dn))else dn$geo==g
 data.frame(geo=g,women_in_need=sum(ii),complete_demand=sum(ii & dn_complete),excluded=sum(ii & !dn_complete),
 complete_weight_share=sum(dn$weight[ii & dn_complete])/sum(dn$weight[ii]))})),"demand_complete_case_coverage.csv")
savecsv(do.call(rbind,lapply(D,function(k)data.frame(variable=k,missing=sum(!is.finite(dn[[k]])),
 total=nrow(dn),missing_pct=100*mean(!is.finite(dn[[k]])),weighted_missing_pct=100*wm(as.numeric(!is.finite(dn[[k]])),dn$weight)))),"demand_missingness.csv")
savecsv(do.call(rbind,lapply(S,function(k){ii<-fac$geo%in%areas;data.frame(variable=k,valid=sum(is.finite(fac[[k]][ii])),
 missing=sum(!is.finite(fac[[k]][ii])),total=sum(ii),missing_pct=100*mean(!is.finite(fac[[k]][ii])))})),"supply_missingness.csv")
savecsv(fac[c("fid","geo","source_geo_name","assigned_join_area","geo_join_pipeline")],"facility_geography_audit.csv")
cells <- list(); meta <- list()
display_areas <- sort(unique(inventory$geo))
for (g in c("LAGOS", display_areas)) {
  wi <- if (g == "LAGOS") rep(TRUE, nrow(dn)) else dn$geo %in% g
  fi <- (if (g == "LAGOS") fac$geo %in% areas else fac$geo %in% g) & fac$included
  meta[[g]] <- list(geo = g, n_need = sum(wi), facility_n = sum(fi), linked = g == "LAGOS" || g %in% areas)
  for (j in seq_along(D)) {
    k <- D[j]; ok <- wi & is.finite(dn[[k]]) & (demand_sample == "available" | dn_complete)
    cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Demand", variable = k, label = DL[j],
      value = if (any(ok)) wm(dn[[k]][ok], dn$weight[ok]) * 100 else NA_real_,
      n_valid = sum(ok), n_total = sum(wi), numerator = sum(dn[[k]][ok] == 1))
  }
  for (j in seq_along(S)) {
    k <- S[j]; ok <- fi & is.finite(fac[[k]])
    cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Supply", variable = k, label = SL[j],
      value = mean_valid(fac[[k]][fi]) * 100, n_valid = sum(ok), n_total = sum(fi), numerator = sum(fac[[k]][ok]))
  }
}
cells <- do.call(rbind, cells)
savecsv <- function(z, name, rows = FALSE) write.csv(z, file.path(out, name), row.names = rows, na = "")
savecsv(cells, "heatmap_values_and_denominators.csv")
M<-list(); transforms<-list()
for(side in c("Demand","Supply")){
 cols<-if(side=="Demand")D else S
 original<-sapply(cols,function(k){z<-cells[cells$side==side & cells$variable==k,];z$value[match(areas,z$geo)]})
 m<-if(side=="Supply")as.matrix(100*lagos_favorable_supply(as.data.frame(original/100)))else original
 rownames(m)<-areas;reference<-complete.cases(m)
 means<-colMeans(m[reference,,drop=FALSE]);sds<-apply(m[reference,,drop=FALSE],2,sd)
 meds<-apply(m[reference,,drop=FALSE],2,median);iqrs<-apply(m[reference,,drop=FALSE],2,IQR)
 ranks<-matrix(NA_real_,nrow(m),ncol(m),dimnames=dimnames(m))
 ranks[reference,]<-apply(m[reference,,drop=FALSE],2,rank,ties.method="average")
 stopifnot(all(sds>0),all(iqrs>0))
 M[[side]]<-list(raw=m,rank=ranks,
  robust=sweep(sweep(m,2,meds,"-"),2,iqrs,"/"),zscore=sweep(sweep(m,2,means,"-"),2,sds,"/"))
 for(k in cols)transforms[[length(transforms)+1L]]<-data.frame(geo=areas,side=side,variable=k,original_pct=original[,k],aligned_pct=m[,k],higher_raw_is_worse=side=="Supply"&&k=="stockout_burden",reference_included=reference,reference_n=sum(reference),
  indicator_mean=means[k],indicator_SD=sds[k],indicator_median=meds[k],indicator_IQR=iqrs[k],
  component_rank=M[[side]]$rank[,k],zscore=M[[side]]$zscore[,k],robust=M[[side]]$robust[,k])
}
classify<-function(df,sf)ifelse(is.na(df)|is.na(sf),"unassessed",ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
scores<-thresholds<-list()
for(id in c("rank","raw","robust","zscore")){
 ds<-rowMeans(M$Demand[[id]]);ss<-rowMeans(M$Supply[[id]]);dc<-if(id=="zscore")0 else median(ds,na.rm=TRUE);sc<-if(id=="zscore")0 else median(ss,na.rm=TRUE)
 df<-ds>dc+1e-10;sf<-ss<sc-1e-10
 scores[[id]]<-data.frame(method=id,geo=areas,demand_score=ds,supply_score=ss,demand_flag=df,supply_flag=sf,
   demand_priority_rank=rank(-ds,ties.method="average",na.last="keep"),supply_priority_rank=rank(ss,ties.method="average",na.last="keep"),category=classify(df,sf))
 thresholds[[id]]<-data.frame(method=id,demand_cutoff=dc,supply_cutoff=sc,demand_rule=if(id=="zscore")"strictly above final-score mean (zero)"else"strictly above final-score median",
  supply_rule=if(id=="zscore")"strictly below readiness-score mean (zero)"else"strictly below readiness-score median",demand_reference_n=sum(is.finite(ds)),supply_reference_n=sum(is.finite(ss)))
}
dc<-rowSums(sweep(M$Demand$raw,2,apply(M$Demand$raw,2,median,na.rm=TRUE),"-")>1e-10)
sc<-rowSums(sweep(M$Supply$raw,2,apply(M$Supply$raw,2,median,na.rm=TRUE),"-")< -1e-10)
scores$median_count<-data.frame(method="median_count",geo=areas,demand_score=dc,supply_score=sc,demand_flag=dc>=4,supply_flag=sc>=4,
 demand_priority_rank=rank(-dc,ties.method="average",na.last="keep"),supply_priority_rank=rank(-sc,ties.method="average",na.last="keep"),category=classify(dc>=4,sc>=4))
thresholds$median_count<-data.frame(method="median_count",demand_cutoff=4,supply_cutoff=4,demand_rule="4 or more of six above component medians",
 supply_rule="4 or more of six below component medians",demand_reference_n=sum(is.finite(dc)),supply_reference_n=sum(is.finite(sc)))
scores<-do.call(rbind,scores);thresholds<-do.call(rbind,thresholds);trace<-do.call(rbind,transforms)
exact <- cells[cells$geo %in% areas, c("geo", "side", "variable", "value")]
exact$original_pct_exact <- sprintf("%.17g", exact$value)
savecsv(exact[c("geo", "side", "variable", "original_pct_exact")], "scoring_inputs_exact.csv")
savecsv(scores, "all_method_scores_and_classifications.csv"); savecsv(thresholds, "classification_thresholds.csv")
savecsv(trace, "component_score_trace.csv")
planning <- lapply(areas, function(g) {
  q <- scores[scores$method == "zscore" & scores$geo == g, ]
  dt <- trace[trace$geo == g & trace$side == "Demand", ]; st <- trace[trace$geo == g & trace$side == "Supply", ]
  dt <- dt[order(-dt$zscore, match(dt$variable, D)), ]; st <- st[order(st$zscore, match(st$variable, S)), ]
  list(geo = g, category = q$category, demand_flag = q$demand_flag, supply_flag = q$supply_flag,
       demand_score = q$demand_score, supply_score = q$supply_score, n = meta[[g]]$n_need,
       top_demand = head(dt$variable, 3), top_supply = head(st$variable, 3))
})
coverage <- lapply(display_areas, function(g) list(geo = g, women = sum(d$geo %in% g), facilities = sum(inventory$geo %in% g), fp_facilities=sum(fac$geo %in% g)))
bar_table <- do.call(rbind, lapply(bars, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
savecsv(bar_table, "use_distribution_fpn2.csv")
lga_rates <- do.call(rbind, lapply(rates, function(r) data.frame(geo = r$geo, n_need = r$n,
  cpr = r$cpr$estimate, mcpr = r$mcpr$estimate, intender = r$intender$estimate, no_intent = r$no_intent$estimate)))
savecsv(lga_rates, "lga_use_rates_fpn2.csv")
audit <- list(country="Lagos",source_rows=nrow(x),positive_weight_rows=sum(valid_weight),need_n=nrow(dn),
 need_eligibility="Original SURGO fpn2=1, unchanged; no additional need reconstruction.",women_weight="weight",
 weighted_need_share=wm(as.numeric(d$need[valid_weight]),d$weight[valid_weight]),
 linked_lgas=length(areas),supply_lgas=length(display_areas),all_facility_records=nrow(inventory),
 linked_lga_facility_records=sum(inventory$geo%in%areas),fp_facility_records=nrow(fac),
 linked_fp_facilities=sum(fac$geo%in%areas),
 supply_aggregation="Q506 Yes only; valid-response equal-facility means within LGA, combining Rural and Urban; no facility weights. Genuine zeros included, undefined excluded per measure.",
 free_exception="No new zero recoding. The prior Ibeju-Lekki Urban exception was a Q506 No facility and is excluded under the FP-provider restriction.",
 health_definition="PB2A only: 1/2 concern; 3/4 no concern; otherwise unresolved.",
 stockout_definition="Original cleaned p_any_stockout unchanged. Heatmap shows burden; every score and PCA uses continuity=1-burden.",
 chart_denominator="All 2159 women with original fpn2=1; final weight percentages, unweighted n. Overlapping subsets.",
 use_definition="CU1/CU5 confirmed current use; CU4 and prompts classify modern/traditional; unresolved remains distinct.",
 design_fields_confirmed=design_confirmed,
 survey_variance=if(design_confirmed)"Confirmed runtime PSU/strata Taylor intervals"else"Final weight point estimates; official EA/PSU and strata unconfirmed, so overview intervals are withheld. No clusters inferred from household IDs or sampling probabilities.",
 four_category_labels="Relative above-average concerns and below-average readiness; not causal binding constraints or an absolute standard.",
 scoring_reference="17 linked LGAs equally weighted; each component centered on LGA mean and divided by sample SD. Mean six favorable supply z-scores; reverse stockout to continuity first. Demand>0 and readiness<0; 1e-10 ties unflagged.",
 main_method="zscore",sources=list(joined_file=basename(input),facility_file=basename(facility_input)),
 source_md5=as.list(unname(hash_before)),source_files_unchanged=identical(unname(hash_before),unname(tools::md5sum(c(input,facility_input)))))
stopifnot(audit$source_files_unchanged)
profiles <- data.frame(geo=areas,district=areas)
for(k in S)profiles[[k]] <- vapply(areas,function(g)mean_valid(fac[[k]][fac$geo==g & fac$included]),numeric(1))
profiles$n_facilities <- vapply(areas,function(g)sum(fac$geo==g & fac$included),integer(1))
stopifnot(all(profiles$n_facilities > 0))
sp <- profiles[match(d$geo,profiles$geo),S,drop=FALSE];rownames(sp)<-NULL
# Reconcile the original administrative join, allowing only the documented
# precision of the newly reaggregated FP-provider profiles.
map <- c(offering="share_offer_larc_and_3_sa",free="mean_p_free_prod",training="mean_p_train",daily_provision="mean_daily_modern_provision",larc_stock="share_stocked_1plus_larc",stockout_burden="mean_facility_any_stockout")
reconcile <- do.call(rbind,lapply(names(map),function(k){a<-as.numeric(x[[map[k]]]);b<-sp[[k]]
 data.frame(variable=k,max_abs_difference=max(abs(a-b),na.rm=TRUE),exception=FALSE,
 passed=if(facility_sample=="all") {ii<-rep(TRUE,nrow(d)); max(abs(a[ii]-b[ii]),na.rm=TRUE)<5.0001e-6} else TRUE)}))
stopifnot(all(reconcile$passed));savecsv(reconcile,"join_supply_reconciliation.csv")
audit$country <- "Lagos"; audit$facility_sample <- facility_sample; audit$demand_sample <- demand_sample
 audit$facility_records_used <- sum(fac$included & fac$geo%in%areas)
 audit$complete_facilities <- sum(fac$complete_supply & fac$geo%in%areas)
 audit$complete_demand_women <- sum(dn_complete)
 audit$supply_resolution <- "LGA only: each woman receives all included facilities averaged within her LGA"
 audit$counseling_included <- FALSE
 audit$positive_weight_rows <- sum(d$positive)
ctx <- unique(cbind(d[c("geo","district","residence","source_profile","fallback")],sp))
savecsv(profiles,"lga_supply_profiles.csv");savecsv(ctx,"assigned_supply_profiles.csv")
saveRDS(list(women=d,supply=sp,profiles=profiles,assigned_profiles=ctx,facility_sample=facility_sample),file.path(out,"analysis_frame.rds"))
payload <- list(audit = audit, demand_variables = D, demand_labels = DL, supply_variables = S, supply_labels = SL,
  bars = bars, rates = rates, heatmap = cells, area_metadata = unname(meta), scores = scores, thresholds = thresholds,
  component_trace = trace, planning = planning, coverage = coverage, profiles=profiles, assigned_profiles=ctx)
jsonlite::write_json(payload, file.path(out, "report_data.json"), auto_unbox = TRUE, dataframe = "rows", pretty = FALSE, na = "null", digits = 12)
jsonlite::write_json(audit, file.path(out, "analysis_audit.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("Built data:", nrow(dn), "women in need;", length(areas), "linked LGAs;", nrow(fac), "facility records.\n")
print(bar_table[, c("field", "n", "estimate")], row.names = FALSE)
print(thresholds, row.names = FALSE)
print(table(scores$method, scores$category))
