# Build Sindh's weighted need-group summaries and district planning scores.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k,args); if(is.na(i)||i==length(args)) stop("Supply ",k); args[i+1L] }
input <- opt("--input"); need_file <- opt("--need-file"); facility_file <- opt("--facility-input"); out <- opt("--output-dir")
for(pkg in c("survey","jsonlite")) if(!requireNamespace(pkg,quietly=TRUE)) stop("Install ",pkg)
here <- dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
source(file.path(here,"sindh_definitions.R")); dir.create(out,recursive=TRUE,showWarnings=FALSE)
hash <- unname(tools::md5sum(c(input,need_file,facility_file)))
x <- readRDS(input); d <- sindh_definitions(x,need_file); domains <- sindh_domains()
D <- domains$D; DL <- domains$DL; S <- domains$S; SL <- domains$SL
sp <- sindh_supply(x); profiles <- unique(cbind(d[c("geo","district")],sp,n_facilities=as.numeric(x$n_facilities)))
stopifnot(!anyDuplicated(profiles$geo),nrow(profiles)==30,nrow(x)==10364,sum(d$positive)==9926)
fac <- readRDS(facility_file); fac <- fac[fac$country=="sindh",]
key <- function(v) toupper(gsub("[^A-Za-z0-9]","",as.character(v)))
fac$geo <- key(fac$geo_join_pipeline)
mapped <- !is.na(fac$geo_join_pipeline) & nzchar(fac$geo_join_pipeline)
bool <- function(v) ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
fac$offering <- bool(fac$one_larc_3_sa_offer); fac$free <- as.numeric(fac$p_free_prod)
fac$training <- as.numeric(fac$p_train); fac$counselor <- as.numeric(fac$counselor_available)
fac$larc_stock <- bool(fac$one_larc_stockin); fac$popular_stock <- bool(fac$mp_in_stock)
valid_mean <- function(v) if(any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
wm <- function(v,w) {ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))weighted.mean(v[ok],w[ok]) else NA_real_}
savecsv <- function(v,name) write.csv(v,file.path(out,name),row.names=FALSE,na="")
areas <- sort(profiles$geo); names_map <- setNames(profiles$district,profiles$geo)
reconcile <- list()
for(g in areas) {
 ff <- fac[mapped & fac$geo%in%g,]; pp <- profiles[profiles$geo==g,]
 stopifnot(nrow(ff)==pp$n_facilities)
 for(k in S){actual<-valid_mean(ff[[k]]);expected<-pp[[k]]
  passed<-if(is.na(actual)||is.na(expected))is.na(actual)&&is.na(expected) else abs(actual-expected)<=5.00001e-6
  reconcile[[length(reconcile)+1L]]<-data.frame(geo=g,variable=k,facility_recalculation=actual,joined_value=expected,
    n_valid=sum(is.finite(ff[[k]])),n_facilities=nrow(ff),passed=passed)
 }
}
reconcile<-do.call(rbind,reconcile);stopifnot(all(reconcile$passed));savecsv(reconcile,"supply_input_reconciliation.csv")
options(survey.lonely.psu="adjust")
full <- survey::svydesign(ids=~psu,weights=~weight,data=d[d$positive,],nest=TRUE)
need_design <- subset(full,need)
estimate <- function(des,k){
 v<-as.numeric(des$variables[[k]]); p<-wm(v,des$variables$weight); ci<-c(NA_real_,NA_real_)
 if(is.finite(p)&&p>0&&p<1){des$variables$.event<-v;ci<-as.numeric(confint(survey::svyciprop(~.event,des,method="logit",df=survey::degf(des),na.rm=TRUE)))}
 list(n=sum(v==1,na.rm=TRUE),denominator=length(v),estimate=p,lo=ci[1],hi=ci[2])
}
bar_defs<-data.frame(field=c("user","wanted_start","other_start","modern","modern_other_start","traditional","traditional_modern_switch","intender","no_intent","unresolved"),
 label=c("Current contraceptive users","Using the method wanted at start*","Not using the method wanted at start*","Current modern-method users","Wanted another method at start*","Traditional methods (subset of users)","Wants to switch to a modern method","Nonusers with intention","Nonusers without stated intention","Use / intention unresolved"),
 indent=c(0,1,1,0,1,0,1,0,0,0),color=c("#0a7d89","#579b90","#ba8743","#307e60","#ba8743","#8c8981","#b57a32","#446da4","#89639a","#9caab0"))
bars<-lapply(seq_len(nrow(bar_defs)),function(i)c(as.list(bar_defs[i,]),estimate(need_design,bar_defs$field[i])))
rates<-lapply(areas,function(g){a<-subset(need_design,geo==g);list(geo=g,name=names_map[[g]],n=nrow(a$variables),
 cpr=estimate(a,"user"),mcpr=estimate(a,"modern"),intender=estimate(a,"intender"),no_intent=estimate(a,"no_intent"))})
dn<-d[d$need,];cells<-list();coverage<-list()
for(g in c("SINDH",areas)){
 wi<-if(g=="SINDH")rep(TRUE,nrow(dn)) else dn$geo%in%g
 fi<-mapped & if(g=="SINDH")rep(TRUE,nrow(fac)) else fac$geo%in%g
 coverage[[g]]<-list(geo=g,name=if(g=="SINDH")"Sindh" else names_map[[g]],women=sum(d$positive & if(g=="SINDH")rep(TRUE,nrow(d)) else d$geo%in%g),
  all_source_women=if(g=="SINDH")nrow(d) else sum(d$geo%in%g),n_need=sum(wi),facilities=sum(fi))
 for(j in seq_along(D)){k<-D[j];ok<-wi & is.finite(dn[[k]])
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Demand",variable=k,label=DL[j],value=100*wm(dn[[k]][ok],dn$weight[ok]),
    n_valid=sum(ok),n_total=sum(wi),numerator=sum(dn[[k]][ok]==1))}
 for(j in seq_along(S)){k<-S[j];v<-if(g=="SINDH")valid_mean(fac[[k]][fi]) else profiles[[k]][match(g,profiles$geo)]
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Supply",variable=k,label=SL[j],value=100*v,
    n_valid=sum(fi & is.finite(fac[[k]])),n_total=sum(fi),numerator=sum(fac[[k]][fi],na.rm=TRUE))}
}
cells<-do.call(rbind,cells);savecsv(cells,"heatmap_values_and_denominators.csv")
M<-list(); transforms<-list()
for(side in c("Demand","Supply")){
 cols<-if(side=="Demand")D else S
 m<-sapply(cols,function(k){z<-cells[cells$side==side & cells$variable==k,];z$value[match(areas,z$geo)]});rownames(m)<-areas
 means<-colMeans(m,na.rm=TRUE);sds<-apply(m,2,sd,na.rm=TRUE);meds<-apply(m,2,median,na.rm=TRUE);iqrs<-apply(m,2,IQR,na.rm=TRUE)
 stopifnot(all(sds>0),all(iqrs>0))
 M[[side]]<-list(raw=m,rank=apply(m,2,rank,ties.method="average",na.last="keep"),
  robust=sweep(sweep(m,2,meds,"-"),2,iqrs,"/"),zscore=sweep(sweep(m,2,means,"-"),2,sds,"/"))
 for(k in cols)transforms[[length(transforms)+1L]]<-data.frame(geo=areas,side=side,variable=k,original_pct=m[,k],
  indicator_mean=means[k],indicator_SD=sds[k],indicator_median=meds[k],indicator_IQR=iqrs[k],
  component_rank=M[[side]]$rank[,k],zscore=M[[side]]$zscore[,k],robust=M[[side]]$robust[,k])
}
classify<-function(df,sf)ifelse(is.na(df)|is.na(sf),"unassessed",ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
scores<-thresholds<-list()
for(id in c("rank","raw","robust","zscore")){
 ds<-rowMeans(M$Demand[[id]]);ss<-rowMeans(M$Supply[[id]]);dc<-median(ds,na.rm=TRUE);sc<-median(ss,na.rm=TRUE)
 df<-ds>dc+1e-10;sf<-ss<sc-1e-10
 scores[[id]]<-data.frame(method=id,geo=areas,demand_score=ds,supply_score=ss,demand_flag=df,supply_flag=sf,
   demand_priority_rank=rank(-ds,ties.method="average",na.last="keep"),supply_priority_rank=rank(ss,ties.method="average",na.last="keep"),category=classify(df,sf))
 thresholds[[id]]<-data.frame(method=id,demand_cutoff=dc,supply_cutoff=sc,demand_rule="strictly above final-score median",
  supply_rule="strictly below readiness-score median",demand_reference_n=sum(is.finite(ds)),supply_reference_n=sum(is.finite(ss)))
}
dc<-rowSums(sweep(M$Demand$raw,2,apply(M$Demand$raw,2,median,na.rm=TRUE),"-")>1e-10)
sc<-rowSums(sweep(M$Supply$raw,2,apply(M$Supply$raw,2,median,na.rm=TRUE),"-")< -1e-10)
scores$median_count<-data.frame(method="median_count",geo=areas,demand_score=dc,supply_score=sc,demand_flag=dc>=4,supply_flag=sc>=4,
 demand_priority_rank=rank(-dc,ties.method="average",na.last="keep"),supply_priority_rank=rank(-sc,ties.method="average",na.last="keep"),category=classify(dc>=4,sc>=4))
thresholds$median_count<-data.frame(method="median_count",demand_cutoff=4,supply_cutoff=4,demand_rule="4 or more of six above component medians",
 supply_rule="4 or more of six below component medians",demand_reference_n=sum(is.finite(dc)),supply_reference_n=sum(is.finite(sc)))
scores<-do.call(rbind,scores);thresholds<-do.call(rbind,thresholds);trace<-do.call(rbind,transforms)
planning<-lapply(areas,function(g){q<-scores[scores$method=="zscore" & scores$geo==g,];dt<-trace[trace$geo==g & trace$side=="Demand",];st<-trace[trace$geo==g & trace$side=="Supply",]
 list(geo=g,name=names_map[[g]],category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,demand_score=q$demand_score,supply_score=q$supply_score,
  top_demand=head(dt$variable[order(-dt$zscore,match(dt$variable,D),na.last=TRUE)],3),
  top_supply=if(all(is.finite(st$zscore)))head(st$variable[order(st$zscore,match(st$variable,S))],3) else character(),
  n_need=sum(dn$geo==g),facility_count=profiles$n_facilities[match(g,profiles$geo)])})
missing<-do.call(rbind,lapply(areas,function(g)do.call(rbind,lapply(D,function(k){a<-dn[dn$geo==g,];data.frame(geo=g,variable=k,n=nrow(a),valid=sum(is.finite(a[[k]])),missing=sum(!is.finite(a[[k]])),weighted_pct=100*wm(a[[k]],a$weight))}))))
savecsv(missing,"demand_missingness_by_district.csv")
savecsv(scores,"all_method_scores_and_classifications.csv");savecsv(thresholds,"classification_thresholds.csv");savecsv(trace,"component_score_trace.csv")
savecsv(do.call(rbind,lapply(bars,as.data.frame)),"use_distribution.csv")
savecsv(do.call(rbind,lapply(rates,function(r)data.frame(geo=r$geo,name=r$name,n_need=r$n,cpr=r$cpr$estimate,mcpr=r$mcpr$estimate))),"district_use_rates.csv")
savecsv(profiles,"district_supply_profiles.csv")
comparison<-data.frame(geo=areas,name=names_map[areas]);for(id in c("rank","raw","robust","median_count","zscore"))comparison[[id]]<-scores$category[scores$method==id][match(areas,scores$geo[scores$method==id])]
savecsv(comparison,"classification_comparison.csv")
plan_table<-do.call(rbind,lapply(planning,function(q)data.frame(district=q$name,category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,
 top_demand=paste(q$top_demand,collapse="; "),top_supply=paste(q$top_supply,collapse="; "),n_need=q$n_need,facility_count=q$facility_count)))
savecsv(plan_table,"planning_priorities.csv")
audit<-list(country="Sindh",source_rows=nrow(x),consenting_positive_weight=sum(d$positive),need_n=sum(d$need),
 need_variable="fpn2_derived from the supplied Sindh pipeline, matched by col_id; no need_n permanent-protection additions.",
 need_qualification="Researcher reconstruction of the Pakistan questionnaire formula, not a populated SURGO fpn2 flag. Approved existing coding retains SA1=95/97 as exclusions; 72 otherwise eligible women are excluded by that special-code policy.",
 need_independent_reproduction_disagreements=0,weight_field="Normalizedweights",psu_field="clusterCode from the weighted source file",
 psu_precision="rb0e exactly reproduces clusterCode after IEEE single-precision rounding, with differences up to two identifier units and some collapsed IDs. Use clusterCode without rounding; do not use rb0e as an exact duplicate.",
 survey_design="Final normalized weights and recorded sampling clusterCode; no inferred strata or finite-population correction. Full consenting positive-weight design constructed before need-domain selection. Official full sampling specification remains unavailable.",
 survey_psus=length(unique(d$psu[d$positive])),districts=length(areas),facility_records=nrow(fac),mapped_facility_records=sum(mapped),unmapped_facility_records=sum(!mapped),
 geography_version="pipeline_crosswalk; identical district join supplied previously. 881 facilities without source district_name are not silently reassigned.",
 supply_resolution="district",supply_unavailable="Cleaned three-LARC stock share is NA throughout because the source aggregation suppresses a country-wide all-zero indicator. Raw three_larc_stockin is FALSE for all Sindh facilities; it cannot be standardized. Existing share_stocked_1plus_larc replaces it, explicitly labeled.",
 supply_missing="Tando Allah Yar has no valid trained-provider or counselor records in the source-crosswalk facility subset. Retain NA, show its demand profile, leave supply category unassessed, and exclude incomplete supply rows from the common model sample. No zero fill, partial-component averaging or geographic fallback.",
 supply_denominators="Original UoM valid-response definitions; facility aggregation is unweighted. Shares copied from join are rounded to five decimal places for the original Boolean summaries; reconciliation allows only that rounding.",
 scoring="Six components per side; average-z main. Normalize each component across districts with valid data; demand has 30 complete scores, supply has 29. Score medians computed separately from observed scores; strict ties are unflagged. No outcome-informed weights or cutoffs.",
 weighted_need_share=wm(as.numeric(d$need[d$positive]),d$weight[d$positive]),
 use_definition="CU1 yes/sometimes plus CU5 prompts. Modern: CU4 1-8, prompted condom/ECP or explicitly contraceptive LAM. Traditional-only and confirmed nonuse are zero for modern-use models; unresolved method/use is excluded from models.",
 controls="Age and age squared, country-specific education, fertility goal. Partnership is constant in the modeled cohort; urban/rural is unavailable. Neither is artificially added.",
 source_md5=as.list(hash),source_names=as.list(basename(c(input,need_file,facility_file))),source_files_unchanged=identical(hash,unname(tools::md5sum(c(input,need_file,facility_file)))))
stopifnot(audit$source_files_unchanged,sum(fac$country=="sindh")==4308,sum(mapped)==3427,
 sum(vapply(bars,function(b)if(b$field%in%c("user","intender","no_intent","unresolved"))b$n else 0,numeric(1)))==sum(d$need))
payload<-list(audit=audit,demand_variables=D,demand_labels=DL,supply_variables=S,supply_labels=SL,bars=bars,rates=rates,
 heatmap=cells,coverage=unname(coverage),scores=scores,thresholds=thresholds,component_trace=trace,planning=planning,profiles=profiles)
jsonlite::write_json(payload,file.path(out,"report_data.json"),dataframe="rows",auto_unbox=TRUE,na="null",digits=12)
jsonlite::write_json(audit,file.path(out,"analysis_audit.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
saveRDS(list(women=d,supply=sp,profiles=profiles),file.path(out,"analysis_frame.rds"))
cat('Built',sum(d$need),'women in need across',length(areas),'districts; recorded clusters',audit$survey_psus,'\n')
print(table(scores$method,scores$category));print(data.frame(field=bar_defs$field,n=vapply(bars,`[[`,numeric(1),"n"),pct=100*vapply(bars,`[[`,numeric(1),"estimate")))
