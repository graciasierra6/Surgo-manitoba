# Build Sindh's weighted need-group summaries and district planning scores.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k,args); if(is.na(i)||i==length(args)) stop("Supply ",k); args[i+1L] }
input <- opt("--input"); need_file <- opt("--need-file"); facility_file <- opt("--facility-input"); out <- opt("--output-dir"); raw_facility<-opt("--raw-facility")
for(pkg in c("survey","jsonlite")) if(!requireNamespace(pkg,quietly=TRUE)) stop("Install ",pkg)
here <- dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
source(file.path(here,"..","..","..","analysis/sindh/sindh_definitions.R"));source(file.path(here,"supply_definitions.R")); dir.create(out,recursive=TRUE,showWarnings=FALSE)
hash <- unname(tools::md5sum(c(input,need_file,facility_file)))
x <- readRDS(input); d <- sindh_definitions(x,need_file); domains <- sindh_updated_domains()
pb2a<-as.numeric(x$pb2a);d$health<-ifelse(pb2a%in%1:4,as.numeric(pb2a%in%1:2),NA_real_)
d$source_profile<-d$geo;d$fallback<-FALSE
D <- domains$D; DL <- domains$DL; S <- domains$S; SL <- domains$SL
sp <- sindh_updated_supply(x); profiles <- unique(cbind(d[c("geo","district")],sp,n_facilities=as.numeric(x$n_facilities)))
stopifnot(!anyDuplicated(profiles$geo),nrow(profiles)==30,nrow(x)==10364,sum(d$positive)==9926)
fac <- readRDS(facility_file); fac <- fac[fac$country=="sindh",]
key <- function(v) toupper(gsub("[^A-Za-z0-9]","",as.character(v)))
fac$geo<-key(fac$assigned_join_area);inventory<-fac
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","facility_id","q506",sindh_frequency_fields())))
raw_id<-as.character(r$key);empty<-is.na(raw_id)|trimws(raw_id)=="";raw_id[empty]<-as.character(r$facility_id[empty])
ri<-match(fac$fid,raw_id);stopifnot(!anyNA(ri))
frequency<-sindh_frequency(r[ri,]);fac<-cbind(fac,frequency)
fp<-as.numeric(r$q506[ri]);fac<-fac[fp%in%1,]
mapped<-!is.na(fac$geo)&nzchar(fac$geo)
bool<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
fac$offering<-bool(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
fac$training<-as.numeric(fac$p_train);fac$larc_stock<-bool(fac$one_larc_stockin)
fac$stockout_burden<-as.numeric(fac$p_any_stockout)
stopifnot(nrow(fac)==2823,all(mapped),length(unique(fac$geo))==30)
valid_mean <- function(v) if(any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
wm <- function(v,w) {ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))weighted.mean(v[ok],w[ok]) else NA_real_}
savecsv <- function(v,name) write.csv(v,file.path(out,name),row.names=FALSE,na="")
areas <- sort(profiles$geo); names_map <- setNames(profiles$district,profiles$geo)
savecsv(fac[c("fid","geo",names(frequency))],"daily_provision_facility_audit.csv")
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
 if(is.finite(p)&&p>0&&p<1&&survey::degf(des)>0){des$variables$.event<-v;ci<-as.numeric(confint(survey::svyciprop(~.event,des,method="logit",df=survey::degf(des),na.rm=TRUE)))}
 list(n=sum(v==1,na.rm=TRUE),denominator=length(v),estimate=p,lo=ci[1],hi=ci[2])
}
bar_defs<-data.frame(field=c("user","wanted_start","other_start","modern","modern_other_start","traditional","traditional_modern_switch","intender","no_intent","unresolved"),
 label=c("Current contraceptive users","Using the method wanted at start*","Not using the method wanted at start*","Current modern-method users","Wanted another method at start*","Traditional methods (subset of users)","Wants to switch to a modern method","Nonusers with intention","Nonusers without stated intention","Use / intention unresolved"),
 indent=c(0,1,1,0,1,0,1,0,0,0),color=c("#0a7d89","#579b90","#ba8743","#307e60","#ba8743","#8c8981","#b57a32","#446da4","#89639a","#9caab0"))
bar_defs$y<-c(0,1,2,3.8,4.8,6.6,7.6,9.4,11,12.6)
bars<-lapply(seq_len(nrow(bar_defs)),function(i)c(as.list(bar_defs[i,]),estimate(need_design,bar_defs$field[i])))
rates<-lapply(areas,function(g){a<-subset(need_design,geo==g);list(geo=g,name=names_map[[g]],n=nrow(a$variables),
 cpr=estimate(a,"user"),mcpr=estimate(a,"modern"),intender=estimate(a,"intender"),no_intent=estimate(a,"no_intent"))})
dn<-d[d$need,];cells<-list();coverage<-list()
for(g in c("SINDH",areas)){
 wi<-if(g=="SINDH")rep(TRUE,nrow(dn)) else dn$geo%in%g
 fi<-mapped & if(g=="SINDH")rep(TRUE,nrow(fac)) else fac$geo%in%g
 coverage[[g]]<-list(geo=g,name=if(g=="SINDH")"Sindh" else names_map[[g]],women=sum(if(g=="SINDH")rep(TRUE,nrow(d)) else d$geo%in%g),
  all_source_women=if(g=="SINDH")nrow(d) else sum(d$geo%in%g),n_need=sum(wi),facilities=sum(if(g=="SINDH")rep(TRUE,nrow(inventory))else inventory$geo%in%g),fp_facilities=sum(fi))
 for(j in seq_along(D)){k<-D[j];ok<-wi & is.finite(dn[[k]])
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Demand",variable=k,label=DL[j],value=100*wm(dn[[k]][ok],dn$weight[ok]),
    n_valid=sum(ok),n_total=sum(wi),numerator=sum(dn[[k]][ok]==1))}
 for(j in seq_along(S)){k<-S[j];v<-if(g=="SINDH")valid_mean(fac[[k]][fi]) else profiles[[k]][match(g,profiles$geo)]
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Supply",variable=k,label=SL[j],value=100*v,
    n_valid=sum(fi & is.finite(fac[[k]])),n_total=sum(fi),numerator=sum(fac[[k]][fi],na.rm=TRUE))}
}
cells<-do.call(rbind,cells);savecsv(cells,"heatmap_values_and_denominators.csv")
M<-list();transforms<-list()
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")D else S
 original<-sapply(fields,function(k){rr<-cells[cells$side==side&cells$variable==k,];rr$value[match(areas,rr$geo)]})
 aligned<-original;if(side=="Supply")aligned[,"stockout_burden"]<-100-aligned[,"stockout_burden"]
 rownames(aligned)<-areas;reference<-complete.cases(aligned)
 mu<-colMeans(aligned[reference,,drop=FALSE]);sd0<-apply(aligned[reference,,drop=FALSE],2,sd)
 stopifnot(all(sd0>0),sum(reference)==30)
 zz<-sweep(sweep(aligned,2,mu,"-"),2,sd0,"/")
 M[[side]]<-list(zscore=zz)
 for(k in fields)transforms[[length(transforms)+1L]]<-data.frame(geo=areas,side=side,variable=k,
  original_pct=original[,k],aligned_pct=aligned[,k],higher_raw_is_worse=side=="Supply"&&k=="stockout_burden",
  reference_included=reference,reference_n=sum(reference),indicator_mean=mu[k],indicator_SD=sd0[k],zscore=zz[,k])
}
ds<-rowMeans(M$Demand$zscore);ss<-rowMeans(M$Supply$zscore);df<-ds>1e-10;sf<-ss< -1e-10
scores<-data.frame(method="zscore",geo=areas,demand_score=ds,supply_score=ss,demand_flag=df,supply_flag=sf,
 demand_priority_rank=rank(-ds,ties.method="average"),supply_priority_rank=rank(ss,ties.method="average"),
 category=ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
thresholds<-data.frame(method="zscore",demand_cutoff=0,supply_cutoff=0,demand_rule="strictly above final-score mean (zero)",
 supply_rule="strictly below readiness-score mean (zero)",demand_reference_n=30,supply_reference_n=30)
trace<-do.call(rbind,transforms)
stopifnot(abs(mean(ds))<1e-12,abs(mean(ss))<1e-12)
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
plan_table<-do.call(rbind,lapply(planning,function(q)data.frame(district=q$name,category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,
 top_demand=paste(q$top_demand,collapse="; "),top_supply=paste(q$top_supply,collapse="; "),n_need=q$n_need,facility_count=q$facility_count)))
savecsv(plan_table,"planning_priorities.csv")
dn_complete<-complete.cases(dn[D])
savecsv(do.call(rbind,lapply(D,function(k)data.frame(variable=k,missing=sum(!is.finite(dn[[k]])),total=nrow(dn),
 missing_pct=100*mean(!is.finite(dn[[k]])),weighted_missing_pct=100*wm(as.numeric(!is.finite(dn[[k]])),dn$weight)))),"demand_missingness.csv")
savecsv(do.call(rbind,lapply(S,function(k)data.frame(variable=k,valid=sum(is.finite(fac[[k]])),missing=sum(!is.finite(fac[[k]])),
 total=nrow(fac),missing_pct=100*mean(!is.finite(fac[[k]]))))),"supply_missingness.csv")
correlations<-list()
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")D else S;mat<-if(side=="Demand")dn[D]else profiles[S]
 for(pair in combn(fields,2,simplify=FALSE)){
  ok<-complete.cases(mat[pair]);a<-mat[[pair[1]]][ok];b<-mat[[pair[2]]][ok]
  w<-if(side=="Demand")dn$weight[ok]else rep(1,sum(ok))
  cv<-sum(w*(a-weighted.mean(a,w))*(b-weighted.mean(b,w)))/sum(w)
  den<-sqrt(weighted.mean((a-weighted.mean(a,w))^2,w)*weighted.mean((b-weighted.mean(b,w))^2,w))
  correlations[[length(correlations)+1L]]<-data.frame(side=side,first=pair[1],second=pair[2],correlation=cv/den,paired_records=sum(ok))
 }
}
savecsv(do.call(rbind,correlations),"component_correlations.csv")
audit<-list(country="Sindh",source_rows=nrow(x),positive_weight_rows=sum(d$positive),consenting_positive_weight=sum(d$positive),need_n=sum(d$need),
 need_variable="Authorized fpn2_derived matched by col_id; original source fpn2 is entirely code 95 and is not used as a need flag.",
 need_qualification="Retain the approved upstream instrument-based rule, including SA1=95/97 exclusions. Not a DHS unmet-need algorithm; no need_n permanent-protection additions.",
 women_weight="Normalizedweights",weight_field="Normalizedweights",psu_field="Recorded unrounded clusterCode",
 survey_psus=length(unique(d$psu[d$positive])),design_fields_confirmed=TRUE,
 survey_design="Final normalized weights and recorded clusterCode; no inferred strata or FPC. Full consenting positive-weight design before need-domain subsetting. Full original sampling design remains unavailable.",
 linked_districts=length(areas),districts=length(areas),all_facility_records=nrow(inventory),linked_source_facilities=nrow(inventory),
 fp_facility_records=nrow(fac),linked_fp_facilities=nrow(fac),daily_valid_linked=sum(is.finite(fac$daily_provision)),daily_undefined_linked=sum(!is.finite(fac$daily_provision)),
 geography_version="Checked named district plus supplied aliases; all 4308 facilities assigned. The older source-crosswalk report used 3427 and omitted 881; no pre-existing matched assignment conflicts with the checked one.",
 supply_aggregation="Q506 Yes only; equal-facility valid-response means within district. Valid zero/FALSE included; unknown/undefined excluded separately by measure. No facility weights, zero filling or Rural/Urban split.",
 health_definition="PB2A only: 1/2 concern, 3/4 no concern; otherwise unresolved.",
 scoring_reference="Same 30 complete districts on each side; unweighted district mean and sample SD per component. Stockout becomes continuity before supply scoring. Average six z-scores; demand>0 and readiness<0; 1e-10 ties unflagged.",
 complete_demand_women=sum(dn_complete),weighted_need_share=wm(as.numeric(d$need[d$positive]),d$weight[d$positive]),
 controls="Age, age squared, country-specific education, fertility goal. Partnership constant among modeled women; Rural/Urban unavailable.",
 main_method="zscore",source_md5=as.list(hash),source_names=as.list(basename(c(input,need_file,facility_file))),
 source_files_unchanged=identical(hash,unname(tools::md5sum(c(input,need_file,facility_file)))))
stopifnot(audit$source_files_unchanged,sum(d$need)==3274,nrow(inventory)==4308,nrow(fac)==2823)
meta<-lapply(coverage,function(q)list(geo=q$geo,n_need=q$n_need,facility_n=q$fp_facilities,source_facilities=q$facilities,linked=TRUE))
payload<-list(audit=audit,demand_variables=D,demand_labels=DL,supply_variables=S,supply_labels=SL,bars=bars,rates=rates,
 heatmap=cells,coverage=unname(coverage),area_metadata=unname(meta),scores=scores,thresholds=thresholds,component_trace=trace,planning=planning,profiles=profiles)
jsonlite::write_json(payload,file.path(out,"report_data.json"),dataframe="rows",auto_unbox=TRUE,na="null",digits=12)
jsonlite::write_json(audit,file.path(out,"analysis_audit.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
saveRDS(list(women=d,supply=sp,profiles=profiles),file.path(out,"analysis_frame.rds"))
cat("Built",sum(d$need),"women in need;",nrow(fac),"FP facilities in 30 districts.\n")
print(table(scores$category));print(data.frame(field=bar_defs$field,n=vapply(bars,`[[`,numeric(1),"n"),pct=100*vapply(bars,`[[`,numeric(1),"estimate")))
