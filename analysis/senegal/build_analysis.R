# Senegal main analysis: exact department-residence profiles, flagged department fallback.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-opt("--input");admin_input<-opt("--admin-input");facility_file<-opt("--facility-input")
raw_file<-opt("--raw");raw_facility<-opt("--raw-facility");residence_lookup<-opt("--residence-lookup")
department_lookup<-opt("--department-lookup");out<-opt("--output-dir")
for(pkg in c("survey","jsonlite","haven","readxl"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
here<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
source(file.path(here,"senegal_definitions.R"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
inputs<-c(input,admin_input,facility_file,raw_file,raw_facility,residence_lookup,department_lookup)
hash<-unname(tools::md5sum(inputs));savecsv<-function(v,name)write.csv(v,file.path(out,name),row.names=FALSE,na="")
key<-senegal_key
lab<-function(v)if(inherits(v,"haven_labelled"))as.character(haven::as_factor(v))else as.character(v)
valid_mean<-function(v)if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_
wm<-function(v,w){ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))weighted.mean(v[ok],w[ok])else NA_real_}
bool<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
x0<-readRDS(input);admin<-readRDS(admin_input);x<-x0
stopifnot(nrow(x)==7169,!anyDuplicated(x$IDW),!anyDuplicated(admin$IDW))
ii<-match(x$IDW,admin$IDW)
stopifnot(!anyNA(ii),all(x$link_geo_join==admin$link_geo_join[ii]),all(admin$link_has_supply[ii]))
fallback<-!x$link_has_supply
stopifnot(!anyNA(fallback),sum(fallback)==1815,all(x$link_rurb[fallback]=="Rural"))
raw_names<-names(haven::read_dta(raw_file,n_max=0))
supply_columns<-setdiff(intersect(names(x),names(admin)),raw_names)
supply_columns<-supply_columns[!grepl("^link_",supply_columns)]
for(k in supply_columns)x[[k]][fallback]<-admin[[k]][ii[fallback]]
x$analysis_supply_fallback<-fallback
x$analysis_supply_match_level<-ifelse(fallback,"Department fallback","Department + residence")
x$analysis_supply_source_key<-paste(key(x$link_geo_join),ifelse(fallback,"Department",as.character(x$link_rurb)),sep="|")
x$analysis_has_supply<-x$link_has_supply|fallback
x$analysis_facility_residence_basis<-"Health-district lookup proxy; not directly recorded facility settlement type"
stopifnot(all(x$analysis_has_supply),all(x$IDW==x0$IDW),nrow(x)==nrow(x0))
saveRDS(x,file.path(out,"senegal_women_manitoba_hybrid.rds"))
d<-senegal_definitions(x);domains<-senegal_domains();D<-domains$D;DL<-domains$DL;S<-domains$S;SL<-domains$SL
sp<-senegal_supply(x)
d$fallback<-fallback;d$source_profile<-x$analysis_supply_source_key
profiles<-unique(cbind(geo=key(admin$link_geo_join),district=as.character(admin$link_geo_join),
 senegal_supply(admin),n_facilities=as.numeric(admin$n_facilities)))
stopifnot(!anyDuplicated(profiles$geo),nrow(profiles)==33,sum(d$positive)==7158,sum(d$need)==1905)
ctx<-unique(cbind(d[c("geo","district","residence","fallback","source_profile")],sp,n_facilities=as.numeric(x$n_facilities)))
stopifnot(!anyDuplicated(paste(ctx$geo,ctx$residence)),nrow(ctx)==49)
savecsv(ctx,"assigned_supply_profiles.csv")
fac<-readRDS(facility_file);fac<-fac[fac$country=="senegal",]
fac$geo<-key(fac$assigned_join_area)
areas<-sort(profiles$geo);names_map<-setNames(profiles$district,profiles$geo)
mapped<-fac$geo%in%areas
# Independently verify health-district -> department and lookup residence against raw records.
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("fid","s1_3","s7_2")))
ri<-match(fac$fid,lab(r$fid));stopifnot(!anyNA(ri))
lut<-readxl::read_xlsx(residence_lookup);li<-match(key(lab(r$s1_3)[ri]),key(lut$s1_3))
rv<-as.numeric(lut$urbanrural[li]);rr<-ifelse(rv==1,"Rural",ifelse(rv==2,"Urban",NA_character_))
stopifnot(identical(ifelse(is.na(rr),"[unknown]",rr),ifelse(is.na(fac$rurb),"[unknown]",as.character(fac$rurb))))
e<-read.csv(department_lookup,check.names=FALSE,stringsAsFactors=FALSE)
ei<-match(key(lab(r$s1_3)[ri]),key(e[["Health district"]]));stopifnot(!anyNA(ei))
ed<-trimws(sub(" \u2014.*$","",e[["Department-checked"]][ei]))
ed<-trimws(sub(" \\(.*$","",ed))
stopifnot(all(key(ed)==fac$geo))
counsel<-as.numeric(r$s7_2[ri]);cf<-ifelse(counsel%in%1,1,ifelse(counsel%in%c(0,2),0,NA_real_))
stopifnot(isTRUE(all.equal(cf,fac$counselor_available,check.attributes=FALSE)))
fac$offering<-bool(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
fac$training<-as.numeric(fac$p_train);fac$counselor<-as.numeric(fac$counselor_available)
fac$larc_stock<-bool(fac$one_larc_stockin);fac$popular_stock<-bool(fac$mp_in_stock)
stopifnot(all(bool(fac$three_larc_stockin)==0),nrow(fac)==2043,sum(mapped)==1648)
reconcile<-list()
check_profile<-function(ff,pp,scope,area,residence){
 stopifnot(nrow(ff)==pp$n_facilities)
 for(k in S){actual<-valid_mean(ff[[k]]);expected<-pp[[k]]
  passed<-if(is.na(actual)||is.na(expected))is.na(actual)&&is.na(expected)else abs(actual-expected)<=5.00001e-6
  reconcile[[length(reconcile)+1L]]<<-data.frame(scope=scope,geo=area,residence=residence,variable=k,
   facility_recalculation=actual,joined_value=expected,n_valid=sum(is.finite(ff[[k]])),n_facilities=nrow(ff),passed=passed)
 }
}
for(g in areas)check_profile(fac[fac$geo==g,],profiles[profiles$geo==g,],"Department",g,"All")
for(j in seq_len(nrow(ctx))){q<-ctx[j,];fi<-fac$geo==q$geo
 if(!q$fallback)fi<-fi&!is.na(fac$rurb)&fac$rurb==q$residence
 check_profile(fac[fi,],q,if(q$fallback)"Department fallback"else"Department + residence",q$geo,q$residence)
}
reconcile<-do.call(rbind,reconcile);stopifnot(all(reconcile$passed));savecsv(reconcile,"supply_input_reconciliation.csv")
trace<-data.frame(row=d$row,IDW=d$id,department=d$district,residence=d$residence,in_need=d$need,
 exact_profile_available=!fallback,department_fallback=fallback,supply_source_key=d$source_profile,
 assigned_facilities=x$n_facilities)
saveRDS(trace,file.path(out,"woman_assignment_audit.rds"))
fallback_table<-do.call(rbind,lapply(areas,function(g){ff<-fac[fac$geo==g,];w<-d$geo==g
 data.frame(geo=g,department=names_map[[g]],source_women=sum(w),rural_women=sum(w&d$residence=="Rural"),
 urban_women=sum(w&d$residence=="Urban"),need_women=sum(w&d$need),rural_need=sum(w&d$need&d$residence=="Rural"),
 urban_need=sum(w&d$need&d$residence=="Urban"),fallback_women=sum(w&fallback),fallback_need=sum(w&fallback&d$need),
 facility_records=nrow(ff),rural_classified_facilities=sum(ff$rurb=="Rural",na.rm=TRUE),
 urban_classified_facilities=sum(ff$rurb=="Urban",na.rm=TRUE),unknown_residence_facilities=sum(is.na(ff$rurb)))
}));savecsv(fallback_table,"department_residence_assignment_summary.csv")
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
for(g in c("SENEGAL",areas)){
 wi<-if(g=="SENEGAL")rep(TRUE,nrow(dn)) else dn$geo%in%g
 fi<-mapped & if(g=="SENEGAL")rep(TRUE,nrow(fac)) else fac$geo%in%g
 coverage[[g]]<-list(geo=g,name=if(g=="SENEGAL")"Senegal" else names_map[[g]],women=sum(d$positive & if(g=="SENEGAL")rep(TRUE,nrow(d)) else d$geo%in%g),
  all_source_women=if(g=="SENEGAL")nrow(d) else sum(d$geo%in%g),n_need=sum(wi),facilities=sum(fi))
 for(j in seq_along(D)){k<-D[j];ok<-wi & is.finite(dn[[k]])
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Demand",variable=k,label=DL[j],value=100*wm(dn[[k]][ok],dn$weight[ok]),
    n_valid=sum(ok),n_total=sum(wi),numerator=sum(dn[[k]][ok]==1))}
 for(j in seq_along(S)){k<-S[j];v<-if(g=="SENEGAL")valid_mean(fac[[k]][fi]) else profiles[[k]][match(g,profiles$geo)]
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Supply",variable=k,label=SL[j],value=100*v,
    n_valid=sum(fi & is.finite(fac[[k]])),n_total=sum(fi),numerator=sum(fac[[k]][fi],na.rm=TRUE))}
}
for(g in setdiff(sort(unique(fac$geo)),areas)) {
 coverage[[g]]<-list(geo=g,name=fac$assigned_join_area[match(g,fac$geo)],women=0,
  all_source_women=0,n_need=0,facilities=sum(fac$geo==g))
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
savecsv(missing,"demand_missingness_by_department.csv")
savecsv(scores,"all_method_scores_and_classifications.csv");savecsv(thresholds,"classification_thresholds.csv");savecsv(trace,"component_score_trace.csv")
savecsv(do.call(rbind,lapply(bars,as.data.frame)),"use_distribution.csv")
savecsv(do.call(rbind,lapply(rates,function(r)data.frame(geo=r$geo,name=r$name,n_need=r$n,cpr=r$cpr$estimate,mcpr=r$mcpr$estimate))),"department_use_rates.csv")
savecsv(profiles,"department_supply_profiles.csv")
comparison<-data.frame(geo=areas,name=names_map[areas]);for(id in c("rank","raw","robust","median_count","zscore"))comparison[[id]]<-scores$category[scores$method==id][match(areas,scores$geo[scores$method==id])]
savecsv(comparison,"classification_comparison.csv")
plan_table<-do.call(rbind,lapply(planning,function(q)data.frame(department=q$name,category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,
 top_demand=paste(q$top_demand,collapse="; "),top_supply=paste(q$top_supply,collapse="; "),n_need=q$n_need,facility_count=q$facility_count)))
savecsv(plan_table,"planning_priorities.csv")
audit<-list(country="Senegal",source_rows=nrow(x),consenting_positive_weight=sum(d$positive),need_n=sum(d$need),
 need_variable="Original SURGO FPN2 = 1, retained unchanged. No reconstructed need_n, sterilization or hysterectomy additions.",
 need_qualification="Use the supplied current-need flag as recorded; do not substitute the broader segmentation population or DHS unmet-need algorithm.",
 weight_field="WEIGHT",psu_field="RB0H: recorded District de recensement (census district)",
 survey_design="Final WEIGHT and recorded RB0H census districts, nested in departments and residence. Full consenting positive-weight frame before FPN2 domain selection. Original sampling strata/FPC not documented; no reconstructed strata claimed.",
 survey_psus=length(unique(d$psu[d$positive])),districts=length(areas),departments=length(areas),
 facility_records=nrow(fac),mapped_facility_records=sum(mapped),facility_only_records=sum(!mapped),unmapped_facility_records=0,
 facility_departments=length(unique(fac$geo)),geography_version="Completed, documented health-district -> department crosswalk used consistently in both input joins. Outside-SURGO departments retained for coverage.",
 supply_resolution="Department + Rural/Urban where a profile exists; department-wide fallback only for absent residence profiles.",
 fallback_source_women=sum(fallback),fallback_need_women=sum(fallback&d$need),fallback_departments=sum(fallback_table$fallback_women>0),
 assigned_context_profiles=nrow(ctx),facility_residence_missing=sum(is.na(fac$rurb)),
 supply_unavailable="The cleaned three-LARC stock indicator is constant FALSE in all Senegal facilities and suppressed by the original country aggregation. Existing at-least-one-LARC stock share is used instead, explicitly labeled.",
 supply_missing="Fallback is triggered only by absence of the department-residence profile. Missing individual components in an existing profile remain NA; no component-level fallback, zero filling or partial-index averaging.",
 supply_denominators="Original UoM valid-response definitions; unweighted facility aggregation. Counseling uses s7_2 Yes/(Yes+No). Boolean join summaries retain original five-decimal rounding.",
 residence_limitation="Facility Rural/Urban is inherited from a health-district lookup, not directly recorded settlement type. Mbacke's 64 facility records are Urban-labelled; 253 Rural women therefore receive the department-wide fallback. This does not establish no rural services or individual facility attendance.",
 scoring="Department planning uses all facilities within each department. Six components per side, average-z primary; valid department reference means/SDs and strict score-median thresholds. Model demand is personal; model supply uses assigned residence/fallback inputs with fixed department normalization references.",
 weighted_need_share=wm(as.numeric(d$need[d$positive]),d$weight[d$positive]),
 use_definition="CU1 yes/sometimes plus CU5 prompts. Modern: CU4 1-8, prompted condom/ECP or explicitly contraceptive LAM. Traditional-only and confirmed nonuse are zero for modern-use models; unresolved status remains missing.",
 controls="Age and age squared, Senegal-specific education, partnership, fertility goal and woman's Rural/Urban status. No random effects or interactions.",
 source_md5=as.list(hash),source_names=as.list(basename(inputs)),
 source_files_unchanged=identical(hash,unname(tools::md5sum(inputs))))
stopifnot(audit$source_files_unchanged,
 sum(vapply(bars,function(b)if(b$field%in%c("user","intender","no_intent","unresolved"))b$n else 0,numeric(1)))==sum(d$need))
payload<-list(audit=audit,demand_variables=D,demand_labels=DL,supply_variables=S,supply_labels=SL,bars=bars,rates=rates,
 heatmap=cells,coverage=unname(coverage),scores=scores,thresholds=thresholds,component_trace=trace,planning=planning,
 profiles=profiles,assignment_summary=fallback_table,assigned_profiles=ctx)
jsonlite::write_json(payload,file.path(out,"report_data.json"),dataframe="rows",auto_unbox=TRUE,na="null",digits=12)
jsonlite::write_json(audit,file.path(out,"analysis_audit.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
saveRDS(list(women=d,supply=sp,profiles=profiles,assigned_profiles=ctx),file.path(out,"analysis_frame.rds"))
cat('Built',sum(d$need),'women in need across',length(areas),'departments; source fallback',sum(fallback),'need fallback',sum(fallback&d$need),'\n')
print(table(scores$method,scores$category));print(data.frame(field=bar_defs$field,n=vapply(bars,`[[`,numeric(1),"n"),pct=100*vapply(bars,`[[`,numeric(1),"estimate")))
