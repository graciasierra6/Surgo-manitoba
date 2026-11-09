# Côte d’Ivoire: department-wide facility profiles; original SURGO FPN2.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
optional<-function(k,default){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
input<-opt("--input");admin_input<-input;facility_file<-opt("--facility-input")
facility_sample<-optional("--facility-sample","all");demand_sample<-optional("--demand-sample","available");reference_dir<-optional("--reference-dir","")
stopifnot(facility_sample%in%c("all","complete"),demand_sample%in%c("available","complete"))
raw_file<-opt("--raw");raw_facility<-opt("--raw-facility");codebook<-opt("--district-codebook");out<-opt("--output-dir")
for(pkg in c("survey","jsonlite","haven"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
here<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
source(file.path(here,"..","..","..","analysis","cotedivoire","cdi_definitions.R"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
inputs<-c(input,admin_input,facility_file,raw_file,raw_facility,codebook)
hash<-unname(tools::md5sum(inputs));savecsv<-function(v,name)write.csv(v,file.path(out,name),row.names=FALSE,na="")
key<-cdi_key;lab<-function(v)if(inherits(v,"haven_labelled"))as.character(haven::as_factor(v))else as.character(v)
valid_mean<-function(v)if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_
wm<-function(v,w){ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))weighted.mean(v[ok],w[ok])else NA_real_}
bool<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
x<-readRDS(input);admin<-readRDS(admin_input)
stopifnot(nrow(x)==10859,nrow(admin)==nrow(x),identical(x$IDW,admin$IDW),
 identical(as.character(x$link_geo_join),as.character(admin$link_geo_join)))
# Residence is retained from the woman survey for adjustment, never as a supply key.
x$link_rurb<-ifelse(as.numeric(x$FV_MILIEU)==1,"Rural",ifelse(as.numeric(x$FV_MILIEU)==2,"Urban",NA_character_))
d<-cdi_definitions(x);domains<-cdi_domains();D<-domains$D;DL<-domains$DL;S<-domains$S;SL<-domains$SL
sp<-cdi_supply(x);d$source_profile<-ifelse(is.na(d$geo),NA_character_,d$geo)
d$matched_supply<-as.logical(x$link_has_supply)
# Do not mutate any joined supply value or replace a missing profile with zero.
analysis_join<-x
analysis_join$analysis_supply_source_key<-d$source_profile
analysis_join$analysis_supply_match_level<-ifelse(x$link_has_supply,"Department","Unmatched")
analysis_join$analysis_facility_residence_basis<-"Recorded facility s1_5 residence"
saveRDS(analysis_join,file.path(out,"cotedivoire_women_manitoba_analysis.rds"))
profiles<-unique(cbind(geo=key(admin$link_geo_join),district=as.character(admin$link_geo_join),
 cdi_supply(admin),n_facilities=as.numeric(admin$n_facilities)))
profiles<-profiles[!is.na(profiles$geo),]
stopifnot(!anyDuplicated(profiles$geo),nrow(profiles)==82,sum(d$positive)==10753,sum(d$need)==4292)
ctx<-unique(cbind(d[c("geo","district","residence","source_profile","matched_supply")],sp,n_facilities=as.numeric(x$n_facilities)))
ctx<-ctx[!is.na(ctx$geo),]
stopifnot(!anyDuplicated(paste(ctx$geo,ctx$residence)),nrow(ctx)==131,sum(ctx$matched_supply)==126)
savecsv(ctx,"assigned_supply_profiles.csv")
fac<-readRDS(facility_file);fac<-fac[fac$country=="cotedivoire",];fac$geo<-key(fac$assigned_join_area)
areas<-sort(profiles$geo);names_map<-setNames(profiles$district,profiles$geo);mapped<-fac$geo%in%areas
# Independently restore raw health-district names and check the explicit department rollups.
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("fid","s1_2","s1_5","s7_2","s2_5_sp")))
ri<-match(fac$fid,lab(r$fid));stopifnot(!anyNA(ri))
cb<-read.csv(codebook,stringsAsFactors=FALSE);ci<-match(as.numeric(r$s1_2[ri]),as.numeric(cb$code));stopifnot(!anyNA(ci))
restored<-cb$label[ci]
stopifnot(all(key(restored)==key(fac$corrected_source_geo_name)))
expected_geo<-key(restored);codes<-as.numeric(r$s1_2[ri]);expected_geo[codes%in%1:10]<-"ABIDJAN"
aliases<-c(KORHOGO1="KORHOGO",KORHOGO2="KORHOGO",GAGNOA1="GAGNOA",GAGNOA2="GAGNOA",
 BOUAKENORDEST="BOUAKE",BOUAKENORDOUEST="BOUAKE",BOUAKESUD="BOUAKE",TOULEUPLEU="TOULEPLEU")
ix<-match(expected_geo,names(aliases));expected_geo[!is.na(ix)]<-aliases[ix[!is.na(ix)]]
stopifnot(all(expected_geo==fac$geo))
res<-as.numeric(r$s1_5[ri]);rr<-ifelse(res%in%1,"Rural",ifelse(res%in%2,"Urban",NA_character_))
stopifnot(identical(rr,as.character(fac$rurb)))
counsel<-as.numeric(r$s7_2[ri]);cf<-ifelse(counsel%in%1,1,ifelse(counsel%in%2,0,NA_real_))
stopifnot(isTRUE(all.equal(cf,fac$counselor_available,check.attributes=FALSE)))
fac$offering<-bool(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
fac$training<-as.numeric(fac$p_train);fac$counselor<-as.numeric(fac$counselor_available)
fac$larc_stock<-bool(fac$one_larc_stockin);fac$popular_stock<-bool(fac$mp_in_stock)
stopifnot(all(bool(fac$three_larc_stockin)==0),nrow(fac)==3078,length(unique(fac$geo))==100)
# All six supply measures, including counseling, define facility completeness.
fac$complete_supply<-complete.cases(fac[S]) & apply(fac[S],1,function(v)all(is.finite(v)))
savecsv(data.frame(geo=areas,department=names_map[areas],
 total_facilities=vapply(areas,function(g)sum(fac$geo%in%g),integer(1)),
 complete_facilities=vapply(areas,function(g)sum(fac$geo%in%g & fac$complete_supply),integer(1))),"facility_complete_case_coverage.csv")
stopifnot(sum(mapped & fac$complete_supply)==1173)
if(facility_sample=="complete"){
 for(g in areas){ff<-fac[fac$geo%in%g & fac$complete_supply,]
  for(k in S)profiles[[k]][profiles$geo==g]<-valid_mean(ff[[k]])
  profiles$n_facilities[profiles$geo==g]<-nrow(ff)
 }
 for(k in S)sp[[k]]<-profiles[[k]][match(d$geo,profiles$geo)]
 ctx<-unique(cbind(d[c("geo","district","residence","source_profile","matched_supply")],sp,n_facilities=profiles$n_facilities[match(d$geo,profiles$geo)]))
 ctx<-ctx[!is.na(ctx$geo),]
}
for(k in S)stopifnot(all(vapply(split(sp[[k]],d$geo),function(v)length(unique(v))==1,logical(1))))
savecsv(ctx,"assigned_supply_profiles.csv")
reconcile<-list()
check_profile<-function(ff,pp,scope,area,residence){
 stopifnot(nrow(ff)==pp$n_facilities)
 for(k in S){actual<-valid_mean(ff[[k]]);expected<-pp[[k]]
  passed<-if(is.na(actual)||is.na(expected))is.na(actual)&&is.na(expected)else abs(actual-expected)<=5.00001e-6
  reconcile[[length(reconcile)+1L]]<<-data.frame(scope=scope,geo=area,residence=residence,variable=k,
   facility_recalculation=actual,joined_value=expected,n_valid=sum(is.finite(ff[[k]])),n_facilities=nrow(ff),passed=passed)
 }
}
for(g in areas)check_profile(fac[fac$geo%in%g & (facility_sample=="all" | fac$complete_supply),],profiles[profiles$geo==g,],"Department",g,"All")
for(j in seq_len(nrow(ctx))){q<-ctx[j,];fi<-fac$geo%in%q$geo & (facility_sample=="all" | fac$complete_supply)
 check_profile(fac[fi,],q,"Department assigned to woman",q$geo,q$residence)
}
reconcile<-do.call(rbind,reconcile);stopifnot(all(reconcile$passed));savecsv(reconcile,"supply_input_reconciliation.csv")
assignment<-data.frame(row=d$row,IDW=d$id,department=d$district,residence=d$residence,in_need=d$need,
 exact_profile_available=d$matched_supply,supply_source_key=d$source_profile,assigned_facilities=profiles$n_facilities[match(d$geo,profiles$geo)])
saveRDS(assignment,file.path(out,"woman_assignment_audit.rds"))
assignment_table<-do.call(rbind,lapply(areas,function(g){ff<-fac[fac$geo==g,];w<-d$geo%in%g
 data.frame(geo=g,department=names_map[[g]],source_women=sum(w),rural_women=sum(w&d$residence%in%"Rural"),
 urban_women=sum(w&d$residence%in%"Urban"),need_women=sum(w&d$need),rural_need=sum(w&d$need&d$residence%in%"Rural"),
 urban_need=sum(w&d$need&d$residence%in%"Urban"),unmatched_women=sum(w&!d$matched_supply),unmatched_need=sum(w&!d$matched_supply&d$need),
 facility_records=nrow(ff),rural_facilities=sum(ff$rurb%in%"Rural"),urban_facilities=sum(ff$rurb%in%"Urban"))
}));savecsv(assignment_table,"department_residence_assignment_summary.csv")
issues<-do.call(rbind,lapply(seq_len(nrow(ctx)),function(i){q<-ctx[i,];missing<-S[!is.finite(as.numeric(q[S]))]
 if(!length(missing))return(NULL)
 w<-d$geo%in%q$geo&d$residence%in%q$residence
 data.frame(geo=q$geo,residence=q$residence,matched=q$matched_supply,n_facilities=q$n_facilities,
 source_women=sum(w),need_women=sum(w&d$need),missing_components=paste(missing,collapse="; "),
 reason=if(!q$matched_supply)"No included facility profile in this department"else"Matched facilities but component has no valid reporting denominator")
}));savecsv(issues,"supply_assignment_issues.csv")
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
bars<-lapply(seq_len(nrow(bar_defs)),function(i)c(as.list(bar_defs[i,]),estimate(need_design,bar_defs$field[i])))
rates<-lapply(areas,function(g){a<-subset(need_design,geo==g);list(geo=g,name=names_map[[g]],n=nrow(a$variables),
 cpr=estimate(a,"user"),mcpr=estimate(a,"modern"),intender=estimate(a,"intender"),no_intent=estimate(a,"no_intent"))})
dn<-d[d$need,];cells<-list();coverage<-list()
dn_complete<-complete.cases(dn[D]);stopifnot(sum(dn_complete)==3954)
savecsv(do.call(rbind,lapply(c("CDI",areas),function(g){ii<-if(g=="CDI")rep(TRUE,nrow(dn))else dn$geo%in%g
 data.frame(geo=g,department=if(g=="CDI")"Côte d’Ivoire"else names_map[[g]],women_in_need=sum(ii),complete_demand=sum(ii & dn_complete),
 excluded=sum(ii & !dn_complete),complete_weight_share=sum(dn$weight[ii & dn_complete])/sum(dn$weight[ii]))})),"demand_complete_case_coverage.csv")
for(g in c("CDI",areas)){
 wi<-if(g=="CDI")rep(TRUE,nrow(dn)) else dn$geo%in%g
 fi<-mapped & (if(g=="CDI")rep(TRUE,nrow(fac)) else fac$geo%in%g) & (facility_sample=="all" | fac$complete_supply)
 coverage[[g]]<-list(geo=g,name=if(g=="CDI")"Côte d’Ivoire" else names_map[[g]],women=sum(d$positive & if(g=="CDI")rep(TRUE,nrow(d)) else d$geo%in%g),
  all_source_women=if(g=="CDI")nrow(d) else sum(d$geo%in%g),n_need=sum(wi),facilities=sum(mapped & (if(g=="CDI")rep(TRUE,nrow(fac))else fac$geo%in%g)))
 for(j in seq_along(D)){k<-D[j];ok<-wi & is.finite(dn[[k]]) & (demand_sample=="available" | dn_complete)
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Demand",variable=k,label=DL[j],value=100*wm(dn[[k]][ok],dn$weight[ok]),
    n_valid=sum(ok),n_total=sum(wi),numerator=sum(dn[[k]][ok]==1))}
 for(j in seq_along(S)){k<-S[j];v<-if(g=="CDI")valid_mean(fac[[k]][fi]) else profiles[[k]][match(g,profiles$geo)]
  cells[[length(cells)+1L]]<-data.frame(geo=g,side="Supply",variable=k,label=SL[j],value=100*v,
    n_valid=sum(fi & is.finite(fac[[k]])),n_total=sum(fi),numerator=sum(fac[[k]][fi],na.rm=TRUE))}
}
for(g in setdiff(sort(unique(fac$geo[!is.na(fac$geo)])),areas)) {
 coverage[[g]]<-list(geo=g,name=fac$assigned_join_area[match(g,fac$geo)],women=0,
  all_source_women=0,n_need=0,facilities=sum(fac$geo==g))
}
cells<-do.call(rbind,cells);savecsv(cells,"heatmap_values_and_denominators.csv")
primary_trace<-if(nzchar(reference_dir))read.csv(file.path(reference_dir,"component_score_trace.csv"),stringsAsFactors=FALSE)else NULL
primary_thresholds<-if(nzchar(reference_dir))read.csv(file.path(reference_dir,"classification_thresholds.csv"),stringsAsFactors=FALSE)else NULL
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
if(!is.null(primary_trace)){
 # Hold references and thresholds fixed within a comparison using the same domains.
 for(side in c("Demand","Supply")){
  fields<-if(side=="Demand")D else S
  for(k in fields){rr<-primary_trace[primary_trace$side==side & primary_trace$variable==k,];ii<-trace$side==side & trace$variable==k
   stopifnot(nrow(rr)==length(areas))
   trace$indicator_mean[ii]<-unique(rr$indicator_mean);trace$indicator_SD[ii]<-unique(rr$indicator_SD)
   trace$zscore[ii]<-(trace$original_pct[ii]-unique(rr$indicator_mean))/unique(rr$indicator_SD)
  }
  value<-vapply(areas,function(g)mean(trace$zscore[trace$side==side & trace$geo==g]),numeric(1))
  cut<-primary_thresholds[primary_thresholds$method=="zscore",if(side=="Demand")"demand_cutoff"else"supply_cutoff"]
  ix<-which(scores$method=="zscore")
  if(side=="Demand"){
   scores$demand_score[ix]<-value[match(scores$geo[ix],areas)]
   scores$demand_flag[ix]<-scores$demand_score[ix]>cut+1e-10
   scores$demand_priority_rank[ix]<-rank(-scores$demand_score[ix],ties.method="average")
   thresholds$demand_cutoff[thresholds$method=="zscore"]<-cut
  }else{
   scores$supply_score[ix]<-value[match(scores$geo[ix],areas)]
   scores$supply_flag[ix]<-scores$supply_score[ix]<cut-1e-10
   scores$supply_priority_rank[ix]<-rank(scores$supply_score[ix],ties.method="average")
   thresholds$supply_cutoff[thresholds$method=="zscore"]<-cut
  }
 }
 scores$category[ix]<-classify(scores$demand_flag[ix],scores$supply_flag[ix])
}
planning<-lapply(areas,function(g){q<-scores[scores$method=="zscore" & scores$geo==g,];dt<-trace[trace$geo==g & trace$side=="Demand",];st<-trace[trace$geo==g & trace$side=="Supply",]
 list(geo=g,name=names_map[[g]],category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,demand_score=q$demand_score,supply_score=q$supply_score,
  top_demand=head(dt$variable[order(-dt$zscore,match(dt$variable,D),na.last=TRUE)],3),
  top_supply=if(all(is.finite(st$zscore)))head(st$variable[order(st$zscore,match(st$variable,S))],3) else character(),
  n_need=sum(dn$geo==g),facility_count=profiles$n_facilities[match(g,profiles$geo)])})
missing<-do.call(rbind,lapply(areas,function(g)do.call(rbind,lapply(D,function(k){a<-dn[dn$geo==g,];data.frame(geo=g,variable=k,n=nrow(a),valid=sum(is.finite(a[[k]])),missing=sum(!is.finite(a[[k]])),weighted_pct=100*wm(a[[k]],a$weight))}))))
savecsv(missing,"demand_missingness_by_department.csv")
savecsv(scores,"all_method_scores_and_classifications.csv");savecsv(thresholds,"classification_thresholds.csv");savecsv(trace,"component_score_trace.csv")
savecsv(data.frame(geo=trace$geo,side=trace$side,variable=trace$variable,original_pct_exact=sprintf("%.17g",trace$original_pct)),"scoring_inputs_exact.csv")
savecsv(do.call(rbind,lapply(bars,as.data.frame)),"use_distribution.csv")
savecsv(do.call(rbind,lapply(rates,function(r)data.frame(geo=r$geo,name=r$name,n_need=r$n,cpr=r$cpr$estimate,mcpr=r$mcpr$estimate))),"department_use_rates.csv")
savecsv(profiles,"department_supply_profiles.csv")
comparison<-data.frame(geo=areas,name=names_map[areas]);for(id in c("rank","raw","robust","median_count","zscore"))comparison[[id]]<-scores$category[scores$method==id][match(areas,scores$geo[scores$method==id])]
savecsv(comparison,"classification_comparison.csv")
plan_table<-do.call(rbind,lapply(planning,function(q)data.frame(department=q$name,category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,
 top_demand=paste(q$top_demand,collapse="; "),top_supply=paste(q$top_supply,collapse="; "),n_need=q$n_need,facility_count=q$facility_count)))
savecsv(plan_table,"planning_priorities.csv")
audit<-list(country="Côte d’Ivoire",source_rows=nrow(x),consenting_positive_weight=sum(d$positive),need_n=sum(d$need),
 need_variable="Original SURGO FPN2 = 1, unchanged. No old need_n reconstruction or permanent-protection additions.",
 need_qualification="Original SURGO current-need classification, not the DHS unmet-need algorithm.",
 weight_field="WEIGHT",psu_field="IDZD: documented unique census-district / enumeration-zone identifier",
 survey_design="Final WEIGHT and recorded IDZD clusters. Construct the full consenting positive-weight frame before selecting FPN2=1. Original strata and FPC are not documented; no identifiers inferred from household or woman IDs.",
 survey_psus=length(unique(d$psu[d$positive])),districts=length(areas),departments=length(areas),
 facility_records=nrow(fac),mapped_facility_records=sum(mapped),facility_only_records=sum(!mapped),unmapped_facility_records=0,
 facility_departments=length(unique(fac$geo)),geography_version="Completed documented health-district to SURGO department-equivalent crosswalk. Raw health-district codes 1-13 restored from supplied companion codebook; explicit Abidjan/Bouake/Gagnoa/Korhogo rollups checked.",
 supply_resolution="Department-wide facility summaries assigned to all women in the same department-equivalent. No residence split or neighboring-area fallback.",
 facility_sample=facility_sample,demand_sample=demand_sample,counseling_included=TRUE,primary_reference_fixed=nzchar(reference_dir),
 facility_records_used=if(facility_sample=="all")sum(mapped)else sum(mapped & fac$complete_supply),complete_demand_women=sum(dn_complete),
 matched_source_women=sum(d$matched_supply),unmatched_source_women=sum(!d$matched_supply),unmatched_need_women=sum(d$need&!d$matched_supply),
 assigned_context_profiles=length(areas),woman_residence_cells=nrow(ctx),matched_context_profiles=sum(vapply(areas,function(g)any(fac$geo%in%g),logical(1))),
 complete_supply_need_women=sum(d$need&complete.cases(sp)),facility_residence_missing=sum(is.na(fac$rurb)),
 supply_unavailable="Three-LARC stock indicator is constant FALSE in the cleaned source; use the already joined at-least-one-LARC stock indicator instead, explicitly labeled.",
 supply_missing="Four department-equivalents lack facility matches: Bonon, Facobly, Gohitafla and Taabo. Daoukro free provision remains undefined at department level. No unknown is coded zero, no partial score or geographic fallback is used. The complete-facility check retains unknown profiles where no complete facility exists.",
 supply_denominators="Original unweighted UoM valid-response definitions. s7_2 counseling: 1=Yes, 2=No, other/unknown excluded. Boolean join summaries retain five-decimal rounding.",
 residence_limitation="s1_5 records facility Rural/Urban directly. Geographic linkage describes the local recorded service environment; it does not identify the facility attended by a woman or prove absence of services in unmatched departments.",
 scoring="Planning uses all facilities in each department and weighted valid-domain demand percentages among original FPN2=1 women. Model demand is individual; every woman receives her department-wide supply profile. Sensitivity checks hold primary normalization and thresholds fixed; no residence-specific profiles enter.",
 weighted_need_share=wm(as.numeric(d$need[d$positive]),d$weight[d$positive]),
 use_definition="CU1 yes/sometimes plus CU5 prompts. Modern: CU4 1-8, prompted condoms/ECP or explicitly contraceptive LAM. Confirmed traditional-only and nonuse=0; unresolved outcomes stay missing.",
 controls="Age and age squared, Côte d’Ivoire education, partnership, fertility goal and woman Rural/Urban. No random effects or interactions.",
 daoukro_check="All 21 facilities have zero recorded free and nonfree product counts, hence undefined p_free_prod; 19 nonetheless report providing FP services in raw s2_5_sp. Requires source-question/coding clarification; no repair assumed.",
 source_md5=as.list(hash),source_names=as.list(basename(inputs)),source_files_unchanged=identical(hash,unname(tools::md5sum(inputs))))
stopifnot(audit$source_files_unchanged,sum(d$matched_supply)==10703,
 sum(vapply(bars,function(b)if(b$field%in%c("user","intender","no_intent","unresolved"))b$n else 0,numeric(1)))==sum(d$need))
payload<-list(audit=audit,demand_variables=D,demand_labels=DL,supply_variables=S,supply_labels=SL,bars=bars,rates=rates,
 heatmap=cells,coverage=unname(coverage),scores=scores,thresholds=thresholds,component_trace=trace,planning=planning,
 profiles=profiles,assignment_summary=assignment_table,assigned_profiles=ctx,assignment_issues=issues)
jsonlite::write_json(payload,file.path(out,"report_data.json"),dataframe="rows",auto_unbox=TRUE,na="null",digits=12)
jsonlite::write_json(audit,file.path(out,"analysis_audit.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
saveRDS(list(women=d,supply=sp,profiles=profiles,assigned_profiles=ctx),file.path(out,"analysis_frame.rds"))
cat('Built',sum(d$need),'women in need across',length(areas),'departments; complete assigned supply',sum(d$need&complete.cases(sp)),'facility sample',facility_sample,'demand sample',demand_sample,'\n')
print(table(scores$method,scores$category));print(data.frame(field=bar_defs$field,n=vapply(bars,`[[`,numeric(1),"n"),pct=100*vapply(bars,`[[`,numeric(1),"estimate")))
