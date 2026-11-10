# Reaggregate FP providers in residence cells; use flagged LGA means for empty cells.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-arg("--input");facility_input<-arg("--facility-input");raw_facility<-arg("--raw-facility")
dictionary_file<-arg("--dictionary");data_dir<-arg("--data-dir");out<-arg("--output-dir")
script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
project_root<-normalizePath(file.path(dirname(script),"..","..",".."),mustWork=TRUE)
root<-file.path(project_root,"administrative_area")
source(file.path(root,"R/helpers.R"))
source(file.path(root,"R/aggregate_supply.R"))
source(file.path(project_root,"administrative_area/analysis/lagos/supply_definitions.R"))
key<-function(v){v<-toupper(gsub("[^A-Za-z0-9]","",as.character(v)));v[v=="IFAKOIJAIYE"]<-"IFAKOIJAYE";v}
mean_valid<-function(v){v<-as.numeric(v);if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_}
source_hash<-unname(tools::md5sum(c(input,facility_input,raw_facility)))
x<-readRDS(input);frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
stopifnot(nrow(x)==4555,identical(as.character(x$uid),d$id),identical(key(x$link_geo_join),d$geo),
 identical(as.character(x$link_rurb),as.character(x$settlement)),all(x$link_rurb%in%c("Rural","Urban")))
f<-readRDS(facility_input);f<-f[f$country=="lagos",]
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","q506","location",lagos_frequency_fields())))
ri<-match(f$fid,as.character(r$key));stopifnot(!anyNA(ri),!anyDuplicated(ri),nrow(f)==1741)
f$geo<-key(f$assigned_join_area);f$residence<-as.character(f$rurb)
raw_residence<-as.character(haven::as_factor(r$location[ri]))
stopifnot(identical(tolower(raw_residence),tolower(f$residence)))
f$context<-paste(f$geo,f$residence,sep="|");fp<-as.numeric(r$q506[ri])%in%1
f<-cbind(f,lagos_frequency(r[ri,]));eligible<-f[fp,]
stopifnot(nrow(eligible)==1040)
dictionary<-read_chars(dictionary_file);dictionary<-dictionary[dictionary$table_name=="merged_supply_demand_department",]
mapping<-c(offering="share_offer_larc_and_3_sa",free="mean_p_free_prod",training="mean_p_train",
 daily_provision="mean_daily_modern_provision",larc_stock="share_stocked_1plus_larc",stockout_burden="mean_facility_any_stockout")
aggregate_profiles<-function(group){
 a<-aggregate_supply(eligible,dictionary,group)
 a$mean_daily_modern_provision<-NA_real_;a$n_daily_modern_valid<-0L
 for(i in seq_len(nrow(a))){
  z<-eligible[eligible[[group]]==a$geo_join[i],]
  a$share_offer_larc_and_3_sa[i]<-mean_valid(bool(z$one_larc_3_sa_offer))
  a$share_stocked_1plus_larc[i]<-mean_valid(bool(z$one_larc_stockin))
  a$mean_p_free_prod[i]<-mean_valid(z$p_free_prod)
  a$mean_p_train[i]<-mean_valid(z$p_train)
  a$mean_facility_any_stockout[i]<-mean_valid(z$p_any_stockout)
  a$mean_daily_modern_provision[i]<-mean_valid(z$daily_provision)
  a$n_daily_modern_valid[i]<-sum(is.finite(z$daily_provision))
 }
 a
}
profiles<-aggregate_profiles("context");admin_profiles<-aggregate_profiles("geo")
wk<-paste(d$geo,as.character(x$link_rurb),sep="|");wi<-match(wk,profiles$geo_join)
ai<-match(d$geo,admin_profiles$geo_join)
fallback<-is.na(wi)&!is.na(ai)
joined<-x;replace<-setdiff(names(profiles),c("country","join_level","geo_join"))
for(k in replace){
 joined[[k]]<-profiles[[k]][wi]
 joined[[k]][fallback]<-admin_profiles[[k]][ai[fallback]]
}
count_fields<-replace[grepl("^n_",replace)]
for(k in count_fields)joined[[k]][is.na(wi)&!fallback]<-0
assigned_profile<-ifelse(fallback,paste(d$geo,"LGA",sep="|"),wk)
joined$link_has_supply<-!is.na(wi)|fallback;joined$link_fp_provider_only<-TRUE
joined$link_has_exact_fp_supply<-!is.na(wi);joined$link_supply_fallback<-fallback
joined$link_requested_supply_profile<-wk;joined$link_supply_profile<-assigned_profile
joined$link_supply_resolution<-ifelse(fallback,"LGA-wide fallback","LGA-residence")
joined$link_geo_mode<-"audited_fp_providers_residence_with_empty_cell_lga_fallback"
joined$link_status<-ifelse(fallback,"LGA-wide FP-provider fallback: zero exact-cell FP providers",
 ifelse(is.na(wi),"No eligible FP profile at either resolution","Matched exact LGA-residence FP profile"))
S<-names(mapping);sp<-as.data.frame(lapply(mapping,function(k)as.numeric(joined[[k]])))
names(sp)<-S
d$residence<-as.character(x$link_rurb);d$source_profile<-assigned_profile;d$fallback<-fallback
d$has_exact_fp_supply<-!is.na(wi);d$has_fp_supply<-joined$link_has_supply
observed<-unique(data.frame(context=wk,geo=d$geo,residence=d$residence,stringsAsFactors=FALSE))
observed<-observed[order(observed$geo,observed$residence),]
observed$women<-vapply(observed$context,function(k)sum(wk==k),integer(1))
observed$women_in_need<-vapply(observed$context,function(k)sum(wk==k&d$need),integer(1))
observed$source_facilities<-vapply(observed$context,function(k)sum(f$context==k),integer(1))
observed$fp_facilities<-vapply(observed$context,function(k)sum(eligible$context==k),integer(1))
observed$fallback<-vapply(observed$context,function(k)any(fallback[wk==k]),logical(1))
observed$assigned_profile<-vapply(observed$context,function(k)unique(assigned_profile[wk==k]),character(1))
observed$assignment_resolution<-ifelse(observed$fallback,"LGA-wide fallback","Exact residence")
observed$assigned_fp_facilities<-vapply(observed$context,function(k)as.integer(joined$n_facilities[match(k,wk)]),integer(1))
for(k in S){
 v<-switch(k,offering=bool(eligible$one_larc_3_sa_offer),free=as.numeric(eligible$p_free_prod),
  training=as.numeric(eligible$p_train),daily_provision=eligible$daily_provision,
  larc_stock=bool(eligible$one_larc_stockin),stockout_burden=as.numeric(eligible$p_any_stockout))
 observed[[k]]<-vapply(observed$context,function(a)sp[[k]][match(a,wk)],numeric(1))
 observed[[paste0(k,"_valid")]]<-vapply(seq_len(nrow(observed)),function(i){
  use<-if(observed$fallback[i])eligible$geo==observed$geo[i]else eligible$context==observed$context[i]
  sum(use&is.finite(v))
 },integer(1))
}
observed$complete_supply<-complete.cases(observed[S])
stopifnot(nrow(observed)==18,sum(observed$complete_supply)==18,
 sum(fallback)==157,sum(fallback&d$need)==62,all(d$has_fp_supply),
 all(d$geo[fallback]=="IBEJULEKKI"),all(d$residence[fallback]=="Urban"),
 all(is.finite(as.matrix(sp))),identical(x$uid,joined$uid),nrow(joined)==4555)
for(k in intersect(names(x),names(joined))){
 if(k%in%replace||grepl("^link_",k))next
 stopifnot(isTRUE(all.equal(x[[k]],joined[[k]],check.attributes=FALSE,tolerance=0)))
}
dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(joined,file.path(out,"lagos_women_manitoba_rural_urban_fp.rds"),compress="gzip")
saveRDS(profiles,file.path(out,"lagos_rural_urban_fp_profiles.rds"),compress="gzip")
saveRDS(admin_profiles,file.path(out,"lagos_fp_fallback_profiles.rds"),compress="gzip")
write.csv(observed,file.path(data_dir,"residence_supply_profiles_and_coverage.csv"),row.names=FALSE,na="")
write.csv(profiles,file.path(out,"lagos_rural_urban_fp_profiles.csv"),row.names=FALSE,na="")
frame$women<-d;frame$supply<-sp;frame$profiles<-observed
frame$assigned_profiles<-observed;frame$supply_resolution<-"urban_rural"
saveRDS(frame,file.path(data_dir,"analysis_frame.rds"))
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"),simplifyVector=FALSE)
p$audit$supply_resolution<-"LGA-Rural/Urban FP-provider profiles, with flagged LGA-wide fallback only when the exact cell has zero FP providers; LGA-wide planning summaries"
p$audit$model_supply_aggregation<-"Q506 Yes, valid-response equal-facility means within residence cells; zero-provider cells receive the same LGA's FP-provider means. No zero filling or component fallback when eligible exact-cell providers exist."
p$audit$observed_residence_cells<-18L;p$audit$complete_residence_profiles<-18L
p$audit$exact_residence_profiles<-17L;p$audit$fallback_residence_profiles<-1L
p$audit$unmatched_source_women<-0L;p$audit$unmatched_need_women<-0L
p$audit$fallback_source_women<-157L;p$audit$fallback_need_women<-62L
p$audit$fallback_area<-"Ibeju-Lekki Urban: one source facility, Q506 No. Use LGA-wide FP-provider means and flag the broader resolution."
p$audit$planning_resolution<-"All Q506 Yes facilities in each LGA, combining Rural and Urban, unchanged from current aggregate report."
p$audit$free_exception<-"No old Ibeju-Lekki Urban zero exception; its Q506 No facility is excluded. The full LGA-wide FP profile supplies the flagged fallback."
p$audit$exact_residence_source_md5<-as.list(source_hash)
jsonlite::write_json(p,file.path(data_dir,"report_data.json"),auto_unbox=TRUE,pretty=FALSE,na="null",null="null",digits=12)
jsonlite::write_json(p$audit,file.path(data_dir,"analysis_audit.json"),auto_unbox=TRUE,pretty=TRUE,na="null",null="null")
stopifnot(identical(source_hash,unname(tools::md5sum(c(input,facility_input,raw_facility)))))
cat("Residence join: 4555 women retained; 18 complete assigned profiles (17 exact, 1 LGA fallback); 62 need-group women use the flagged fallback.\n")
