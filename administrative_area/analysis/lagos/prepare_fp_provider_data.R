# Restrict supply summaries to confirmed FP providers, preserving original women.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-arg("--input");facility_input<-arg("--facility-input");raw_facility<-arg("--raw-facility")
raw_women<-arg("--raw-women");dictionary_file<-arg("--dictionary");out<-arg("--output-dir")
script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
project_root<-normalizePath(file.path(dirname(script),"..","..",".."),mustWork=TRUE)
root<-file.path(project_root,"administrative_area")
source(file.path(root,"R/helpers.R"));source(file.path(root,"R/aggregate_supply.R"))
source(file.path(dirname(script),"supply_definitions.R"))
inputs<-c(input,facility_input,raw_facility,raw_women,dictionary_file);hash<-unname(tools::md5sum(inputs))
fac<-readRDS(facility_input);fac<-fac[fac$country=="lagos",]
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","q506","q702",lagos_frequency_fields())))
assert_unique(data.frame(fid=as.character(r$key)),"fid","Raw Lagos facilities")
ix<-match(fac$fid,as.character(r$key));stopifnot(!anyNA(ix),nrow(fac)==nrow(r),nrow(fac)==1741)
fp<-num(r$q506[ix]);fac$fp_provider<-fp%in%1
counsel<-num(r$q702[ix]);cf<-ifelse(counsel%in%1,1,ifelse(counsel%in%2,0,NA_real_))
stopifnot(isTRUE(all.equal(cf,fac$counselor_available,check.attributes=FALSE)))
frequency<-lagos_frequency(r[ix,]);fac<-cbind(fac,frequency)
eligible<-fac[fac$fp_provider,]
dictionary<-read_chars(dictionary_file);dictionary<-dictionary[dictionary$table_name=="merged_supply_demand_department",]
profile<-aggregate_supply(eligible,dictionary,"assigned_join_area")
# Use full precision for the selected six components; presentation rounding happens later.
profile$mean_daily_modern_provision<-profile$mean_weekly_modern_provision<-NA_real_
profile$n_daily_modern_valid<-profile$n_frequency_incomplete<-0L
for(j in seq_len(nrow(profile))){
 z<-eligible[eligible$assigned_join_area==profile$geo_join[j],]
 mean_valid<-function(v){v<-as.numeric(v);if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_}
 profile$share_offer_larc_and_3_sa[j]<-mean_valid(bool(z$one_larc_3_sa_offer))
 profile$share_stocked_1plus_larc[j]<-mean_valid(bool(z$one_larc_stockin))
 profile$mean_daily_modern_provision[j]<-mean_valid(z$daily_provision)
 profile$mean_weekly_modern_provision[j]<-mean_valid(z$at_least_weekly_provision)
 profile$n_daily_modern_valid[j]<-sum(is.finite(z$daily_provision))
 profile$n_frequency_incomplete[j]<-sum(z$frequency_unknown_categories>0)
}
original<-readRDS(input);joined<-original
key<-function(v){v<-toupper(gsub("[^A-Za-z0-9]","",as.character(v)));v[v%in%"IFAKOIJAIYE"]<-"IFAKOIJAYE";v}
gi<-match(key(original$link_geo_join),key(profile$geo_join))
stopifnot(!anyNA(gi),!anyDuplicated(key(profile$geo_join)),nrow(profile)==20)
replace<-setdiff(names(profile),c("country","join_level","geo_join"))
for(k in replace)joined[[k]]<-profile[[k]][gi]
joined$link_has_supply<-!is.na(gi);joined$link_fp_provider_only<-TRUE;joined$link_geo_mode<-"audited_fp_providers"
raw<-read_women(raw_women)
same<-function(a,b)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=0))
preserved<-vapply(names(raw),function(k)same(raw[[k]],joined[[k]]),logical(1))
stopifnot(nrow(joined)==4555,nrow(raw)==4555,identical(joined$uid,original$uid),all(preserved))
linked<-key(fac$assigned_join_area)%in%key(original$link_geo_join)
coverage<-do.call(rbind,lapply(sort(unique(fac$assigned_join_area)),function(g){
 z<-fac[fac$assigned_join_area==g,];data.frame(lga=g,source_records=nrow(z),fp_yes=sum(z$fp_provider))
}))
exception<-key(fac$assigned_join_area)=="IBEJULEKKI"&fac$rurb%in%"Urban"
stopifnot(sum(exception)==1,fp[exception]==2,is.na(fac$p_free_prod[exception]))
audit<-list(source_women=nrow(raw),original_survey_columns=length(preserved),original_fields_preserved=all(preserved),
 source_facilities=nrow(fac),fp_yes=sum(fp%in%1),fp_no=sum(fp%in%2),fp_unknown=sum(!fp%in%1:2),
 eligibility_question=attr(r$q506,"label"),eligibility_rule="Q506=1 (Yes), on-site FP services; No and unknown excluded.",
 linked_source_facilities=sum(linked),linked_fp_providers=sum(linked&fac$fp_provider),linked_lgas=length(unique(gi)),
 daily_valid_linked=sum(linked&fac$fp_provider&is.finite(fac$daily_provision)),
 daily_undefined_linked=sum(linked&fac$fp_provider&!is.finite(fac$daily_provision)),
 daily_definition="Daily classes (code 1) divided by recorded offered classes (codes 1-4); code 5 Not at all excluded. Seven method classes; most frequent recorded IUD/injectable variant. No removals or sterilization. Average valid facility ratios among Q506 Yes.",
 stockout_definition="Original cleaned UoM p_any_stockout, averaged among Q506 Yes with valid values; unchanged upstream coding.",
 zero_exception="The prior Ibeju-Lekki Urban free-product exception concerns a Q506 No facility, which is excluded. No undefined free-product value is recoded in this update.",
 all_supply_columns_reaggregated=replace,source_files_unchanged=identical(hash,unname(tools::md5sum(inputs))))
stopifnot(audit$source_files_unchanged,audit$fp_yes==1040,audit$linked_fp_providers==882,audit$linked_lgas==17)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(joined,file.path(out,"lagos_women_manitoba_fp_providers.rds"),compress="gzip")
haven::write_dta(joined,file.path(out,"lagos_women_manitoba_fp_providers.dta"),version=15)
back<-haven::read_dta(file.path(out,"lagos_women_manitoba_fp_providers.dta"))
stopifnot(nrow(back)==nrow(raw),all(vapply(names(raw),function(k){
 a<-raw[[k]];b<-back[[k]]
 if(is.character(a)){a[is.na(a)]<-"";b[is.na(b)]<-"";identical(as.vector(a),as.vector(b))}
 else if(is.logical(a))same(as.numeric(a),as.numeric(b))else same(a,b)
},logical(1))))
saveRDS(profile,file.path(out,"lagos_fp_provider_profiles.rds"))
write.csv(profile,file.path(out,"lagos_fp_provider_profiles.csv"),row.names=FALSE,na="")
write.csv(coverage,file.path(out,"fp_provider_coverage.csv"),row.names=FALSE)
write.csv(fac[c("fid","assigned_join_area","fp_provider",names(frequency))],file.path(out,"daily_provision_facility_audit.csv"),row.names=FALSE,na="")
jsonlite::write_json(audit,file.path(out,"dataset_verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Verified original women; reaggregated supply among",audit$linked_fp_providers,"FP facilities across 17 linked LGAs.\n")
