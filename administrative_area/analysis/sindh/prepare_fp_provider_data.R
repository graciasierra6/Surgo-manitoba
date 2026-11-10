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
fac<-readRDS(facility_input);fac<-fac[fac$country=="sindh",]
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","facility_id","district","q506","q702","q507ab","q507ai",sindh_frequency_fields())))
raw_id<-as.character(r$key);empty<-is.na(raw_id)|trimws(raw_id)=="";raw_id[empty]<-as.character(r$facility_id[empty])
assert_unique(data.frame(fid=raw_id),"fid","Raw Sindh facilities")
ix<-match(fac$fid,raw_id);stopifnot(!anyNA(ix),nrow(fac)==nrow(r),nrow(fac)==4308,sum(empty)==13,
 all(is.na(r$q507ab)|trimws(r$q507ab)==""),all(is.na(r$q507ai)|trimws(r$q507ai)==""))
fp<-num(r$q506[ix]);fac$fp_provider<-fp%in%1
counsel<-num(r$q702[ix]);cf<-ifelse(counsel%in%1,1,ifelse(counsel%in%2,0,NA_real_))
stopifnot(isTRUE(all.equal(cf,fac$counselor_available,check.attributes=FALSE)))
frequency<-sindh_frequency(r[ix,]);fac<-cbind(fac,frequency)
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
key<-norm
gi<-match(key(original$link_geo_join),key(profile$geo_join))
stopifnot(!anyNA(gi),!anyDuplicated(key(profile$geo_join)),nrow(profile)==30)
replace<-setdiff(names(profile),c("country","join_level","geo_join"))
for(k in replace)joined[[k]]<-profile[[k]][gi]
joined$link_has_supply<-!is.na(gi);joined$link_fp_provider_only<-TRUE;joined$link_geo_mode<-"audited_fp_providers"
raw<-read_women(raw_women)
same<-function(a,b)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=0))
preserved<-vapply(names(raw),function(k)same(raw[[k]],joined[[k]]),logical(1))
stopifnot(nrow(joined)==10364,nrow(raw)==10364,identical(joined$col_id,original$col_id),all(preserved))
linked<-key(fac$assigned_join_area)%in%key(original$link_geo_join)
coverage<-do.call(rbind,lapply(sort(unique(fac$assigned_join_area)),function(g){
 z<-fac[fac$assigned_join_area==g,];data.frame(lga=g,source_records=nrow(z),fp_yes=sum(z$fp_provider))
}))
audit<-list(source_women=nrow(raw),original_survey_columns=length(preserved),original_fields_preserved=all(preserved),
 source_facilities=nrow(fac),fp_yes=sum(fp%in%1),fp_no=sum(fp%in%2),fp_unknown=sum(!fp%in%1:2),
 eligibility_question=attr(r$q506,"label"),eligibility_rule="Q506=1 (Yes), on-site FP services; No and unknown excluded.",
 linked_source_facilities=sum(linked),linked_fp_providers=sum(linked&fac$fp_provider),linked_districts=length(unique(gi)),
 daily_valid_linked=sum(linked&fac$fp_provider&is.finite(fac$daily_provision)),
 daily_undefined_linked=sum(linked&fac$fp_provider&!is.finite(fac$daily_provision)),
 daily_definition="Daily classes (code 1) divided by recorded offered classes (codes 1-4); code 5 Not at all excluded. Seven method classes; most frequent recorded IUD/injectable variant. No removals or sterilization. Average valid facility ratios among Q506 Yes.",
 stockout_definition="Original cleaned UoM p_any_stockout, averaged among Q506 Yes with valid values; unchanged upstream coding.",
 geography="Checked named district and explicit supplied aliases; all 4308 facilities assigned to 30 survey districts. The source-crosswalk subset omitted 881 facilities; checked assignments are used here.",
 frequency_unpopulated_fields="q507ab (hormonal IUD) and q507ai (Noristerat) are entirely blank and excluded from the selected frequency inputs; no zero/No recoding.",
 all_supply_columns_reaggregated=replace,source_files_unchanged=identical(hash,unname(tools::md5sum(inputs))))
stopifnot(audit$source_files_unchanged,audit$fp_yes==2823,audit$linked_fp_providers==2823,audit$linked_districts==30)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(joined,file.path(out,"sindh_women_manitoba_fp_providers.rds"),compress="gzip")
haven::write_dta(joined,file.path(out,"sindh_women_manitoba_fp_providers.dta"),version=15)
back<-haven::read_dta(file.path(out,"sindh_women_manitoba_fp_providers.dta"))
stopifnot(nrow(back)==nrow(raw),all(vapply(names(raw),function(k){
 a<-raw[[k]];b<-back[[k]]
 if(is.character(a)){a[is.na(a)]<-"";b[is.na(b)]<-"";identical(as.vector(a),as.vector(b))}
 else if(is.logical(a))same(as.numeric(a),as.numeric(b))else same(a,b)
},logical(1))))
saveRDS(profile,file.path(out,"sindh_fp_provider_profiles.rds"))
write.csv(profile,file.path(out,"sindh_fp_provider_profiles.csv"),row.names=FALSE,na="")
write.csv(coverage,file.path(out,"fp_provider_coverage.csv"),row.names=FALSE)
write.csv(fac[c("fid","assigned_join_area","fp_provider",names(frequency))],file.path(out,"daily_provision_facility_audit.csv"),row.names=FALSE,na="")
jsonlite::write_json(audit,file.path(out,"dataset_verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Verified original women; reaggregated supply among",audit$linked_fp_providers,"FP facilities across 30 linked districts.\n")
