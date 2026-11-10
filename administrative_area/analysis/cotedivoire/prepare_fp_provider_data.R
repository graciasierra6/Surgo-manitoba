# Reaggregate all supply columns among confirmed FP providers; retain every woman.
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k) {i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input <- arg("--input"); facility_input <- arg("--facility-input")
raw_facility <- arg("--raw-facility"); raw_women <- arg("--raw-women")
dictionary_file <- arg("--dictionary"); out <- arg("--output-dir")
script <- gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
project_root <- normalizePath(file.path(dirname(script),"..","..",".."),mustWork=TRUE)
root <- file.path(project_root,"administrative_area")
source(file.path(root,"R/helpers.R"));source(file.path(root,"R/aggregate_supply.R"))
source(file.path(project_root,"analysis/cotedivoire/cdi_definitions.R"))
source(file.path(dirname(script),"supply_definitions.R"))
inputs<-c(input,facility_input,raw_facility,raw_women,dictionary_file)
hashes<-unname(tools::md5sum(inputs))
all_fac<-readRDS(facility_input);fac<-all_fac[all_fac$country=="cotedivoire",]
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("fid","s2_5_sp","s5_1","s7_2",cdi_frequency_fields())))
assert_unique(data.frame(fid=lab(r$fid)),"fid","Raw Côte d’Ivoire facilities")
ri<-match(fac$fid,lab(r$fid));stopifnot(!anyNA(ri),nrow(fac)==nrow(r))
fp<-num(r$s2_5_sp[ri]);counsel<-num(r$s7_2[ri])
onsite<-num(r$s5_1[ri])
expected<-ifelse(counsel%in%1,1,ifelse(counsel%in%2,0,NA_real_))
stopifnot(isTRUE(all.equal(expected,fac$counselor_available,check.attributes=FALSE)))
fac$counselor_available<-expected;fac$fp_provider<-fp%in%1
frequency<-cdi_frequency(r[ri,])
fac<-cbind(fac,frequency)
eligible<-fac[fac$fp_provider,]
dictionary<-read_chars(dictionary_file)
dictionary<-dictionary[dictionary$table_name=="merged_supply_demand_department",]
profile<-aggregate_supply(eligible,dictionary,"assigned_join_area")
for(k in c("mean_daily_modern_provision","mean_weekly_modern_provision"))profile[[k]]<-NA_real_
profile$n_daily_modern_valid<-profile$n_frequency_incomplete<-0L
for(i in seq_len(nrow(profile))){
  z<-eligible[eligible$assigned_join_area==profile$geo_join[i],]
  vv<-z$daily_provision;profile$mean_daily_modern_provision[i]<-if(any(is.finite(vv)))mean(vv[is.finite(vv)])else NA_real_
  vv<-z$at_least_weekly_provision;profile$mean_weekly_modern_provision[i]<-if(any(is.finite(vv)))mean(vv[is.finite(vv)])else NA_real_
  profile$n_daily_modern_valid[i]<-sum(is.finite(z$daily_provision))
  profile$n_frequency_incomplete[i]<-sum(z$frequency_unknown_categories>0)
}
stopifnot(nrow(profile)==100,sum(profile$n_facilities)==sum(fp%in%1))
original<-readRDS(input);joined<-original
ix<-match(norm(original$link_geo_join),norm(profile$geo_join))
stopifnot(!anyDuplicated(norm(profile$geo_join)))
replace<-setdiff(names(profile),c("country","join_level","geo_join"))
for(k in replace) joined[[k]]<-profile[[k]][ix]
joined$n_facilities<-ifelse(is.na(ix),0,profile$n_facilities[ix])
joined$link_has_supply<-!is.na(ix)
joined$link_geo_mode<-"audited_fp_providers"
joined$link_fp_provider_only<-TRUE
raw<-haven::read_dta(raw_women)
same<-function(a,b)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=0))
preserved<-vapply(names(raw),function(k)same(raw[[k]],joined[[k]]),logical(1))
stopifnot(nrow(joined)==nrow(raw),identical(original$IDW,joined$IDW),all(preserved))
areas<-unique(norm(original$link_geo_join));linked<-norm(fac$assigned_join_area)%in%areas
coverage<-do.call(rbind,lapply(sort(unique(fac$assigned_join_area)),function(g){
 z<-fac[fac$assigned_join_area==g,];selected<-z$fp_provider
 data.frame(departement=g,all_records=nrow(z),fp_yes=sum(selected),
  counselor_yes=sum(selected&z$counselor_available%in%1),
  counselor_no=sum(selected&z$counselor_available%in%0),
  counselor_unknown=sum(selected&is.na(z$counselor_available)))
}))
audit<-list(source_women=nrow(raw),original_survey_columns=length(preserved),
 original_fields_preserved=all(preserved),source_facilities=nrow(fac),
 fp_yes=sum(fp%in%1),fp_no=sum(fp%in%c(0,2)),fp_unknown=sum(!fp%in%c(0,1,2)),
 eligibility_question=attr(r$s2_5_sp,"label"),
 eligibility_scope="General FP-service provision, s2_5_sp=1. The separate s5_1 on-site provision response is diagnostic only, not an additional eligibility filter.",
 onsite_yes_among_general_fp=sum(fp%in%1 & onsite%in%1),
 onsite_no_among_general_fp=sum(fp%in%1 & onsite%in%2),
 linked_source_facilities=sum(linked),linked_fp_providers=sum(linked&fac$fp_provider),
 counselor_question=attr(r$s7_2,"label"),counselor_yes=sum(linked&fac$fp_provider&expected%in%1),
 counselor_no=sum(linked&fac$fp_provider&expected%in%0),
 counselor_unknown=sum(linked&fac$fp_provider&is.na(expected)),
 counselor_definition="s7_2: 1 Yes; 2 No; all other values unknown. Availability is not a measure of training, receipt or quality.",
 daily_provision_definition="s5_2a-g variants: combine injectable 2m/3m and implant 3y/5y using most frequent recorded offered variant; count Daily (1) / count offered categories (1-4); code 5 Not at all excluded from the offered-method denominator; unknown responses excluded, not zero. Mean valid facility ratios among s2_5_sp=1.",
 daily_valid_linked=sum(linked&fac$fp_provider&is.finite(fac$daily_provision)),
 daily_undefined_linked=sum(linked&fac$fp_provider&!is.finite(fac$daily_provision)),
 stockout_definition="Original cleaned UoM p_any_stockout retained without recoding; valid facility means among s2_5_sp=1. Higher raw values are worse; scoring reverses direction.",
 all_supply_columns_reaggregated=replace,linked_departements=length(unique(ix[!is.na(ix)])),
 source_files_unchanged=identical(hashes,unname(tools::md5sum(inputs))))
stopifnot(audit$source_files_unchanged,audit$fp_yes==2723,audit$linked_fp_providers==2370,
 audit$linked_departements==78,audit$onsite_yes_among_general_fp==2614,
 audit$onsite_no_among_general_fp==109)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(joined,file.path(out,"cotedivoire_women_manitoba_fp_providers.rds"),compress="gzip")
haven::write_dta(joined,file.path(out,"cotedivoire_women_manitoba_fp_providers.dta"),version=15)
back<-haven::read_dta(file.path(out,"cotedivoire_women_manitoba_fp_providers.dta"))
stopifnot(nrow(back)==nrow(raw),all(vapply(names(raw),function(k){
 a<-raw[[k]];b<-back[[k]]
 if(is.character(a)){a[is.na(a)]<-"";b[is.na(b)]<-"";identical(a,b)}else same(a,b)
},logical(1))))
saveRDS(profile,file.path(out,"cotedivoire_fp_provider_profiles.rds"))
write.csv(profile,file.path(out,"cotedivoire_fp_provider_profiles.csv"),row.names=FALSE,na="")
write.csv(coverage,file.path(out,"fp_provider_counselor_coverage.csv"),row.names=FALSE,na="")
write.csv(fac[c("fid","assigned_join_area","fp_provider",names(frequency))],file.path(out,"daily_provision_facility_audit.csv"),row.names=FALSE,na="")
jsonlite::write_json(audit,file.path(out,"dataset_verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Verified all original women and survey columns; reaggregated all supply columns among FP providers.\n")
cat(audit$linked_fp_providers,"eligible facilities in",audit$linked_departements,"linked departements.\n")
