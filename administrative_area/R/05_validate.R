# Join/reference validation only; no models or substantive analysis.
source(file.path(root, "R/helpers.R"))
input <- Sys.getenv("LINKAGE_INPUT_DIR", file.path(root, "data/extracted"))
out <- Sys.getenv("LINKAGE_OUTPUT_DIR", file.path(root, "output"))
dest <- file.path(out, "reconciliation")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

fac <- readRDS(file.path(out, "audit/facility_linkage_inputs.rds"))
crosswalk <- read_chars(file.path(input, "exports/uom_surgo_geo_crosswalk_reviewed.csv"))
reference <- read_chars(file.path(input, "exports/merged_supply_demand_department.csv"))
source_coverage <- read_chars(file.path(input, "exports/merged_supply_demand_coverage.csv"))
arch <- read_chars(file.path(input, "uom_pipeline/Facilities data with archetype assignments.csv"))
curated <- read_chars(file.path(input, "curated/Facilities data with archetype assignments.csv"))
dictionary <- read_chars(file.path(input, "exports/merged_supply_demand_dictionary.csv"))
sup_names <- dictionary$column_name[dictionary$table_name == "merged_supply_demand_department" & dictionary$topic == "Supply"]
pipeline <- readRDS(file.path(out, "supply_profiles/supply_profiles_pipeline_crosswalk.rds"))
audited <- readRDS(file.path(out, "supply_profiles/supply_profiles_audited_geography.rds"))
tests <- list()
check <- function(name, ok, detail = "") {
  tests[[length(tests) + 1]] <<- data.frame(check = name, passed = isTRUE(ok), detail = detail)
  if (!isTRUE(ok)) warning("CHECK FAILED: ", name, ": ", detail)
}
same_num <- function(a, b) {
  a <- num(a); b <- num(b)
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a-b) <= 1e-9 * pmax(1, abs(b)))
}
source_key <- function(x, prefix="") apply(x[paste0(prefix,c("admin1","admin2","admin3"))],1,
    function(z)paste(ifelse(is.na(z)|z=="","<NA>",z),collapse="\r"))
cw_keys <- paste(crosswalk$loc, source_key(crosswalk,"uom_"),sep="\r")
fi <- match(paste(fac$loc,source_key(fac),sep="\r"),cw_keys)
check("Every facility maps to one source-crosswalk row",!anyNA(fi)&&!anyDuplicated(cw_keys))
fac$source_crosswalk_row <- fi

# The exact source file is retained separately, unchanged byte for byte.
file.copy(file.path(input,"exports/uom_surgo_geo_crosswalk_reviewed.csv"),
          file.path(dest,"his_reviewed_crosswalk_exact.csv"),overwrite=TRUE)
summary_rows <- list(); flow_rows <- list()
for(i in seq_len(nrow(crosswalk))){
  x <- crosswalk[i,,drop=FALSE]; z <- fac[fac$source_crosswalk_row==i,,drop=FALSE]
  cfg_name <- names(country_config)[vapply(country_config,function(y)y$loc==x$loc,logical(1))]
  target <- if(country_config[[cfg_name]]$join_level=="geo_level3")x$surgo_geo_level3 else x$surgo_geo_level2
  if(x$match_method%in%c("none")||x$status%in%"unmappable"||is.na(target)||target=="")target<-NA_character_
  ref_targets <- unique(z$geo_join_pipeline)
  equal_ref <- all((is.na(z$geo_join_pipeline)&is.na(target))|(!is.na(z$geo_join_pipeline)&!is.na(target)&z$geo_join_pipeline==toupper(target)))
  count_ok <- nrow(z)==num(x$n_facilities)
  actual_targets <- sort(unique(z$geo_join_audited[!is.na(z$geo_join_audited)]))
  physical <- sort(unique(z$assigned_join_area[!is.na(z$assigned_join_area)]))
  restored <- sum(!is.na(z$corrected_source_geo_name)&!is.na(z$source_geo_name)&norm(z$corrected_source_geo_name)!=norm(z$source_geo_name))
  r<-x
  r$source_crosswalk_id<-sprintf("CW%04d",i)
  r$our_reference_facilities<-nrow(z)
  r$our_reference_target<-if(length(ref_targets)&&!all(is.na(ref_targets)))paste(sort(unique(na.omit(ref_targets))),collapse="; ")else NA_character_
  r$reference_count_matches<-count_ok;r$reference_target_matches<-equal_ref
  r$our_source_names_after_label_check<-paste(sort(unique(na.omit(z$corrected_source_geo_name))),collapse="; ")
  r$our_assigned_join_areas<-paste(physical,collapse="; ")
  r$our_audited_surgo_targets<-paste(actual_targets,collapse="; ")
  r$our_audited_target_count<-length(actual_targets)
  r$our_audited_matched_facilities<-sum(!is.na(z$geo_join_audited))
  r$our_changed_assignments<-sum(z$geo_changed)
  r$our_restored_source_labels<-restored
  r$our_qualified_assignments<-sum(z$geo_review_required)
  r$unused_parent_field_issue<-x$loc=="Lagos" & norm(x$uom_admin3)%in%norm(c("Agege","Alimosho","Ifako-Ijaiye")) & norm(x$uom_admin2)!="IKEJA"
  r$assignment_basis<-paste(sort(unique(z$geo_assignment_basis)),collapse=" | ")
  r$review_result<-if(!count_ok||!equal_ref)"FAIL_REFERENCE_REPRODUCTION" else if(any(z$geo_review_required))"QUALIFIED_GEOGRAPHY" else if(any(z$geo_changed))"DOCUMENTED_TARGET_DIFFERENCE" else if(restored>0)"LABEL_CORRECTION_SAME_TARGET" else if(r$unused_parent_field_issue)"REFERENCE_MATCH_UNUSED_PARENT_ISSUE" else "REFERENCE_MATCH"
  summary_rows[[i]]<-r
  split_key<-paste(ifelse(is.na(z$geo_join_audited),"<NO_SURGO_TARGET>",z$geo_join_audited),
                   ifelse(is.na(z$assigned_join_area),"<UNKNOWN>",z$assigned_join_area),sep="\r")
  for(k in unique(split_key)){
    s<-z[split_key==k,,drop=FALSE]
    flow_rows[[length(flow_rows)+1]]<-data.frame(source_crosswalk_id=sprintf("CW%04d",i),loc=x$loc,
      uom_admin1=x$uom_admin1,uom_admin2=x$uom_admin2,uom_admin3=x$uom_admin3,
      his_surgo_target=target,our_audited_target=s$geo_join_audited[1],
      our_assigned_join_area=s$assigned_join_area[1],facility_records=nrow(s),
      changed_records=sum(s$geo_changed),qualified_records=sum(s$geo_review_required),
      basis=paste(sort(unique(s$geo_assignment_basis)),collapse=" | "))
  }
}
cw_summary<-bind_rows(summary_rows);flows<-bind_rows(flow_rows)
check("All source crosswalk facility counts reproduced",all(cw_summary$reference_count_matches))
check("All source crosswalk targets reproduced",all(cw_summary$reference_target_matches))
check("Expanded crosswalk flows sum to all facility records",sum(flows$facility_records)==nrow(fac))
write_table(cw_summary,file.path(dest,"complete_crosswalk_comparison.csv"))
write_table(flows,file.path(dest,"expanded_crosswalk_assignment_flows.csv"))

# Facility classifications: imported assignments must match by facility ID, not row position.
key<-function(x)paste(x$loc,x$fid,sep="\r")
ai<-match(key(fac),key(arch));oi<-match(key(fac),key(curated))
curated_match_method<-rep("fid_and_loc",nrow(fac))
missing_curated<-which(is.na(oi))
if(length(missing_curated)){
  # The supplied legacy file has13 blank Sindh fids; the README documents sid recovery.
  sid_key<-paste(curated$loc,curated$sid,sep="\r")
  for(j in missing_curated){
    hit<-which(sid_key==paste(fac$loc[j],fac$sid[j],sep="\r") & (is.na(curated$fid)|curated$fid==""))
    if(length(hit)==1){oi[j]<-hit;curated_match_method[j]<-"unique_sid_for_legacy_blank_fid"}
  }
}
check("All facility IDs present in supplied archetype outputs",!anyNA(ai)&&!anyNA(oi))
write_table(data.frame(country=fac$country,fid=fac$fid,sid=fac$sid,curated_match_method=curated_match_method),
            file.path(dest,"archetype_identity_reconciliation.csv"))
classification_cells<-list();classification_summary<-list();permutation<-list()
ari<-function(a,b){
  t<-table(a,b);n<-sum(t);choose2<-function(z)z*(z-1)/2
  if(n<2)return(1)
  expected<-sum(choose2(rowSums(t)))*sum(choose2(colSums(t)))/choose2(n)
  denom<-(sum(choose2(rowSums(t)))+sum(choose2(colSums(t))))/2-expected
  if(abs(denom)<1e-14)return(1)
  (sum(choose2(t))-expected)/denom
}
for(country in names(country_config)){
  take<-fac$country==country
  for(k in 3:6){
    v<-paste0("cluster_k",k);ours<-num(fac[[v]][take]);his<-num(arch[[v]][ai[take]]);old<-num(curated[[v]][oi[take]])
    ok<-same_num(ours,his)
    classification_summary[[length(classification_summary)+1]]<-data.frame(country=country,solution=k,facilities=sum(take),
      exact_pipeline_matches=sum(ok),exact_pipeline_match_pct=100*mean(ok),curated_code_matches=sum(same_num(ours,old)),
      adjusted_rand_vs_curated=ari(ours,old))
    classification_cells[[length(classification_cells)+1]]<-data.frame(country=country,fid=fac$fid[take],solution=k,
      our_cluster=ours,his_pipeline_cluster=his,his_curated_cluster=old,exact_pipeline_match=ok)
    pp<-as.data.frame(table(pipeline_cluster=his,curated_cluster=old));pp<-pp[pp$Freq>0,,drop=FALSE]
    pp$country<-country;pp$solution<-k;permutation[[length(permutation)+1]]<-pp
    check(paste(country,k,"archetype assignment parity"),all(ok))
    check(paste(country,k,"archetype partition parity with curated"),abs(ari(ours,old)-1)<1e-12)
  }
  for(v in c("pubpriv","ftyp2"))check(paste(country,v,"classification parity"),
    all((is.na(fac[[v]][take])&is.na(arch[[v]][ai[take]]))|(!is.na(fac[[v]][take])&!is.na(arch[[v]][ai[take]])&fac[[v]][take]==arch[[v]][ai[take]])))
}
write_table(bind_rows(classification_cells),file.path(dest,"facility_archetype_assignment_comparison.csv"))
write_table(bind_rows(classification_summary),file.path(dest,"archetype_classification_summary.csv"))
write_table(bind_rows(permutation),file.path(dest,"curated_pipeline_cluster_label_mapping.csv"))
labels<-arch[,c("loc","cluster_k6","NamesC")]%>%mutate(NamesC=gsub("[\r\n]+"," ",NamesC))%>%group_by(loc,cluster_k6,NamesC)%>%summarise(facilities=n(),.groups="drop")
write_table(labels,file.path(dest,"six_archetype_label_lookup.csv"))

# Compare the same analytical women/need flags as his source RDS, not raw totals.
area_rows<-list();cohort_rows<-list();coverage_rows<-list();woman_parity<-list()
for(country in names(country_config)){
  cfg<-country_config[[country]]
  original<-read_women(file.path(input,cfg$woman_file))
  his<-readRDS(file.path(input,"surgo_pipeline",country,"wra_design_ready.rds"))
  oid<-case_name(names(original),cfg$id);hid<-case_name(names(his),cfg$id)
  ids<-as.character(original[[oid]]);hids<-as.character(his[[hid]])
  present_ids<-!is.na(ids)&ids!=""
  present_hids<-!is.na(hids)&hids!=""
  check(paste(country,"nonmissing woman IDs unique; analytic IDs complete"),
        !anyDuplicated(ids[present_ids])&&!anyDuplicated(hids)&&all(present_hids))
  hi<-match(ids,hids)
  check(paste(country,"every source analytic woman exists in raw data"),all(hids%in%ids))
  in_his<-!is.na(hi)
  need<-rep(NA_real_,nrow(original));need[in_his]<-num(his$need_n[hi[in_his]])
  geo<-toupper(trimws(lab(original[[case_name(names(original),cfg$woman_geo)]])))
  hg<-toupper(trimws(as.character(his$.dept)))
  check(paste(country,"analytic woman geography unchanged"),all(norm(geo[in_his])==norm(hg[hi[in_his]])))
  w<-num(original[[case_name(names(original),cfg$weight)]])
  check(paste(country,"analytic weights unchanged"),all(same_num(w[in_his],his$.w[hi[in_his]])))
  c<-readRDS(file.path(out,"datasets",paste0(country,"_women_manitoba.rds")))
  rr<-reference[reference$country==country,,drop=FALSE]
  ri<-match(norm(geo),norm(rr$geo_join))
  ncmp<-0;ndiff<-0
  for(v in c("n_facilities",sup_names)){
    expected<-num(rr[[v]][ri]);if(v=="n_facilities")expected[is.na(ri)]<-0
    same<-same_num(c[[v]],expected)
    ncmp<-ncmp+length(same);ndiff<-ndiff+sum(!same)
  }
  woman_parity[[country]]<-data.frame(country=country,women=nrow(original),supply_columns=length(sup_names)+1,
    comparisons=ncmp,mismatches=ndiff)
  check(paste(country,"every reference woman's supply values agree with source geography"),ndiff==0)
  consent_field<-case_name(names(original),"irb1")
  cons<-num(original[[consent_field]])==1;cons[is.na(cons)]<-FALSE
  valid<-is.finite(w)&w>0
  cohort_rows[[country]]<-data.frame(country=country,raw_women=nrow(original),his_analytic_women=nrow(his),
    original_women_retained_in_ours=nrow(c),women_outside_his_analysis=sum(!in_his),
    missing_or_nonpositive_weight=sum(!valid),nonconsenting_or_unknown=sum(!cons),
    excluded_with_valid_weight=sum(!in_his&valid),raw_missing_woman_ID=sum(!present_ids),missing_analytic_ids= sum(!hids%in%ids))
  for(i in seq_len(nrow(rr))){
    r<-rr[i,,drop=FALSE];z<-norm(geo)==norm(r$geo_join);z[is.na(z)]<-FALSE
    hc<-z&in_his
    p<-pipeline[pipeline$country==country&norm(pipeline$geo_join)==norm(r$geo_join),,drop=FALSE]
    aa<-audited[audited$country==country&norm(audited$geo_join)==norm(r$geo_join),,drop=FALSE]
    ours_n<-if(nrow(p))p$n_facilities else 0;aud_n<-if(nrow(aa))aa$n_facilities else 0
    area_rows[[length(area_rows)+1]]<-data.frame(country=country,geo_join=r$geo_join,
      his_facilities=num(r$n_facilities),our_reference_facilities=ours_n,our_audited_facilities=aud_n,
      his_analytic_women=num(r$n_sample),our_same_cohort_women=sum(hc),our_all_raw_women=sum(z),
      his_women_in_need=num(r$n_in_need),our_same_cohort_need=sum(need[hc]==1,na.rm=TRUE),
      his_has_supply=bool(r$has_supply)==1,our_reference_has_supply=nrow(p)>0,our_audited_has_supply=nrow(aa)>0,
      his_binding_constraint=r$binding_constraint)
  }
  cov<-source_coverage[source_coverage$country==country,,drop=FALSE]
  x<-bind_rows(area_rows);x<-x[x$country==country,,drop=FALSE]
  total_fac<-sum(fac$country==country)
  hfac<-sum(x$his_facilities);p_fac<-sum(x$our_reference_facilities);a_fac<-sum(x$our_audited_facilities)
  sample<-sum(x$his_analytic_women);nd<-sum(x$his_women_in_need)
  coverage_rows[[country]]<-data.frame(country=country,source_facility_total=num(cov$n_facilities_total),our_facility_total=total_fac,
    his_matched_facilities=hfac,our_reference_matched_facilities=p_fac,our_audited_matched_facilities=a_fac,
    his_facility_match_pct=num(cov$pct_matched),our_reference_facility_match_pct=100*p_fac/total_fac,our_audited_facility_match_pct=100*a_fac/total_fac,
    his_geographies=nrow(x),his_geographies_with_supply=sum(x$his_has_supply),our_reference_geographies_with_supply=sum(x$our_reference_has_supply),
    our_audited_geographies_with_supply=sum(x$our_audited_has_supply),his_analytic_women=sample,
    his_analytic_women_with_supply=sum(x$his_analytic_women[x$his_has_supply]),
    our_reference_same_cohort_with_supply=sum(x$our_same_cohort_women[x$our_reference_has_supply]),
    our_audited_same_cohort_with_supply=sum(x$our_same_cohort_women[x$our_audited_has_supply]),
    his_analytic_need=nd,his_need_with_supply=sum(x$his_women_in_need[x$his_has_supply]),
    our_reference_same_need_with_supply=sum(x$our_same_cohort_need[x$our_reference_has_supply]),
    our_audited_same_need_with_supply=sum(x$our_same_cohort_need[x$our_audited_has_supply]))
}
areas<-bind_rows(area_rows);coverage<-bind_rows(coverage_rows);cohorts<-bind_rows(cohort_rows)
check("All source area facility counts match reference",all(areas$his_facilities==areas$our_reference_facilities))
check("All source area analytic population counts reproduced",all(areas$his_analytic_women==areas$our_same_cohort_women))
check("All source area need counts reproduced",all(areas$his_women_in_need==areas$our_same_cohort_need))
check("All source supply/demand overlap classifications reproduced",all(areas$his_has_supply==areas$our_reference_has_supply))
write_table(areas,file.path(dest,"all_area_overlap_population_comparison.csv"))
write_table(coverage,file.path(dest,"country_overlap_population_comparison.csv"))
write_table(cohorts,file.path(dest,"raw_versus_his_analytic_population.csv"))
write_table(bind_rows(woman_parity),file.path(dest,"woman_supply_reference_parity.csv"))
checks<-bind_rows(tests)
write_table(checks,file.path(dest,"reconciliation_checks.csv"))
result<-list(checks=nrow(checks),passed=sum(checks$passed),failed=sum(!checks$passed),
  source_crosswalk_rows=nrow(crosswalk),comparison_crosswalk_rows=nrow(cw_summary),expanded_flow_rows=nrow(flows),
  source_geographic_rows=nrow(areas),facility_archetype_comparisons=nrow(bind_rows(classification_cells)),
  woman_supply_cell_comparisons=sum(bind_rows(woman_parity)$comparisons),
  limitations=c("Imported archetype assignments are checked against source outputs; clustering was not independently refit.",
                "Source binding_constraint is a SURGO cascade classification,not the same construct as an LGA supply/demand score classification.",
                "Proposed completions/corrections are listed separately; the reference mapping reproduces the source exactly.",
                "This is a technical reconciliation,not an external certification of survey measurement or facility boundary membership."))
jsonlite::write_json(result,file.path(dest,"reconciliation_summary.json"),auto_unbox=TRUE,pretty=TRUE)
print(result)
if(any(!checks$passed))stop("Reconciliation has failed checks; inspect reconciliation_checks.csv")
