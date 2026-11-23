#!/usr/bin/env Rscript
# Senegal Rural/Urban report: complete country analysis.
# Run this one R file from the complete project; output HTML/Rmd is beside it.
# Setup: Rscript setup.R. External data: --input-zip or SURGO_MANITOBA_INPUT_ZIP.
# Exact source filenames: docs/DATASET_FILENAMES.md. No input data are in this repo.
# R packages: haven, dplyr, readr, readxl, jsonlite, digest, sf, survey,
# clubSandwich, ggplot2, svglite, tidyselect, rmarkdown, knitr, base64enc.
# SURGO uses final weights, ids=~1, no strata/FPC; separate area CR2 inference.
# Facility means use confirmed FP providers with valid responses, equally weighted.
# Sections: country definitions; facility preparation; descriptive scores; models;
# sensitivity analyses; independent checks; build and render the matching report.

# Original source weight: WEIGHT -> internal weight.
# Selected source files (ZIP-relative members; dictionaries/lookups also required):
#   inputs/surgo/senegal/surgo_senegal_women.dta
#   inputs/uom/senegal/uom_senegal_facility_pooled.dta
#   uom_pipeline/uom_summary_facility.csv
#   uom_pipeline/Facilities data with archetype assignments.csv
#   uom_pipeline/new_demand_vol_facility.csv
#   exports/uom_surgo_geo_crosswalk_reviewed.csv
#   inputs/SenegalURlookup.xlsx
# Full inventory and roles: docs/INPUTS.md.

.source_files <- vapply(sys.frames(),function(frame) {
  file<-frame$ofile;if(is.null(file))""else as.character(file)[1]
},character(1))
.source_files <- .source_files[nzchar(.source_files)]
.report_file <- if(length(.source_files))tail(.source_files,1) else
  gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
.report_file <- normalizePath(.report_file,mustWork=TRUE)
.report_root <- dirname(dirname(dirname(.report_file)))
source(file.path(.report_root,"support/runtime.R"))
use_project_library(.report_root)
source(file.path(.report_root,"support/linkage.R"))
.report_definitions <- list()
.report_steps <- list()

# Country definition: senegal_definitions.R
.report_definitions[["senegal_definitions.R"]] <- function(target) {
  evalq({
    # Shared Senegal coding. Original SURGO FPN2 is retained without additions.
    senegal_key <- function(v) {
      v <- iconv(as.character(v), to = "ASCII//TRANSLIT")
      v <- toupper(gsub("[^A-Za-z0-9]", "", v))
      v[is.na(v) | v == ""] <- NA_character_
      v
    }
    senegal_domains <- function() list(
      D = c("agency", "stigma", "health", "fertility", "confidence", "opposes"),
      DL = c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs",
             "Future-fertility concerns", "Low method knowledge", "Opposes family planning"),
      S = c("offering", "free", "training", "counselor", "larc_stock", "popular_stock"),
      SL = c("Short + long-acting offering", "Free-product provision", "Trained-provider share",
             "FP counselor availability", "At least one LARC in stock", "Most-popular product in stock"))
    senegal_supply <- function(x) {
      map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod",
        training = "mean_p_train", counselor = "share_fp_counselor", larc_stock = "share_stocked_1plus_larc")
      a <- as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
      yes <- as.numeric(x$n_mp_in_stock_true); no <- as.numeric(x$n_mp_in_stock_false)
      a$popular_stock <- ifelse(yes + no > 0, yes / (yes + no), NA_real_)
      a
    }
    senegal_definitions <- function(x) {
      nv <- function(k) { if (!k %in% names(x)) stop("Missing Senegal field: ", k); as.numeric(x[[k]]) }
      flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
      any_flag <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1,
        ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
      stopifnot(!anyDuplicated(x$IDW))
      d <- data.frame(row = seq_len(nrow(x)), id = as.character(x$IDW), geo = senegal_key(x$link_geo_join),
        district = as.character(x$link_geo_join), weight = nv("WEIGHT"), consent = nv("IRB1") %in% 1,
        psu = as.character(x$RB0H), residence = as.character(x$link_rurb))
      d$positive <- d$consent & is.finite(d$weight) & d$weight > 0
      d$need_code <- nv("FPN2"); d$need <- d$positive & d$need_code %in% 1
      d$agency <- flag("HS7C", 1, 1:2)
      d$stigma <- any_flag(cbind(flag("PB7B", 1:2, 1:4), flag("PB7C", 1:2, 1:4)))
      # Primary health/side-effect belief: perceived health problems, PB2A only.
      d$health <- flag("PB2A", 1:2, 1:4)
      mp <- sapply(paste0("MP4_", letters[1:9]), nv)
      selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
      none <- mp[, 9] %in% 1
      coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & nv("MP4_99") %in% 0 &
        (selected | none) & !(selected & none)
      d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) > 0), NA_real_)
      d$confidence <- any_flag(cbind(flag("MC5A", 3:4, 1:4), flag("MC5B", 3:4, 1:4)))
      d$opposes <- flag("FP1", 3:4, 1:4)
      prompts <- sapply(paste0("CU5", LETTERS[1:4]), nv)
      current <- ifelse(nv("CU1") %in% c(1, 3) | rowSums(prompts == 1, na.rm = TRUE) > 0, 1,
        ifelse(nv("CU1") %in% 2 & rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
      d$user <- current %in% 1
      modern <- rowSums(sapply(paste0("CU4_", 1:8), function(k) nv(k) %in% 1)) > 0 |
        nv("CU5C") %in% 1 | nv("CU5D") %in% 1 | (nv("CU4_11") %in% 1 & nv("CU4_lam") %in% 1)
      traditional <- rowSums(sapply(paste0("CU4_", c(9, 10, 12:15)), function(k) nv(k) %in% 1)) > 0 |
        nv("CU5A") %in% 1 | nv("CU5B") %in% 1
      d$modern <- d$user & modern; d$traditional <- d$user & !modern & traditional
      d$y_modern <- ifelse(current %in% 0, 0, ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
      d$intender <- current %in% 0 & nv("ITU1") %in% 1:2
      d$no_intent <- current %in% 0 & nv("ITU1") %in% c(3, 4, 99)
      d$unresolved <- !(d$user | d$intender | d$no_intent)
      d$wanted_start <- d$user & nv("CUB8") %in% 1
      d$other_start <- d$user & nv("CUB8") %in% 2
      d$modern_other_start <- d$modern & nv("CUB8") %in% 2
      future_modern <- rowSums(sapply(paste0("ITS2_", 1:8), function(k) nv(k) %in% 1)) > 0
      d$traditional_modern_switch <- d$traditional & nv("ITC2") %in% 1:2 & future_modern
      age <- nv("RB2"); d$age10 <- ifelse(age >= 15 & age <= 49, (age - 30) / 10, NA_real_)
      d$age10_sq <- d$age10^2
      edu <- ifelse(nv("RB3") %in% 1:4, "Primary or less",
        ifelse(nv("RB3") %in% 5:6, "Middle / secondary",
          ifelse(nv("RB3") %in% 7:8, "Tertiary / professional", NA_character_)))
      d$education <- factor(edu, levels = c("Primary or less", "Middle / secondary", "Tertiary / professional"))
      d$partnered <- flag("R1", 1:3, 1:4)
      goal <- rep("Uncertain or unstated", nrow(x))
      goal[nv("SF2") %in% 4] <- "After marriage"; goal[nv("SF2") %in% 3] <- "After 3 years"
      goal[nv("SF2") %in% 2] <- "In 1-3 years"
      goal[nv("SF1A") %in% 2 | nv("SF1B") %in% 2 | nv("SF1C") %in% 2] <- "No more children"
      d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children", "After marriage", "Uncertain or unstated"))
      d$urban <- ifelse(nv("FV_MILIEU") %in% 1:2, as.numeric(nv("FV_MILIEU") == 2), NA_real_)
      stopifnot(all(d$residence == ifelse(d$urban == 1, "Urban", "Rural")),
        !anyNA(d$psu[d$positive]), all(nzchar(d$psu[d$positive])),
        all(vapply(split(d$geo[d$positive], d$psu[d$positive]), function(v) length(unique(v)) == 1, logical(1))))
      d
    }

  },envir=target)
}

# Country definition: supply_definitions.R
.report_definitions[["supply_definitions.R"]] <- function(target) {
  evalq({
    # Report-specific supply definitions; historical country definitions are unchanged.
    base_senegal_domains <- senegal_domains
    senegal_domains <- function() {
      a <- base_senegal_domains()
      a$S <- c("offering", "free", "training", "daily_provision", "larc_stock", "stockout_burden")
      a$SL <- c("Short + long-acting offering", "Free-product provision", "Trained-provider share",
                "Daily provision of offered modern methods", "At least one LARC in stock", "Stockout burden")
      a
    }

    senegal_supply <- function(x) {
      map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod",
        training = "mean_p_train", daily_provision = "mean_daily_modern_provision",
        larc_stock = "share_stocked_1plus_larc", stockout_burden = "mean_facility_any_stockout")
      missing <- setdiff(unname(map), names(x))
      if (length(missing)) stop("Missing updated supply fields: ", paste(missing, collapse = ", "))
      as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
    }

    # Raw burden remains visible in tables. Reverse it only for favorable scoring/PCA.
    senegal_favorable_supply <- function(x) {
      x <- as.data.frame(x)
      if ("stockout_burden" %in% names(x)) x$stockout_burden <- 1 - x$stockout_burden
      x
    }

    # Seven core modern categories: pills, injectables, male/female condoms, ECP, IUD,
    # implants. Raw numeric codes avoid collapsing frequency labels through offering.
    senegal_frequency <- function(raw) {
      fields <- paste0("s5_7", letters[1:7])
      stopifnot(all(fields %in% names(raw)))
      m <- vapply(raw[fields], as.numeric, numeric(nrow(raw)))
      valid <- m >= 1 & m <= 5
      offered <- m >= 1 & m <= 4
      n_offered <- rowSums(offered, na.rm = TRUE)
      n_daily <- rowSums(m == 1, na.rm = TRUE)
      n_weekly <- rowSums(m >= 1 & m <= 2, na.rm = TRUE)
      daily <- ifelse(n_offered > 0, n_daily / n_offered, NA_real_)
      weekly <- ifelse(n_offered > 0, n_weekly / n_offered, NA_real_)
      data.frame(daily_provision = daily, at_least_weekly_provision = weekly,
        frequency_daily_categories = n_daily, frequency_offered_categories = n_offered,
        frequency_valid_categories = rowSums(valid, na.rm = TRUE),
        frequency_unknown_categories = 7L - rowSums(valid, na.rm = TRUE))
    }

  },envir=target)
}

# Reuse common methods without duplicating their scientific implementation.
.report_source <- function(path,target) {
  key<-basename(path)
  if(key %in% names(.report_definitions))return(.report_definitions[[key]](target))
  if(key %in% c("helpers.R","aggregate_supply.R","geography.R","00_config.R"))return(load_linkage_definitions(target))
  common<-c("survey_methods.R","risk_scores.R","plot_models.R")
  if(key %in% common)return(base::source(file.path(.report_root,"support",key),local=target))
  stop("Undocumented definition dependency: ",key)
}

# Prepare fp provider data -----------------------------------------------------
.report_steps[["prepare_fp_provider_data.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Reaggregate all supply columns among confirmed FP providers; retain every woman.
  args <- commandArgs(trailingOnly=TRUE)
  arg <- function(k) {i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  input <- arg("--input"); facility_input <- arg("--facility-input")
  raw_facility <- arg("--raw-facility"); raw_women <- arg("--raw-women")
  dictionary_file <- arg("--dictionary"); out <- arg("--output-dir")
  script <- gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
  project_root <- .report_root
  root <- file.path(project_root,"administrative_area")
  source(file.path(root,"R/helpers.R"));source(file.path(root,"R/aggregate_supply.R"))
  source(file.path(project_root,"analysis/senegal/senegal_definitions.R"))
  source(file.path(dirname(script),"supply_definitions.R"))
  inputs<-c(input,facility_input,raw_facility,raw_women,dictionary_file)
  hashes<-unname(tools::md5sum(inputs))
  all_fac<-readRDS(facility_input);fac<-all_fac[all_fac$country=="senegal",]
  r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("fid","s5_6","s7_2",paste0("s5_7",letters[1:7]))))
  assert_unique(data.frame(fid=lab(r$fid)),"fid","Raw Senegal facilities")
  ri<-match(fac$fid,lab(r$fid));stopifnot(!anyNA(ri),nrow(fac)==nrow(r))
  fp<-num(r$s5_6[ri]);counsel<-num(r$s7_2[ri])
  expected<-ifelse(counsel%in%1,1,ifelse(counsel%in%c(0,2),0,NA_real_))
  stopifnot(isTRUE(all.equal(expected,fac$counselor_available,check.attributes=FALSE)))
  fac$counselor_available<-expected;fac$fp_provider<-fp%in%1
  frequency<-senegal_frequency(r[ri,])
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
  stopifnot(nrow(profile)==46,sum(profile$n_facilities)==sum(fp%in%1))
  original<-readRDS(input);joined<-original
  ix<-match(norm(original$link_geo_join),norm(profile$geo_join))
  stopifnot(!anyNA(ix),!anyDuplicated(norm(profile$geo_join)))
  replace<-setdiff(names(profile),c("country","join_level","geo_join"))
  for(k in replace) joined[[k]]<-profile[[k]][ix]
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
   linked_source_facilities=sum(linked),linked_fp_providers=sum(linked&fac$fp_provider),
   counselor_question=attr(r$s7_2,"label"),counselor_yes=sum(linked&fac$fp_provider&expected%in%1),
   counselor_no=sum(linked&fac$fp_provider&expected%in%0),
   counselor_unknown=sum(linked&fac$fp_provider&is.na(expected)),
   counselor_definition="s7_2: 1 Yes; 0/2 No; all other values unknown. Availability is not a measure of training, receipt or quality.",
   daily_provision_definition="s5_7a-g: count Daily (1) / count Daily, Weekly, Biweekly or Monthly (1-4); code 5 Not at all excluded from the offered-method denominator; unknown responses excluded, not zero. Mean valid facility ratios among s5_6=1.",
   daily_valid_linked=sum(linked&fac$fp_provider&is.finite(fac$daily_provision)),
   daily_undefined_linked=sum(linked&fac$fp_provider&!is.finite(fac$daily_provision)),
   stockout_definition="Original cleaned UoM p_any_stockout retained without recoding; valid facility means among s5_6=1. Higher raw values are worse; scoring reverses direction.",
   all_supply_columns_reaggregated=replace,linked_departements=length(unique(ix)),
   source_files_unchanged=identical(hashes,unname(tools::md5sum(inputs))))
  stopifnot(audit$source_files_unchanged,audit$fp_yes==1941,audit$linked_fp_providers==1568,
   audit$counselor_yes==135,audit$counselor_no==50,audit$linked_departements==33)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  saveRDS(joined,file.path(out,"senegal_women_manitoba_fp_providers.rds"),compress="gzip")
  haven::write_dta(joined,file.path(out,"senegal_women_manitoba_fp_providers.dta"),version=15)
  back<-haven::read_dta(file.path(out,"senegal_women_manitoba_fp_providers.dta"))
  stopifnot(nrow(back)==nrow(raw),all(vapply(names(raw),function(k){
   a<-raw[[k]];b<-back[[k]]
   if(is.character(a)){a[is.na(a)]<-"";b[is.na(b)]<-"";identical(a,b)}else same(a,b)
  },logical(1))))
  saveRDS(profile,file.path(out,"senegal_fp_provider_profiles.rds"))
  write.csv(profile,file.path(out,"senegal_fp_provider_profiles.csv"),row.names=FALSE,na="")
  write.csv(coverage,file.path(out,"fp_provider_counselor_coverage.csv"),row.names=FALSE,na="")
  write.csv(fac[c("fid","assigned_join_area","fp_provider",names(frequency))],file.path(out,"daily_provision_facility_audit.csv"),row.names=FALSE,na="")
  jsonlite::write_json(audit,file.path(out,"dataset_verification.json"),auto_unbox=TRUE,pretty=TRUE)
  cat("Verified all original women and survey columns; reaggregated all supply columns among FP providers.\n")
  cat(audit$linked_fp_providers,"eligible facilities in",audit$linked_departements,"linked departements.\n")

}

# Build analysis -----------------------------------------------------
.report_steps[["build_analysis.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Department-only Senegal analysis; facility response coverage is explicit.
  args<-commandArgs(trailingOnly=TRUE)
  opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  optional<-function(k,default){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
  input<-opt("--input");facility_file<-opt("--facility-input");raw_file<-opt("--raw")
  raw_facility<-opt("--raw-facility");department_lookup<-opt("--department-lookup");out<-opt("--output-dir")
  facility_sample<-optional("--facility-sample","all");reference_dir<-optional("--reference-dir","")
  demand_sample<-optional("--demand-sample","available")
  stopifnot(facility_sample=="all",demand_sample=="available")
  for(pkg in c("survey","jsonlite","haven"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
  here<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
  shared<-file.path(here,"..","..","..","analysis","senegal")
  source(file.path(shared,"senegal_definitions.R"))
  source(file.path(here,"supply_definitions.R"))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  inputs<-c(input,facility_file,raw_file,raw_facility,department_lookup)
  hash<-unname(tools::md5sum(inputs));savecsv<-function(v,name)write.csv(v,file.path(out,name),row.names=FALSE,na="")
  key<-senegal_key
  lab<-function(v)if(inherits(v,"haven_labelled"))as.character(haven::as_factor(v))else as.character(v)
  valid_mean<-function(v)if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_
  wm<-function(v,w){ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))survey_weighted_mean(v[ok],w[ok])else NA_real_}
  bool<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
  x0<-readRDS(input);admin<-x0;x<-x0
  stopifnot(nrow(x)==7169,!anyDuplicated(x$IDW),all(x$link_has_supply))
  # Administrative joins do not carry a residence key. Retain the woman's original
  # residence solely for adjustment and descriptive audit.
  x$link_rurb<-ifelse(as.numeric(x$FV_MILIEU)==1,"Rural",ifelse(as.numeric(x$FV_MILIEU)==2,"Urban",NA_character_))
  fallback<-rep(FALSE,nrow(x))
  x$analysis_supply_fallback<-fallback
  x$analysis_supply_match_level<-"Department"
  x$analysis_supply_source_key<-key(x$link_geo_join)
  x$analysis_has_supply<-x$link_has_supply
  d<-senegal_definitions(x)
  # Keep this report's primary health item explicit even with older shared country
  # definitions: PB2A 1/2 = concern, 3/4 = no concern, otherwise unresolved.
  pb2a<-as.numeric(x$PB2A)
  d$health<-ifelse(pb2a%in%1:4,as.numeric(pb2a%in%1:2),NA_real_)
  domains<-senegal_domains();D<-domains$D;DL<-domains$DL;S<-domains$S;SL<-domains$SL
  sp<-senegal_supply(x)[S]
  d$fallback<-fallback;d$source_profile<-x$analysis_supply_source_key
  profiles<-unique(cbind(geo=key(admin$link_geo_join),district=as.character(admin$link_geo_join),
   senegal_supply(admin)[S],n_facilities=as.numeric(admin$n_facilities)))
  stopifnot(!anyDuplicated(profiles$geo),nrow(profiles)==33,sum(d$positive)==7158,sum(d$need)==1905)
  ctx<-unique(cbind(d[c("geo","district","residence","fallback","source_profile")],sp,n_facilities=as.numeric(x$n_facilities)))
  stopifnot(!anyDuplicated(paste(ctx$geo,ctx$residence)),nrow(ctx)==49)
  savecsv(ctx,"assigned_supply_profiles.csv")
  fac<-readRDS(facility_file);fac<-fac[fac$country=="senegal",]
  fac$geo<-key(fac$assigned_join_area)
  areas<-sort(profiles$geo);names_map<-setNames(profiles$district,profiles$geo)
  mapped<-fac$geo%in%areas
  # Verify the checked health-district-to-department assignment against raw records.
  r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("fid","s1_3","s5_6",paste0("s5_7",letters[1:7]))))
  ri<-match(fac$fid,lab(r$fid));stopifnot(!anyNA(ri))
  e<-read.csv(department_lookup,check.names=FALSE,stringsAsFactors=FALSE)
  ei<-match(key(lab(r$s1_3)[ri]),key(e[["Health district"]]));stopifnot(!anyNA(ei))
  ed<-trimws(sub(" \u2014.*$","",e[["Department-checked"]][ei]))
  ed<-trimws(sub(" \\(.*$","",ed))
  stopifnot(all(key(ed)==fac$geo))
  inventory <- fac
  fp_code <- as.numeric(r$s5_6[ri])
  frequency<-senegal_frequency(r[ri,])
  fac<-cbind(fac,frequency)
  fac <- fac[fp_code %in% 1,]
  mapped <- fac$geo %in% areas
  stopifnot(nrow(fac)==1941,sum(mapped)==1568)
  fac$offering<-bool(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
  fac$training<-as.numeric(fac$p_train)
  fac$larc_stock<-bool(fac$one_larc_stockin);fac$stockout_burden<-as.numeric(fac$p_any_stockout)
  stopifnot(all(bool(fac$three_larc_stockin)==0),nrow(inventory)==2043)
  stopifnot(all(vapply(areas,function(g)sum(fac$geo==g)>0,logical(1))))
  savecsv(fac[c("fid","geo",names(frequency))],"daily_provision_facility_audit.csv")
  # Report source completeness separately from the numeric daily-provision ratio.
  savecsv(do.call(rbind,lapply(areas,function(g){ff<-fac[fac$geo==g,];
   data.frame(geo=g,department=names_map[[g]],fp_facilities=nrow(ff),
   daily_valid=sum(is.finite(ff$daily_provision)),daily_undefined=sum(!is.finite(ff$daily_provision)),
   all_seven_frequency_responses=sum(ff$frequency_valid_categories==7),
   stockout_valid=sum(is.finite(ff$stockout_burden)),
   mean_daily_provision=valid_mean(ff$daily_provision),
   mean_at_least_weekly_provision=valid_mean(ff$at_least_weekly_provision))})),"service_regularity_coverage.csv")
  # Residence is retained as a covariate, never as a key for these supply assignments.
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
  for(g in areas)check_profile(fac[fac$geo==g,],profiles[profiles$geo==g,],"Department",g,"All")
  for(j in seq_len(nrow(ctx))){q<-ctx[j,];fi<-fac$geo==q$geo
   check_profile(fac[fi,],q,"Department assigned to woman",q$geo,q$residence)
  }
  reconcile<-do.call(rbind,reconcile);stopifnot(all(reconcile$passed));savecsv(reconcile,"supply_input_reconciliation.csv")
  trace<-data.frame(row=d$row,IDW=d$id,department=d$district,residence=d$residence,in_need=d$need,
   exact_profile_available=!fallback,department_fallback=fallback,supply_source_key=d$source_profile,
   assigned_facilities=profiles$n_facilities[match(d$geo,profiles$geo)])
  saveRDS(trace,file.path(out,"woman_assignment_audit.rds"))
  fallback_table<-do.call(rbind,lapply(areas,function(g){ff<-fac[fac$geo==g,];w<-d$geo==g
   data.frame(geo=g,department=names_map[[g]],source_women=sum(w),rural_women=sum(w&d$residence=="Rural"),
   urban_women=sum(w&d$residence=="Urban"),need_women=sum(w&d$need),rural_need=sum(w&d$need&d$residence=="Rural"),
   urban_need=sum(w&d$need&d$residence=="Urban"),fallback_women=sum(w&fallback),fallback_need=sum(w&fallback&d$need),
   facility_records=nrow(ff),rural_classified_facilities=sum(ff$rurb=="Rural",na.rm=TRUE),
   urban_classified_facilities=sum(ff$rurb=="Urban",na.rm=TRUE),unknown_residence_facilities=sum(is.na(ff$rurb)))
  }));savecsv(fallback_table,"department_residence_assignment_summary.csv")
  options(survey.lonely.psu="adjust")
  full <- surgo_design(d[d$positive, ])
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
  dn_complete<-complete.cases(dn[D]) & apply(dn[D],1,function(v)all(is.finite(v)))
  stopifnot(identical(d$health,ifelse(as.numeric(x$PB2A)%in%1:4,as.numeric(as.numeric(x$PB2A)%in%1:2),NA_real_)))
  savecsv(do.call(rbind,lapply(c("SENEGAL",areas),function(g){ii<-if(g=="SENEGAL")rep(TRUE,nrow(dn))else dn$geo==g
   data.frame(geo=g,department=if(g=="SENEGAL")"Senegal"else names_map[[g]],women_in_need=sum(ii),
   complete_demand=sum(ii & dn_complete),excluded=sum(ii & !dn_complete),
   complete_weight_share=survey_weighted_mean(as.numeric(dn_complete[ii]),dn$weight[ii]))})),"demand_complete_case_coverage.csv")
  for(g in c("SENEGAL",areas)){
   wi<-if(g=="SENEGAL")rep(TRUE,nrow(dn)) else dn$geo%in%g
   fi<-mapped & (if(g=="SENEGAL")rep(TRUE,nrow(fac)) else fac$geo%in%g)
   coverage[[g]]<-list(geo=g,name=if(g=="SENEGAL")"Senegal" else names_map[[g]],women=sum(d$positive & if(g=="SENEGAL")rep(TRUE,nrow(d)) else d$geo%in%g),
    all_source_women=if(g=="SENEGAL")nrow(d) else sum(d$geo%in%g),n_need=sum(wi),facilities=sum(inventory$geo%in%areas & (if(g=="SENEGAL")rep(TRUE,nrow(inventory)) else inventory$geo%in%g)),fp_facilities=sum(fi))
   for(j in seq_along(D)){k<-D[j];ok<-wi & is.finite(dn[[k]]) & (demand_sample=="available" | dn_complete)
    cells[[length(cells)+1L]]<-data.frame(geo=g,side="Demand",variable=k,label=DL[j],value=100*wm(dn[[k]][ok],dn$weight[ok]),
      n_valid=sum(ok),n_total=sum(wi),numerator=sum(dn[[k]][ok]==1))}
   for(j in seq_along(S)){k<-S[j];v<-if(g=="SENEGAL")valid_mean(fac[[k]][fi]) else profiles[[k]][match(g,profiles$geo)]
    cells[[length(cells)+1L]]<-data.frame(geo=g,side="Supply",variable=k,label=SL[j],value=100*v,
      n_valid=sum(fi & is.finite(fac[[k]])),n_total=sum(fi),numerator=sum(fac[[k]][fi],na.rm=TRUE))}
  }
  for(g in setdiff(sort(unique(inventory$geo)),areas)) {
   coverage[[g]]<-list(geo=g,name=inventory$assigned_join_area[match(g,inventory$geo)],women=0,
    all_source_women=0,n_need=0,facilities=sum(inventory$geo==g),fp_facilities=sum(fac$geo==g))
  }
  cells<-do.call(rbind,cells);savecsv(cells,"heatmap_values_and_denominators.csv")
  primary_trace<-if(nzchar(reference_dir))read.csv(file.path(reference_dir,"component_score_trace.csv"),stringsAsFactors=FALSE)else NULL
  primary_thresholds<-if(nzchar(reference_dir))read.csv(file.path(reference_dir,"classification_thresholds.csv"),stringsAsFactors=FALSE)else NULL
  M<-list(); transforms<-list()
  for(side in c("Demand","Supply")){
   cols<-if(side=="Demand")D else S
   original<-sapply(cols,function(k){z<-cells[cells$side==side & cells$variable==k,];z$value[match(areas,z$geo)]})
   m<-if(side=="Supply")as.matrix(100*(1-senegal_favorable_supply(as.data.frame(original/100))))else original
   rownames(m)<-areas
   means<-colMeans(m,na.rm=TRUE);sds<-apply(m,2,sd,na.rm=TRUE);meds<-apply(m,2,median,na.rm=TRUE);iqrs<-apply(m,2,IQR,na.rm=TRUE)
   stopifnot(all(sds>0),all(iqrs>0))
   M[[side]]<-list(raw=m,rank=apply(m,2,rank,ties.method="average",na.last="keep"),
    robust=sweep(sweep(m,2,meds,"-"),2,iqrs,"/"),zscore=sweep(sweep(m,2,means,"-"),2,sds,"/"))
   for(k in cols)transforms[[length(transforms)+1L]]<-data.frame(geo=areas,side=side,variable=k,original_pct=original[,k],aligned_pct=m[,k],higher_raw_is_worse=side=="Supply"&&k=="stockout_burden",
    indicator_mean=means[k],indicator_SD=sds[k],indicator_median=meds[k],indicator_IQR=iqrs[k],
    component_rank=M[[side]]$rank[,k],zscore=M[[side]]$zscore[,k],robust=M[[side]]$robust[,k])
  }
  classify<-function(df,sf)ifelse(is.na(df)|is.na(sf),"unassessed",ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
  scores<-thresholds<-list()
  for(id in c("rank","raw","robust","zscore")){
   ds<-rowMeans(M$Demand[[id]]);ss<-rowMeans(M$Supply[[id]]);dc<-if(id=="zscore")0 else median(ds,na.rm=TRUE);sc<-if(id=="zscore")0 else median(ss,na.rm=TRUE)
   df<-ds>dc+1e-10;sf<-ss>sc+1e-10
   scores[[id]]<-data.frame(method=id,geo=areas,demand_score=ds,supply_score=ss,demand_flag=df,supply_flag=sf,
     demand_priority_rank=rank(-ds,ties.method="average",na.last="keep"),supply_priority_rank=rank(-ss,ties.method="average",na.last="keep"),category=classify(df,sf))
   thresholds[[id]]<-data.frame(method=id,demand_cutoff=dc,supply_cutoff=sc,demand_rule=if(id=="zscore")"strictly above final-score mean (zero)"else"strictly above final-score median",
    supply_rule=if(id=="zscore")"strictly above constraints-score mean (zero)"else"strictly above constraints-score median",demand_reference_n=sum(is.finite(ds)),supply_reference_n=sum(is.finite(ss)))
  }
  dc<-rowSums(sweep(M$Demand$raw,2,apply(M$Demand$raw,2,median,na.rm=TRUE),"-")>1e-10)
  sc<-rowSums(sweep(M$Supply$raw,2,apply(M$Supply$raw,2,median,na.rm=TRUE),"-")>1e-10)
  scores$median_count<-data.frame(method="median_count",geo=areas,demand_score=dc,supply_score=sc,demand_flag=dc>length(D)/2,supply_flag=sc>length(S)/2,
   demand_priority_rank=rank(-dc,ties.method="average",na.last="keep"),supply_priority_rank=rank(-sc,ties.method="average",na.last="keep"),category=classify(dc>length(D)/2,sc>length(S)/2))
  thresholds$median_count<-data.frame(method="median_count",demand_cutoff=floor(length(D)/2)+1,supply_cutoff=floor(length(S)/2)+1,demand_rule="4 or more of six above component medians",
   supply_rule=paste(floor(length(S)/2)+1,"or more of",length(S),"above constraint-component medians"),demand_reference_n=sum(is.finite(dc)),supply_reference_n=sum(is.finite(sc)))
  scores<-do.call(rbind,scores);thresholds<-do.call(rbind,thresholds);trace<-do.call(rbind,transforms)
  if(!is.null(primary_trace)){
   # Hold references and thresholds fixed within a comparison using the same domains.
   for(side in c("Demand","Supply")){
    fields<-if(side=="Demand")D else S
    for(k in fields){rr<-primary_trace[primary_trace$side==side & primary_trace$variable==k,];ii<-trace$side==side & trace$variable==k
     stopifnot(nrow(rr)==length(areas))
     trace$indicator_mean[ii]<-unique(rr$indicator_mean);trace$indicator_SD[ii]<-unique(rr$indicator_SD)
     trace$zscore[ii]<-(trace$aligned_pct[ii]-unique(rr$indicator_mean))/unique(rr$indicator_SD)
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
     scores$supply_flag[ix]<-scores$supply_score[ix]>cut+1e-10
     scores$supply_priority_rank[ix]<-rank(-scores$supply_score[ix],ties.method="average")
     thresholds$supply_cutoff[thresholds$method=="zscore"]<-cut
    }
   }
   scores$category[ix]<-classify(scores$demand_flag[ix],scores$supply_flag[ix])
  }
  planning<-lapply(areas,function(g){q<-scores[scores$method=="zscore" & scores$geo==g,];dt<-trace[trace$geo==g & trace$side=="Demand",];st<-trace[trace$geo==g & trace$side=="Supply",]
   list(geo=g,name=names_map[[g]],category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,demand_score=q$demand_score,supply_score=q$supply_score,
    top_demand=head(dt$variable[order(-dt$zscore,match(dt$variable,D),na.last=TRUE)],3),
    top_supply=if(all(is.finite(st$zscore)))head(st$variable[order(-st$zscore,match(st$variable,S))],3) else character(),
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
  audit<-list(supply_score_orientation="constraints",country="Senegal",source_rows=nrow(x),consenting_positive_weight=sum(d$positive),need_n=sum(d$need),
   need_variable="Original SURGO FPN2 = 1, retained unchanged. No reconstructed need_n, sterilization or hysterectomy additions.",
   need_qualification="Use the supplied current-need flag as recorded; do not substitute the broader segmentation population or DHS unmet-need algorithm.",
   weight_field="WEIGHT",psu_field="RB0H: recorded District de recensement (census district)",
   survey_design="Final WEIGHT and recorded RB0H census districts, nested in departments and residence. Full consenting positive-weight frame before FPN2 domain selection. Original sampling strata/FPC not documented; no reconstructed strata claimed.",
   survey_psus=length(unique(d$psu[d$positive])),districts=length(areas),departments=length(areas),
   facility_records=nrow(inventory),mapped_facility_records=sum(inventory$geo%in%areas),facility_only_records=sum(!inventory$geo%in%areas),unmapped_facility_records=0,
   eligible_facility_records=nrow(fac),eligible_mapped_facilities=sum(mapped),daily_valid_linked=sum(mapped & is.finite(fac$daily_provision)),daily_undefined_linked=sum(mapped & !is.finite(fac$daily_provision)),stockout_valid_linked=sum(mapped & is.finite(fac$stockout_burden)),health_definition="PB2A only; 1/2 concern, 3/4 no concern, other responses unresolved",
   facility_departments=length(unique(fac$geo)),geography_version="Completed, documented health-district -> department crosswalk used consistently in both input joins. Outside-SURGO departments retained for coverage.",
   supply_resolution="Department-wide facility summaries assigned to every woman in her own department; no Rural/Urban breakdown or fallback.",
   facility_sample=facility_sample,facility_records_used=sum(mapped),
   counseling_included=FALSE,stockout_cleaned_definition_retained=TRUE,daily_provision_included=TRUE,demand_sample=demand_sample,complete_demand_women=sum(dn_complete),primary_reference_fixed=nzchar(reference_dir),
   fallback_source_women=0,fallback_need_women=0,fallback_departments=0,
   assigned_context_profiles=length(areas),facility_residence_missing=sum(is.na(fac$rurb)),
   supply_unavailable="The cleaned three-LARC stock indicator is constant FALSE in Senegal. The existing at-least-one-LARC stock indicator is retained.",
   supply_missing="Valid zeros and FALSE values are included. Daily provision excludes unknown frequency responses from its numerator/denominator and is undefined if no category is recorded offered; undefined facility ratios do not enter the mean. All 33 departments have six finite component means. No partial-index averaging or geographic fallback.",
   supply_denominators="Restrict raw s5_6 to 1 (FP services on-site), then average valid facility values without facility weights. Stockout burden retains original cleaned p_any_stockout. Daily provision uses s5_7a-g: count Daily / count Daily, Weekly, Biweekly or Monthly; Not at all is excluded from the offered denominator. Unknown responses are not zeros. Boolean summaries retain upstream five-decimal rounding.",
   supply_alignment="Heatmaps retain raw stockout burden (higher worse). Before scoring and PCA, convert burden to continuity = 1 - burden, so all six supply inputs are favorable. Average component z-scores for readiness; supply risk is its negative. This equals averaging six risk-aligned z-scores.",
   residence_limitation="Rural/Urban remains a woman-level covariate. Facility residence lookup labels do not determine supply assignment in this report.",
   scoring=paste("Six demand domains and",length(S),"direction-aligned supply domains. Average-z primary with zero (mean) thresholds: demand > 0, supply readiness < 0. Each of 33 linked departments contributes equally to component normalization. Other comparison methods retain their documented rules."),
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
  saveRDS(list(women=d,supply=sp,profiles=profiles,assigned_profiles=ctx,facility_sample=facility_sample),file.path(out,"analysis_frame.rds"))
  cat('Built',sum(d$need),'women in need across',length(areas),'departments; source fallback',sum(fallback),'need fallback',sum(fallback&d$need),'\n')
  print(table(scores$method,scores$category));print(data.frame(field=bar_defs$field,n=vapply(bars,`[[`,numeric(1),"n"),pct=100*vapply(bars,`[[`,numeric(1),"estimate")))

  # Sensitivity analyses ---------------------------------------------------------
  # Rank average, mean percentages, median/IQR and median-count alternatives use
  # the same six components. Final-composite SD maps use 0.5, 1 and 1.5 SD cutoffs.
  source(file.path(.survey_root, "support/risk_scores.R"), local = TRUE)
  payload <- export_risk_scores(payload, out)

}

# Fit models -----------------------------------------------------
.report_steps[["fit_models.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Residence-specific Senegal models with explicit empty-cell fallback; selected supply components and personal demand.
  args <- commandArgs(trailingOnly = TRUE)
  opt <- function(k) { i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L] }
  optional <- function(k,default) {i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-opt("--data-dir");out<-opt("--output-dir");score_method<-optional("--score-method","zscore")
  pca_file<-optional("--pca-score-file","");stopifnot(score_method%in%c("zscore","pca"))
  for(pkg in c("survey","clubSandwich","jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
  script_dir<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
  source(file.path(script_dir,"..","..","..","analysis","senegal","senegal_definitions.R"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
  source(file.path(script_dir,"supply_definitions.R"))
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  favorable_supply<-senegal_favorable_supply(frame$supply)
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables;S<-p$supply_variables
  report_hash<-unname(tools::md5sum(file.path(data_dir,"report_data.json")))
  refs<-list()
  zz<-sapply(D,function(k){ok<-d$need & is.finite(d[[k]]);mu<-survey_weighted_mean(d[[k]][ok],d$weight[ok]);sd<-sqrt(survey_weighted_mean((d[[k]][ok]-mu)^2,d$weight[ok]));stopifnot(sd>0)
   refs[[k]]<<-data.frame(variable=k,weighted_mean=mu,weighted_SD=sd,n_valid_need=sum(ok));(d[[k]]-mu)/sd})
  d$demand_score<-rowMeans(zz)
  write.csv(do.call(rbind,refs),file.path(out,"individual_demand_score_reference.csv"),row.names=FALSE)
  source(file.path(.survey_root,"support/risk_scores.R"), local=TRUE)
  d$supply_risk <- supply_constraint_score(frame$supply, p$component_trace, constraints_reference=identical(p$audit$supply_score_orientation,"constraints"))
  sref<-do.call(rbind,lapply(S,function(k){q<-p$component_trace[p$component_trace$side=="Supply"&p$component_trace$variable==k,]
   data.frame(variable=k,reference_mean_pct=unique(q$indicator_mean),reference_SD_pct=unique(q$indicator_SD),reference_departments=length(unique(q$geo)))}))
  write.csv(sref,file.path(out,"supply_score_reference.csv"),row.names=FALSE)

  if(score_method=="pca"){
   pp<-readRDS(pca_file);stopifnot(identical(pp$scores$row,d$row),pp$audit$report_md5==report_hash)
   d$demand_score<-pp$scores$demand_score;d$supply_risk<-pp$scores$supply_risk
  }
  resolution<-"urban_rural_fallback";demand_resolution<-"individual"
  z<-p$scores[p$scores$method=="zscore",]
  profiles<-unique(cbind(d[c("geo","district","source_profile","fallback")],frame$supply,supply_risk=d$supply_risk))
  stopifnot(nrow(profiles)==49,!anyDuplicated(profiles$source_profile))
  stopifnot(all(vapply(split(d$supply_risk,d$source_profile),function(v)length(unique(v))==1,logical(1))))
  write.csv(profiles,file.path(out,"model_supply_profiles.csv"),row.names=FALSE,na="")
  controls<-c("age10","age10_sq","education","partnered","fertility_goal","urban")
  specs<-list(score_demand=c(controls,"demand_score"),supply=c(controls,"supply_risk"),
   score_joint=c(controls,"demand_score","supply_risk"),components_demand=c(controls,D),components_joint=c(controls,D,"supply_risk"))
  positive<-d$positive
  common_vars<-unique(c("y_modern",controls,D,"demand_score","supply_risk"))
  d$eligible<-d$need & positive & complete.cases(d[common_vars]);cc<-droplevels(d[d$eligible,])
  stopifnot(all(cc$y_modern%in%0:1),all(cc$need),length(unique(cc$geo))>=20,
   all(cc$urban%in%0:1),length(unique(cc$partnered))==2)
  full_design <- surgo_design(d[positive, ])
  des<-subset(full_design,eligible)
  fit_survey_model <- function(fo, a) fit_survey_logistic(fo, a)
  coef_table <- function(f, V) {
    a <- as.data.frame(clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite"))
    lo <- a$beta - qt(.975, a$df_Satt) * a$SE; hi <- a$beta + qt(.975, a$df_Satt) * a$SE
    data.frame(term = rownames(a), estimate = a$beta, se = a$SE, df = a$df_Satt,
      p = a$p_Satt, lo = lo, hi = hi, odds_ratio = exp(a$beta), or_lo = exp(lo), or_hi = exp(hi))
  }
  block_test <- function(f, V, fields) {
    ix <- match(fields, names(coef(f))); stopifnot(!anyNA(ix))
    K <- diag(length(coef(f)))[ix, , drop = FALSE]
    t <- clubSandwich::Wald_test(f, constraints = K, vcov = V, test = "HTZ")
    data.frame(F = t$Fstat, df_num = t$df_num, df_denom = t$df_denom, p = t$p_val)
  }
  vifs <- function(f, a) {
    X <- model.matrix(f)[, -1, drop = FALSE]
    do.call(rbind, lapply(seq_len(ncol(X)), function(j) {
      r <- lm.wfit(cbind(1, X[, -j, drop = FALSE]), X[, j], w = a$weight)
      total <- sum(a$weight * (X[, j] - survey_weighted_mean(X[, j], a$weight))^2)
      data.frame(term = colnames(X)[j], vif = 1 / (sum(a$weight * r$residuals^2) / total))
    }))
  }
  fits <- coefficients <- samples <- tests <- diagnostics <- leaveout <- list()
  for (id in names(specs)) {
    fo <- reformulate(specs[[id]], "y_modern"); f <- fit_survey_logistic(fo, cc, design = des)
    V <- geographic_cr2(f, cc$geo)
    a <- coef_table(f, V); a$model <- id; coefficients[[id]] <- a
    vv <- vifs(f, cc); vv$model <- id; diagnostics[[id]] <- vv
    for (block in c(if (id != "supply") "Demand", if (id %in% c("supply", "score_joint", "components_joint")) "Supply")) {
      fields <- if (block == "Supply") "supply_risk" else if (grepl("^score", id)) "demand_score" else D
      t <- block_test(f, V, fields); t$model <- id; t$block <- block
      tests[[length(tests) + 1L]] <- t
    }
    samples[[id]] <- data.frame(model = id, n = nrow(cc), events = sum(cc$y_modern),
      non_events = sum(cc$y_modern == 0), weighted_modern_use = survey_weighted_mean(cc$y_modern, cc$weight),
      districts = length(unique(cc$geo)), parameters = length(coef(f)), design_rank = f$rank,
      max_vif = max(vv$vif), minimum_df = min(a$df[a$term != "(Intercept)"]),
      min_fitted = min(fitted(f)), max_fitted = max(fitted(f)), converged = f$converged,
      warnings = paste(attr(f, "fit_warnings"), collapse = " | "))
    fields <- intersect(c("demand_score", "supply_risk", D), names(coef(f)))
    for (g in sort(unique(cc$geo))) {
      ff <- fit_survey_model(fo, cc[cc$geo != g, ])
      leaveout[[length(leaveout) + 1L]] <- data.frame(model = id, omitted_department = g, term = fields,
        estimate = coef(ff)[fields], full_estimate = coef(f)[fields], row.names = NULL)
    }
    fits[[id]] <- list(fit = f, V = V, data = cc, formula = fo)
    cat(id, ": N =", nrow(cc), "; events =", sum(cc$y_modern), "; max VIF =", round(max(vv$vif), 2), "\n")
  }
  bind <- function(a) do.call(rbind, a)
  coefficients <- bind(coefficients); samples <- bind(samples); tests <- bind(tests)
  tests$holm_p <- ave(tests$p, tests$model, FUN = function(v) p.adjust(v, "holm"))
  diagnostics <- bind(diagnostics); leaveout <- bind(leaveout)
  savecsv <- function(a, name) write.csv(a, file.path(out, name), row.names = FALSE, na = "")
  savecsv(coefficients, "model_coefficients.csv"); savecsv(samples, "model_samples.csv")
  savecsv(tests, "model_block_tests.csv"); savecsv(diagnostics, "model_vifs.csv")
  savecsv(leaveout, "leave_one_department_out.csv")
  missing <- data.frame(variable = common_vars,
    missing_in_need = vapply(d[d$need, common_vars], function(a) sum(is.na(a)), integer(1)))
  savecsv(missing, "model_missingness.csv")
  savecsv(z[, c("geo", "demand_score", "supply_score")], "model_department_scores.csv")
  # Broader available-case score-only checks isolate the effect of requiring the
  # six individual demand responses for common-sample comparisons.
  available <- d$need & positive & complete.cases(d[c("y_modern", controls, "demand_score", "supply_risk")])
  sensitivity <- list()
  for (id in c("score_demand", "supply", "score_joint")) {
    aa <- d[available, ]; f <- fit_survey_model(fits[[id]]$formula, aa)
    V <- geographic_cr2(f, aa$geo)
    t <- coef_table(f, V); t$model <- id; t$n <- nrow(aa); sensitivity[[id]] <- t
  }
  sensitivity <- bind(sensitivity); savecsv(sensitivity, "broader_score_sample_sensitivity.csv")
  Xmat <- function(f, a) model.matrix(delete.response(terms(f)), a,
    contrasts.arg = f$contrasts, xlev = f$xlevels)[, names(coef(f)), drop = FALSE]
  margin <- function(ob, changes = list()) {
    a <- ob$data
    for (k in names(changes)) a[[k]] <- changes[[k]]
    X <- Xmat(ob$fit, a); pp <- plogis(as.numeric(X %*% coef(ob$fit))); w <- a$weight / sum(a$weight)
    list(estimate = survey_weighted_mean(pp, a$weight), gradient = colSums(X * (w * pp * (1 - pp))))
  }
  ci <- function(ob, estimate, gradient, probability = FALSE) {
    g <- matrix(gradient, nrow = 1)
    lc <- as.data.frame(clubSandwich::linear_contrast(ob$fit, vcov = ob$V, contrasts = g, test = "Satterthwaite"))
    df <- as.numeric(lc[[4]][1]); se <- sqrt(max(0, as.numeric(g %*% ob$V %*% t(g))))
    q <- qt(.975, df)
    if (probability) {
      lo <- plogis(qlogis(estimate) - q * se / (estimate * (1 - estimate)))
      hi <- plogis(qlogis(estimate) + q * se / (estimate * (1 - estimate)))
    } else { lo <- estimate - q * se; hi <- estimate + q * se }
    data.frame(estimate = estimate, se = se, df = df, lo = lo, hi = hi)
  }
  curves <- list()
  for (k in c("demand_score", "supply_risk")) {
    v <- if (k == "supply_risk") unique(cc[, c("geo", "residence", k)])[[k]] else unique(cc[[k]])
    grid <- seq(min(v), max(v), length.out = 41)
    for (id in c(if (k == "demand_score") "score_demand" else "supply", "score_joint")) for (value in grid) {
      ob <- fits[[id]]; a <- margin(ob, setNames(list(value), k))
      t <- ci(ob, a$estimate, a$gradient, TRUE)
      t$model <- id; t$term <- k; t$value <- value
      curves[[length(curves) + 1L]] <- t
    }
  }
  curves <- bind(curves); savecsv(curves, "score_predicted_probabilities.csv")
  gaps <- list(); max_prediction_error <- max_gradient_error <- 0
  for (k in D) {
    ob <- fits$components_joint
    low <- margin(ob, setNames(list(0), k)); high <- margin(ob, setNames(list(1), k))
    grad <- high$gradient - low$gradient; gap <- high$estimate - low$estimate
    t <- ci(ob, gap, grad); t$term <- k; t$probability_absent <- low$estimate; t$probability_present <- high$estimate
    gaps[[k]] <- t
    for (value in 0:1) {
      aa <- ob$data; aa[[k]] <- value
      direct <- survey_weighted_mean(predict(ob$fit, newdata = aa, type = "response"), aa$weight)
      max_prediction_error <- max(max_prediction_error, abs(direct - if (value == 0) low$estimate else high$estimate))
    }
    a0 <- ob$data; a1 <- ob$data; a0[[k]] <- 0; a1[[k]] <- 1
    X0 <- Xmat(ob$fit, a0); X1 <- Xmat(ob$fit, a1); b <- coef(ob$fit); w <- ob$data$weight / sum(ob$data$weight)
    for (j in unique(c(1, match(k, names(b))))) {
      up <- down <- b; up[j] <- up[j] + 1e-6; down[j] <- down[j] - 1e-6
      fun <- function(bb) sum(w * (plogis(as.numeric(X1 %*% bb)) - plogis(as.numeric(X0 %*% bb))))
      max_gradient_error <- max(max_gradient_error, abs((fun(up) - fun(down)) / 2e-6 - grad[j]))
    }
  }
  gaps <- bind(gaps); savecsv(gaps, "individual_demand_probability_gaps.csv")
  stopifnot(max_prediction_error < 1e-10, max_gradient_error < 1e-7)

  source(file.path(script_dir,"..","..","..","administrative_area/analysis/plot_models.R"))
  labels<-render_modern_use_plots(coefficients,curves,gaps,cc,p$demand_labels,D,out,score_method,demand_resolution,resolution,"senegal",
   supply_pca_mixed=score_method=="pca" && length(pp$audit$negative_supply_PC1_loadings)>0)
  influence<-do.call(rbind,lapply(split(leaveout,interaction(leaveout$model,leaveout$term,drop=TRUE)),function(a)
   data.frame(model=a$model[1],term=a$term[1],full_estimate=a$full_estimate[1],deletion_min=min(a$estimate),deletion_max=max(a$estimate),deletion_sign_changes=sum(sign(a$estimate)!=sign(a$full_estimate)))))
  savecsv(influence,"influence_summary.csv")
  saveRDS(fits,file.path(out,"fitted_models.rds"))
  audit<-list(regression_function="survey::svyglm",design_ids="~1",design_strata="None",country="Senegal",outcome="Current modern contraceptive use only: confirmed modern=1; traditional-only or confirmed nonuse=0; unresolved status remains missing.",
   need_definition="Original SURGO FPN2=1, unchanged. No reconstructed need flag or permanent-protection additions.",
   need_n=sum(d$need),common_n=nrow(cc),common_events=sum(cc$y_modern),score_method=score_method,
   supply_components=length(S),counseling_included=FALSE,daily_provision_included=TRUE,stockout_burden_included=TRUE,
   demand_resolution="individual",supply_resolution="Matched departement-residence FP-provider profiles, with flagged departement-wide fallback only for zero-provider residence cells",districts=length(unique(cc$geo)),
   survey_psus=length(unique(cc$psu)),demand_distinct_values=length(unique(cc$demand_score)),supply_distinct_values=length(unique(cc$supply_risk)),
   assigned_context_profiles=length(unique(cc$source_profile)),observed_residence_cells=49L,exact_residence_profiles=33L,fallback_residence_profiles=16L,woman_residence_cells=nrow(unique(cc[c("geo","residence")])),
   fallback_model_women=sum(cc$fallback),fallback_need_women=sum(d$fallback&d$need),
   women_residence_control="FV_MILIEU, retained regardless of facility source level",
   controls=controls,omitted_controls="None of the agreed adjustment domains omitted; all model designs checked for full rank.",
   weights="Original Senegal WEIGHT; common mean-one fitting rescaling. No facility weights.",
   inference="Weighted logistic coefficients with department-clustered CR2 covariance, coefficient-specific Satterthwaite intervals, HTZ block tests and Holm adjustment within each joint model. No random effects or interactions.",
   design="survey::svyglm with ids=~1, no strata or FPC, original SURGO weights; administrative-area CR2 covariance and Satterthwaite tests. Original sampling PSUs are not specified in this model.",
   score_definition=if(score_method=="zscore")"Demand: mean of six personal binary indicators standardized with item-specific weighted mean/population SD in the need group. Supply: transform burden to continuity = 1 - burden, then negative mean of six favorable assigned departement-residence or flagged fallback measures standardized against fixed 33-department component references. Scores enter per one original average-z point." else "Separate weighted demand and supply PCAs; personal demand PC1 and negative assigned residence/fallback supply PC1. Mixed supply signs make it a pattern score, not a uniformly ordered risk index. Scores enter per one original PC1 unit.",
   supply_missing="All 49 observed residence cells have complete assigned inputs: 33 exact, 16 flagged departement-wide fallbacks. Fallback affects 1815 source women, 522 in need. Facility residence is a health-district lookup proxy. No missing indicator is coded zero and no component fallback is made when an exact eligible profile exists.",
   excluded_need=sum(d$need)-nrow(cc),eligible_weight_share=survey_weighted_mean(as.numeric(d$eligible[d$need]),d$weight[d$need]),
   excluded_departments=setdiff(unique(d$geo[d$need]),unique(cc$geo)),
   uncertainty="Intervals condition on estimated scores and do not propagate their construction uncertainty; full original sampling design is not available. Diagnostics are not a formal power analysis or causal identification.",
   probability_checks=list(maximum_direct_prediction_error=max_prediction_error,maximum_numeric_gradient_error=max_gradient_error),
   report_md5=report_hash,source_hashes=p$audit$source_md5)
  payload<-list(audit=audit,coefficients=coefficients,samples=samples,tests=tests,vifs=diagnostics,curves=curves,gaps=gaps,influence=influence,labels=as.list(labels))
  jsonlite::write_json(payload,file.path(out,"model_results.json"),dataframe="rows",auto_unbox=TRUE,digits=12,na="null")
  jsonlite::write_json(audit,file.path(out,"model_audit.json"),auto_unbox=TRUE,pretty=TRUE,digits=12,na="null")
  print(samples);print(tests);print(coefficients[coefficients$term%in%c("demand_score","supply_risk"),])

}

# Build pca scores -----------------------------------------------------
.report_steps[["build_pca_scores.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Separate weighted individual-demand and department-supply PCA sensitivity.
  args<-commandArgs(trailingOnly=TRUE)
  opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-opt("--data-dir");out<-opt("--output-dir")
  for(pkg in c("jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  report_file<-file.path(data_dir,"report_data.json");report<-jsonlite::fromJSON(report_file)
  report_hash<-unname(tools::md5sum(report_file));frame<-readRDS(file.path(data_dir,"analysis_frame.rds"))
  script_dir<-dirname(gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE))
  source(file.path(script_dir,"..","..","..","analysis","senegal","senegal_definitions.R"))
  source(file.path(script_dir,"supply_definitions.R"))
  d<-frame$women;D<-report$demand_variables;S<-report$supply_variables
  eligible<-d$need;dm<-as.matrix(d[D]);sm<-as.matrix(senegal_favorable_supply(frame$supply[S]))
  di<-eligible & complete.cases(dm);si<-eligible & complete.cases(sm)
  weighted_pca <- function(m, weights, anchor = NULL) {
    w <- weights / sum(weights); center <- colSums(m * w)
    centered <- sweep(m, 2, center, "-"); scale <- sqrt(colSums(centered^2 * w))
    stopifnot(all(scale > 0)); Z <- sweep(centered, 2, scale, "/")
    R <- crossprod(Z, Z * w); ee <- eigen(R, symmetric = TRUE)
    rotation <- ee$vectors; rownames(rotation) <- colnames(m); colnames(rotation) <- paste0("PC", 1:ncol(m))
    for (j in seq_len(ncol(rotation))) {
      orient <- if (j == 1 && !is.null(anchor)) sum(rotation[, j] * anchor) else sum(rotation[, j])
      if (orient < 0) rotation[, j] <- -rotation[, j]
    }
    list(center = center, scale = scale, rotation = rotation, eigenvalues = ee$values,
      correlation = R, scores = Z %*% rotation)
  }
  project <- function(f, m) sweep(sweep(m, 2, f$center, "-"), 2, f$scale, "/") %*% f$rotation

  fd<-weighted_pca(dm[di,,drop=FALSE],d$weight[di]);fs<-weighted_pca(sm[si,,drop=FALSE],d$weight[si])
  scores<-data.frame(row=d$row,demand_score=project(fd,dm)[,1],supply_risk=-project(fs,sm)[,1])
  stopifnot(all(is.finite(scores$demand_score[di])),all(is.finite(scores$supply_risk[si])))
  key<-d$source_profile
  profiles<-unique(data.frame(profile=key,geo=d$geo,sm,supply_PC1=-scores$supply_risk,supply_risk=scores$supply_risk))
  stopifnot(!anyDuplicated(profiles$profile));profiles<-profiles[complete.cases(profiles[S]),]
  profiles$women_in_need<-as.integer(table(factor(key[si],levels=profiles$profile)))
  profiles$SURGO_weight<-vapply(profiles$profile,function(k)sum(d$weight[si & key==k]),numeric(1))
  profiles$weight_share<-profiles$SURGO_weight/sum(profiles$SURGO_weight)
  collapsed<-weighted_pca(as.matrix(profiles[S]),profiles$SURGO_weight,fs$rotation[,1])
  collapse_error<-max(abs(collapsed$rotation[,1]-fs$rotation[,1]),abs(collapsed$center-fs$center),abs(collapsed$scale-fs$scale))
  stopifnot(collapse_error<1e-10)
  savecsv<-function(a,name)write.csv(a,file.path(out,name),row.names=FALSE,na="")
  savecsv(profiles,"pca_matched_supply_profiles.csv")
  fits <- list(Demand = fd, Supply = fs); loads <- variance <- moments <- list()
  set.seed(20261106); B <- 2000L
  for (side in names(fits)) {
    f <- fits[[side]]; fields <- if (side == "Demand") D else S
    for (j in seq_along(fields)) loads[[length(loads) + 1L]] <- data.frame(side = side, variable = fields, PC = j,
      loading = f$rotation[, j], squared_loading_share = f$rotation[, j]^2,
      variable_PC_correlation = f$rotation[, j] * sqrt(f$eigenvalues[j]))
    moments[[side]] <- data.frame(side = side, variable = fields, center = f$center, scale = f$scale)
    if (side == "Demand") {
      m <- dm[di, , drop = FALSE]; ww <- d$weight[di]; groups <- split(seq_len(nrow(m)), d$geo[di])
      null <- replicate(B, {
        perm <- m
        for (j in 1:6) for (ii in groups) perm[ii, j] <- sample(m[ii, j], length(ii), replace = FALSE)
        weighted_pca(perm, ww)$eigenvalues
      })
    } else {
      m <- as.matrix(profiles[S]); ww <- profiles$SURGO_weight
      null <- replicate(B, {
        perm <- apply(m, 2, sample)
        weighted_pca(perm, ww)$eigenvalues
      })
    }
    cutoff <- apply(null, 1, quantile, probs = .95, type = 7, names = FALSE)
    variance[[side]] <- data.frame(side = side, PC = seq_along(fields), eigenvalue = f$eigenvalues,
      variance_pct = 100 * f$eigenvalues / sum(f$eigenvalues),
      cumulative_pct = 100 * cumsum(f$eigenvalues) / sum(f$eigenvalues),
      permutation_95 = cutoff, above_permutation_95 = f$eigenvalues > cutoff)
  }
  loads <- do.call(rbind, loads); variance <- do.call(rbind, variance); moments <- do.call(rbind, moments)
  savecsv(loads, "pca_loadings.csv"); savecsv(variance, "pca_variance.csv"); savecsv(moments, "pca_centers_scales.csv")
  savecsv(data.frame(variable = D, fd$correlation, check.names = FALSE), "pca_demand_correlations.csv")
  savecsv(data.frame(variable = S, fs$correlation, check.names = FALSE), "pca_supply_correlations.csv")
  stability <- list()
  for (g in sort(unique(d$geo[eligible]))) for (side in names(fits)) {
    m <- if (side == "Demand") dm else sm; use <- if (side == "Demand") di else si
    fit <- fits[[side]]; train <- use & d$geo != g
    f <- weighted_pca(m[train, , drop = FALSE], d$weight[train], fit$rotation[, 1])
    full_score <- project(fit, m[use, , drop = FALSE])[, 1]
    alt_score <- project(f, m[use, , drop = FALSE])[, 1]
    stability[[length(stability) + 1L]] <- data.frame(side = side, omitted_department = g,
      loading_cosine = sum(f$rotation[, 1] * fit$rotation[, 1]),
      score_rank_correlation = cor(full_score, alt_score, method = "spearman"),
      pc1_variance_pct = 100 * f$eigenvalues[1] / sum(f$eigenvalues))
  }
  stability <- do.call(rbind, stability); savecsv(stability, "pca_leave_one_department_out.csv")
  pc1 <- loads[loads$PC == 1, ]
  audit<-list(method="Separate final-weighted correlation PCAs on individual demand and assigned residence/fallback supply; PC1 prespecified as sensitivity.",
   need_n=sum(eligible),demand_PCA_n=sum(di),supply_PCA_women_n=sum(si),supply_profiles=nrow(profiles),
   demand_PC1_variance=variance$variance_pct[variance$side=="Demand" & variance$PC==1],
   supply_PC1_variance=variance$variance_pct[variance$side=="Supply" & variance$PC==1],
   negative_demand_PC1_loadings=pc1$variable[pc1$side=="Demand" & pc1$loading<0],
   negative_supply_PC1_loadings=pc1$variable[pc1$side=="Supply" & pc1$loading<0],
   supply_collapse_equivalence_error=collapse_error,report_md5=report_hash,
   weighting="SURGO WEIGHT on both sides. Supply PCA is equivalent to assigned residence/fallback profiles weighted by summed weights of represented women; these are not facility weights.",
   missingness="Require all six demand and all selected supply indicators. No zero coding or partial scores. Flagged departement-wide fallback is used only when an exact residence cell has zero FP providers.",
   alignment="Before PCA, raw stockout burden is replaced by continuity = 1 - burden; all six supply columns have favorable direction.",
   sign="Demand PC1 oriented toward average concern; supply PC1 toward average favorable provision and negated for modeling. Mixed signs remain and are explicitly reported.",
   uncertainty="Permutation and department-deletion checks assess exploratory construction stability, not validated survey-design inference. Regression intervals condition on estimated scores.",
   source_unchanged=report_hash==unname(tools::md5sum(report_file)))
  obj<-list(audit=audit,scores=scores,demand_fit=fd,supply_fit=fs,profiles=profiles,loadings=loads,variance=variance,moments=moments,stability=stability)
  saveRDS(obj,file.path(out,"pca_scores.rds"))
  jsonlite::write_json(obj[setdiff(names(obj),c("scores","demand_fit","supply_fit"))],file.path(out,"pca_results.json"),dataframe="rows",auto_unbox=TRUE,na="null",digits=12)
  jsonlite::write_json(audit,file.path(out,"pca_audit.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
  labels <- setNames(c(report$demand_labels, report$supply_labels), c(D, S))
  labels["stockout_burden"] <- "Stock continuity (1 - stockout burden)"
  base <- ggplot2::theme_minimal(base_family = "sans", base_size = 11) + ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 18, color = "#17374a"),
    plot.subtitle = ggplot2::element_text(size = 10, color = "#607985", lineheight = 1.2),
    plot.caption = ggplot2::element_text(hjust = 0, color = "#607985", size = 9),
    strip.background = ggplot2::element_rect(fill = "#edf4f6", color = NA),
    strip.text = ggplot2::element_text(face = "bold", color = "#17374a"),
    panel.spacing = grid::unit(1.5, "lines"), legend.position = "bottom",
    plot.margin = ggplot2::margin(14, 18, 12, 12))
  saveplot <- function(g, name, height) {
    ggplot2::ggsave(file.path(out, paste0(name, ".png")), g, width = 14, height = height, dpi = 170, bg = "white")
    ggplot2::ggsave(file.path(out, paste0(name, ".svg")), g, width = 14, height = height, device = svglite::svglite, bg = "white")
  }
  pc1$row <- factor(pc1$variable, levels = rev(c(D, S)))
  pc1$direction <- ifelse(pc1$loading < 0, "Negative coefficient", pc1$side)
  pc1$panel <- ifelse(pc1$side == "Demand", sprintf("Individual demand PC1 · %.1f%% of variation", audit$demand_PC1_variance),
    sprintf(if(length(audit$negative_supply_PC1_loadings)>0)"Supply pattern PC1 · %.1f%% of variation"else"Favorable supply PC1 · %.1f%% of variation", audit$supply_PC1_variance))
  pc1$panel <- factor(pc1$panel, levels = c(sprintf("Individual demand PC1 · %.1f%% of variation", audit$demand_PC1_variance),
    sprintf(if(length(audit$negative_supply_PC1_loadings)>0)"Supply pattern PC1 · %.1f%% of variation"else"Favorable supply PC1 · %.1f%% of variation", audit$supply_PC1_variance)))
  g <- ggplot2::ggplot(pc1, ggplot2::aes(loading, row, fill = direction)) +
    ggplot2::geom_vline(xintercept = 0, color = "#8da2ad") + ggplot2::geom_col(width = .55) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.2f", loading), hjust = ifelse(loading < 0, 1.15, -.15)), size = 3.5) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") + ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::scale_fill_manual(values = c(Demand = "#087f8a", Supply = "#be6b23", "Negative coefficient" = "#8b99a3")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .2)) +
    ggplot2::labs(title = "What the PCA scores emphasize", subtitle = "Signed coefficients on standardized inputs; demand uses individual responses, supply uses matched department-wide exposures",
      x = "PC1 scoring coefficient (not an association with contraceptive use)", y = NULL, fill = NULL,
      caption = "Supply coefficients show PC1 before the model sign reversal; mixed signs indicate a supply pattern. Inspect signed coefficients: a negative value indicates a contrast within PC1.\nWeights describe variation in the inputs, not program importance or causal effects.") + base
  saveplot(g, "01_pca_loadings", 6)
  g <- ggplot2::ggplot(variance, ggplot2::aes(PC, eigenvalue, fill = side)) + ggplot2::geom_col(width = .55) +
    ggplot2::geom_line(ggplot2::aes(y = permutation_95), color = "#718997", linewidth = .7) +
    ggplot2::geom_point(ggplot2::aes(y = permutation_95), color = "#718997", size = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", variance_pct)), vjust = -.4, size = 3.5) +
    ggplot2::facet_wrap(~side, nrow = 1) + ggplot2::scale_x_continuous(breaks = 1:6) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
    ggplot2::scale_fill_manual(values = c(Demand = "#087f8a", Supply = "#be6b23"), guide = "none") +
    ggplot2::labs(title = "How much of each side's variation does PCA retain?", subtitle = "Bars: observed eigenvalues and variance shares · gray line: exploratory permutation 95th percentile",
      x = "Principal component", y = "Eigenvalue", caption = "2,000 permutations per side. Null constructions differ by level; these are descriptive dimensionality diagnostics, not survey-design tests.") + base
  saveplot(g, "02_pca_variance", 5.5)
  print(audit); print(pc1[, c("side", "variable", "loading")]); print(variance)

}

# Prepare map -----------------------------------------------------
.report_steps[["prepare_map.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Department boundaries are for display; facility assignments use the audited lookup.
  args<-commandArgs(trailingOnly=TRUE)
  opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  boundary<-opt("--boundary");data_dir<-opt("--data-dir")
  for(pkg in c("sf","jsonlite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
  here<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
  source(file.path(here,"..","..","..","analysis","senegal","senegal_definitions.R"))
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"))
  b<-sf::st_read(boundary,quiet=TRUE);stopifnot(nrow(b)==46)
  b$geo<-senegal_key(b$adm2_name)
  aliases<-c(NIORODURIP="NIORO",MEDINAYOROFOULA="MEDINAYOROFOULAH",RANEROU="RANEROUFERLO")
  ii<-match(b$geo,names(aliases));b$geo[!is.na(ii)]<-aliases[ii[!is.na(ii)]]
  coverage<-p$coverage[p$coverage$geo!="SENEGAL",]
  stopifnot(!anyDuplicated(b$geo),setequal(b$geo,coverage$geo),length(intersect(b$geo,p$profiles$geo))==33)
  write.csv(data.frame(boundary_name=b$adm2_name,join_geo=b$geo,join_name=coverage$name[match(b$geo,coverage$geo)],
   boundary_pcode=b$adm2_pcode,exact_normalized_name=senegal_key(b$adm2_name)==b$geo),
   file.path(data_dir,"map_name_crosswalk.csv"),row.names=FALSE)
  utm<-sf::st_transform(sf::st_make_valid(b),32628)
  utm<-sf::st_simplify(utm,dTolerance=120,preserveTopology=TRUE)
  ct<-sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(utm)))
  features<-lapply(seq_len(nrow(utm)),function(i){g<-sf::st_geometry(utm)[[i]]
   rings<-if(inherits(g,"POLYGON"))unclass(g) else unlist(unclass(g),recursive=FALSE)
   rings<-lapply(rings,function(z)unname(as.matrix(z)))
   list(geo=utm$geo[i],name=utm$adm2_name[i],rings=unname(rings),center=unname(ct[i,1:2]),
    inset=utm$geo[i]%in%c("DAKAR","GUEDIAWAYE","PIKINE","KEURMASSAR","RUFISQUE"))})
  jsonlite::write_json(list(crs="EPSG:32628",features=features),file.path(data_dir,"map_layout.json"),auto_unbox=TRUE,digits=10)
  dir.create(file.path(data_dir,"assets"),showWarnings=FALSE)
  file.copy(boundary,file.path(data_dir,"assets/senegal_departments.geojson"),overwrite=TRUE)
  jsonlite::write_json(list(source="https://data.humdata.org/dataset/cod-ab-sen",source_name="Government of Senegal / OCHA administrative boundaries",
   snapshot_md5=unname(tools::md5sum(boundary)),departments=46,display_simplification_metres=120,
   qualification="Boundary snapshot is display geography; no facility point-in-polygon assignment or GPS inference."),
   file.path(data_dir,"assets/boundary_metadata.json"),auto_unbox=TRUE,pretty=TRUE)
  cat('Verified all 46 department polygons: 33 both-source and 13 facility-only departments.\n')

}

# Verify analysis -----------------------------------------------------
.report_steps[["verify_analysis.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Independently verify source preservation, geography, exposure assignment and fits.
  args<-commandArgs(trailingOnly=TRUE)
  opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  input<-opt("--input");admin_input<-input;raw_file<-opt("--raw")
  facility_file<-opt("--facility-input");raw_facility<-opt("--raw-facility");data_dir<-opt("--data-dir")
  cleaned_facility<-opt("--cleaned-facility");residence_lookup<-opt("--residence-lookup")
  library(haven);library(jsonlite);library(clubSandwich)
  out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
  x<-readRDS(input);admin<-readRDS(admin_input);raw<-read_dta(raw_file)
  hybrid<-x
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  p<-fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables;S<-p$supply_variables
  checks<-list();add<-function(name,ok,detail=""){
   if(!isTRUE(ok))stop("Verification failed: ",name," ",detail)
   checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE,detail=as.character(detail))}
  same<-function(a,b)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=1e-12))
  add("All original women and IDs retained",nrow(raw)==7169&&nrow(hybrid)==7169&&identical(raw$IDW,hybrid$IDW))
  raw_same<-vapply(names(raw),function(k)same(raw[[k]],hybrid[[k]]),logical(1))
  add("Every raw SURGO field unchanged",all(raw_same),length(raw_same))
  num<-function(k)as.numeric(raw[[k]])
  valid<-num("IRB1")==1&is.finite(num("WEIGHT"))&num("WEIGHT")>0
  need<-valid&num("FPN2")%in%1
  add("Original SURGO need flag, not a reconstructed flag",identical(need,d$need)&&sum(need)==1905)
  add("Consent and positive weights",identical(valid,d$positive)&&sum(valid)==7158)
  add("Original final survey weights",identical(d$weight,num("WEIGHT")))
  add("Recorded census district retained",identical(d$psu,as.character(raw$RB0H)))
  key<-function(v)toupper(gsub("[^A-Za-z0-9]","",iconv(as.character(v),to="ASCII//TRANSLIT")))
  add("Department matches original woman label",identical(d$geo,key(as.character(haven::as_factor(raw$RB0C)))))
  add("Woman residence matches original FV_MILIEU",identical(d$urban,as.numeric(num("FV_MILIEU")==2)))
  add("Census districts nest in departments",all(vapply(split(d$geo[valid],d$psu[valid]),function(v)length(unique(v))==1,logical(1))))
  add("Census districts nest in residence",all(vapply(split(d$urban[valid],d$psu[valid]),function(v)length(unique(v))==1,logical(1))))
  fallback<-d$fallback
  add("Every source woman has assigned FP supply",all(x$link_has_supply)&&all(d$has_fp_supply))
  add("Fallback flagged for 1815 source and 522 need women",sum(fallback)==1815&&sum(fallback&need)==522&&length(unique(d$geo[fallback]))==16)
  add("Requested residence remains unchanged",identical(d$residence,ifelse(num("FV_MILIEU")==1,"Rural","Urban")))
  add("49 complete assigned residence groups",nrow(frame$residence_profiles)==49&&all(frame$residence_profiles$complete_supply))
  reconciliation<-read.csv(file.path(data_dir,"supply_input_reconciliation.csv"))
  add("Every baseline department planning component reconciled",nrow(reconciliation)==492&&all(reconciliation$passed))
  add("33 department profiles with daily provision and stockout burden",nrow(frame$profiles)==33&&all(complete.cases(frame$profiles[S]))&&identical(S,c("offering","free","training","daily_provision","larc_stock","stockout_burden")))
  fac<-readRDS(facility_file);fac<-fac[fac$country=="senegal",];fac$geo<-key(fac$assigned_join_area)
  cleaned<-read.csv(cleaned_facility,stringsAsFactors=FALSE,check.names=FALSE)
  cleaned<-cleaned[tolower(cleaned$loc)=="senegal",]
  ci<-match(fac$fid,cleaned$fid)
  add("Every stockout burden equals original cleaned UoM export",nrow(cleaned)==2043&&!anyDuplicated(cleaned$fid)&&!anyNA(ci)&&same(as.numeric(fac$p_any_stockout),as.numeric(cleaned$p_any_stockout[ci])))
  raw_fp<-read_dta(raw_facility,col_select=all_of(c("fid","s1_3","s5_6",paste0("s5_7",letters[1:7]))))
  ii<-match(fac$fid,as.character(haven::as_factor(raw_fp$fid)));add("Every facility linked to original eligibility response",!anyNA(ii))
  fp<-as.numeric(raw_fp$s5_6[ii])
  lut<-readxl::read_xlsx(residence_lookup,range="A1:B79")
  li<-match(key(as.character(haven::as_factor(raw_fp$s1_3[ii]))),key(lut$s1_3))
  rv<-as.numeric(lut$urbanrural[li]);proxy<-ifelse(rv%in%1,"Rural",ifelse(rv%in%2,"Urban",NA_character_))
  add("Facility residence independently matches health-district lookup",identical(proxy,as.character(fac$rurb)))
  add("Nine unknown proxy-residence records retained as unknown",sum(is.na(proxy))==9)
  freq<-as.matrix(data.frame(lapply(raw_fp[ii,paste0("s5_7",letters[1:7])],as.numeric)))
  frequency_daily<-apply(freq,1,function(v){offered<-v[!is.na(v)&v>=1&v<=4];if(length(offered))sum(offered==1)/length(offered)else NA_real_})
  fac$daily_provision<-frequency_daily
  add("Independent daily numerator and offered-method denominator",all(frequency_daily[is.finite(frequency_daily)]>=0&frequency_daily[is.finite(frequency_daily)]<=1))
  inventory<-fac;fac<-fac[fp%in%1,]
  add("FP-provider restriction is Yes only",nrow(fac)==1941&&!anyNA(fp[fp%in%1]))
  boolean<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
  fac$offering<-boolean(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
  fac$training<-as.numeric(fac$p_train)
  fac$larc_stock<-boolean(fac$one_larc_stockin);fac$stockout_burden<-as.numeric(fac$p_any_stockout)
  add("Known FP-provider inventory",sum(fac$geo%in%unique(d$geo))==1568)
  linked<-fac$geo%in%unique(d$geo)
  add("Stockout burden exactly original cleaned values",all(is.finite(fac$stockout_burden[linked]))&&sum(linked)==1568)
  add("Daily usable versus undefined denominators",sum(linked&is.finite(fac$daily_provision))==1560&&sum(linked&is.na(fac$daily_provision))==8)
  add("No counseling or popular-stock component in main analysis",!any(c("counselor","popular_stock")%in%S))
  for(g in unique(d$geo)){
   ff<-fac[fac$geo==g,]
   add(paste("Department has facility evidence",g),nrow(ff)>0,nrow(ff))
   for(k in S){
    v<-ff[[k]];v<-v[is.finite(v)];expected<-mean(v)
    pp<-p$heatmap[p$heatmap$side=="Supply"&p$heatmap$geo==g&p$heatmap$variable==k,]
    add(paste("Planning FP-only department mean",g,k),abs(expected-pp$value/100)<=5.00001e-6&&pp$n_valid==length(v))
    for(res in unique(d$residence[d$geo==g])){
     use<-ff$rurb%in%res;fallback_expected<-sum(use)==0
     selected<-if(fallback_expected)ff else ff[use,]
     vals<-selected[[k]];expected<-mean(vals[is.finite(vals)])
     rows<-d$geo==g&d$residence==res
     add(paste("Residence or authorized department mean",g,res,k),all(abs(frame$supply[[k]][rows]-expected)<1e-12))
     add(paste("Fallback only with zero exact FP providers",g,res,k),all(d$fallback[rows]==fallback_expected))
    }
   }
  }
  add("All 46 facility departments kept for coverage",length(unique(inventory$geo))==46&&nrow(inventory)==2043)
  add("Mbacke fallback equals available urban profile",all(vapply(frame$supply[d$geo=="MBACKE",S],function(v)length(unique(v))==1,logical(1))))
  add("Three-LARC indicator is constant, not secretly modeled",all(boolean(fac$three_larc_stockin)==0))
  item<-function(k,pos,valid){v<-num(k);ifelse(v%in%valid,as.integer(v%in%pos),NA_real_)}
  combine<-function(mat)apply(mat,1,function(v)if(any(v==1,na.rm=TRUE))1 else if(anyNA(v))NA_real_ else 0)
  ind<-list(agency=item("HS7C",1,1:2),stigma=combine(cbind(item("PB7B",1:2,1:4),item("PB7C",1:2,1:4))),
   health=item("PB2A",1:2,1:4),
   confidence=combine(cbind(item("MC5A",3:4,1:4),item("MC5B",3:4,1:4))),opposes=item("FP1",3:4,1:4))
  mp<-sapply(paste0("MP4_",letters[1:9]),num)
  ind$fertility<-vapply(seq_len(nrow(mp)),function(i){v<-mp[i,]
   if(!all(v%in%0:1)||num("MP4_99")[i]!=0||!any(v==1)||v[9]==1&&any(v[1:8]==1))return(NA_real_)
   as.numeric(any(v[1:6]==1))},numeric(1))
  for(k in D)add(paste("Independent demand coding",k),same(ind[[k]],d[[k]]))
  unknown<-do.call(rbind,lapply(D,function(k)data.frame(variable=k,n_unresolved=sum(need&is.na(d[[k]])),
   weighted_pct=100*weighted.mean(is.na(d[[k]])[need],d$weight[need]))))
  write.csv(unknown,file.path(out,"demand_unresolved_summary.csv"),row.names=FALSE)
  item_unknown<-do.call(rbind,lapply(c("HS7C","PB7B","PB7C","PB2A","PB2B","PB2C","MP4_99","MC5A","MC5B","FP1"),function(k){
   v<-num(k);codes<-if(k=="MP4_99")1 else if(k=="FP1")c(5,99) else 99
   un<-v%in%codes
   data.frame(variable=k,unresolved_codes=paste(codes,collapse=";"),n=sum(need&un),blank_n=sum(need&is.na(v)),
    weighted_pct=100*weighted.mean(un[need],d$weight[need]))}))
  write.csv(item_unknown,file.path(out,"unresolved_item_responses.csv"),row.names=FALSE)
  prompt<-as.matrix(raw[paste0("CU5",LETTERS[1:4])])
  current<-ifelse(num("CU1")%in%c(1,3)|rowSums(prompt==1,na.rm=TRUE)>0,1,
   ifelse(num("CU1")%in%2&rowSums(prompt==2,na.rm=TRUE)==4,0,NA_real_))
  modern<-rowSums(as.matrix(raw[paste0("CU4_",1:8)])==1,na.rm=TRUE)>0|num("CU5C")%in%1|num("CU5D")%in%1|
   (num("CU4_11")%in%1&num("CU4_lam")%in%1)
  traditional<-rowSums(as.matrix(raw[paste0("CU4_",c(9,10,12:15))])==1,na.rm=TRUE)>0|num("CU5A")%in%1|num("CU5B")%in%1
  y<-ifelse(current%in%0,0,ifelse(current%in%1&modern,1,ifelse(current%in%1&traditional,0,NA_real_)))
  add("Independent modern-use outcome",same(y,d$y_modern))
  for(i in seq_len(nrow(p$bars))){b<-p$bars[i,];v<-d[[b$field]][need];w<-d$weight[need]
   add(paste("Weighted need-group bar",b$field),abs(sum(v*w)/sum(w)-b$estimate)<1e-10&&sum(v)==b$n&&b$denominator==1905)}
  for(g in unique(d$geo))for(k in D){r<-p$heatmap[p$heatmap$geo==g&p$heatmap$variable==k&p$heatmap$side=="Demand",]
   ok<-need&d$geo==g&is.finite(ind[[k]])
   add(paste("Need-group heatmap",g,k),abs(100*weighted.mean(ind[[k]][ok],d$weight[ok])-r$value)<1e-9&&sum(ok)==r$n_valid)
  }
  wcorr<-function(a,b,w){ok<-is.finite(a)&is.finite(b)&is.finite(w)&w>0;a<-a[ok];b<-b[ok];w<-w[ok]/sum(w[ok]);
   a<-a-sum(a*w);b<-b-sum(b*w);sum(w*a*b)/sqrt(sum(w*a^2)*sum(w*b^2))}
  corr<-list()
  for(side in c("Demand","Supply")){
   fields<-if(side=="Demand")D else S;mat<-if(side=="Demand")d[need,fields]else frame$profiles[fields]
   ww<-if(side=="Demand")d$weight[need]else rep(1,nrow(mat))
   for(j in seq_along(fields))for(k in seq_along(fields))corr[[length(corr)+1L]]<-data.frame(side=side,item1=fields[j],item2=fields[k],
    correlation=wcorr(mat[[j]],mat[[k]],ww),n_pair=sum(complete.cases(mat[c(j,k)])))
  }
  write.csv(do.call(rbind,corr),file.path(out,"component_correlations.csv"),row.names=FALSE)
  z<-sapply(D,function(k){v<-ind[[k]];ok<-need&is.finite(v);mu<-weighted.mean(v[ok],d$weight[ok]);
   sd<-sqrt(weighted.mean((v[ok]-mu)^2,d$weight[ok]));(v-mu)/sd})
  personal<-rowSums(z)/6
  favorable_supply<-frame$supply[S];favorable_supply$stockout_burden<-1-favorable_supply$stockout_burden
  zs<-sapply(S,function(k){r<-p$component_trace[p$component_trace$side=="Supply"&p$component_trace$variable==k,];
   (100*(1-favorable_supply[[k]])-unique(r$indicator_mean))/unique(r$indicator_SD)})
  supply<-rowSums(zs)/length(S)
  risk_raw<-frame$profiles[S]
  for(k in setdiff(S,"stockout_burden"))risk_raw[[k]]<-1-risk_raw[[k]]
  risk_check<-rowMeans(scale(risk_raw))
  reported<-p$scores[p$scores$method=="zscore",]
  add("Risk-first and readiness-first standardization equivalent",max(abs(risk_check-reported$supply_score[match(frame$profiles$geo,reported$geo)]))<1e-10)
  add("Stockout high-is-worse sign applied before scoring",all(p$component_trace$higher_raw_is_worse[p$component_trace$variable=="stockout_burden"])&&max(abs(p$component_trace$aligned_pct[p$component_trace$variable=="stockout_burden"]-p$component_trace$original_pct[p$component_trace$variable=="stockout_burden"]))<1e-10)
  pv<-readRDS(file.path(data_dir,"pca_appendix/pca_scores.rds"));max_svd<-0
  for(side in c("demand","supply")){
   f<-pv[[paste0(side,"_fit")]];mat<-if(side=="demand")as.matrix(d[D])else as.matrix(favorable_supply)
   use<-need&complete.cases(mat);w<-d$weight[use]/sum(d$weight[use])
   z0<-sweep(sweep(mat[use,,drop=FALSE],2,f$center,"-"),2,f$scale,"/")
   sv<-svd(z0*sqrt(w));err<-max(abs(sv$d^2-f$eigenvalues));max_svd<-max(max_svd,err)
   add(paste("Independent weighted SVD",side),err<1e-10,err)
   score<-drop(sweep(sweep(mat,2,f$center,"-"),2,f$scale,"/")%*%f$rotation[,1]);if(side=="supply")score<- -score
   saved<-if(side=="demand")pv$scores$demand_score else pv$scores$supply_risk
   add(paste("PCA score for every woman",side),same(score,saved))
  }
  previous<-NULL
  for(method in c("zscore","pca")){
   folder<-if(method=="zscore")file.path(data_dir,"models_zscore")else file.path(data_dir,"pca_appendix/models")
   fits<-readRDS(file.path(folder,"fitted_models.rds"))
   for(id in names(fits)){
    ob<-fits[[id]];dd<-ob$data;ix<-dd$row
    expectedD<-if(method=="zscore")personal[ix]else pv$scores$demand_score[ix]
    expectedS<-if(method=="zscore")supply[ix]else pv$scores$supply_risk[ix]
    add(paste(method,id,"personal demand assignment"),max(abs(dd$demand_score-expectedD))<1e-12)
    add(paste(method,id,"residence-or-fallback supply assignment"),max(abs(dd$supply_risk-expectedS))<1e-12)
    add(paste(method,id,"need-only common sample and modern outcome"),nrow(dd)==sum(need&complete.cases(d[c("y_modern","age10","age10_sq","education","partnered","fertility_goal","urban",D)]))&&sum(dd$y_modern)==sum(d$y_modern[dd$row])&&all(dd$need)&&all(dd$y_modern%in%0:1))
    add(paste(method,id,"agreed covariates including residence"),all(c("age10","age10_sq","education","partnered","fertility_goal","urban")%in%all.vars(ob$formula)))
    add(paste(method,id,"no random effects or interactions"),!grepl("[|:*]",paste(deparse(ob$formula),collapse=" ")))
    add(paste(method,id,"same women in all ten specifications"),is.null(previous)||identical(previous,ix));previous<-ix
    dd$independent_w<-dd$weight/mean(dd$weight)
    ff<-glm(ob$formula,data=dd,weights=independent_w,family=quasibinomial(),control=glm.control(maxit=100))
    add(paste(method,id,"independent coefficient reproduction"),max(abs(coef(ff)-coef(ob$fit)))<1e-10)
    V<-clubSandwich::vcovCR(ff,cluster=dd$geo,type="CR2",inverse_var=FALSE)
    add(paste(method,id,"department CR2 reproduction"),max(abs(V-ob$V))<1e-10)
   }
  }
  zm<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"));pm<-readRDS(file.path(data_dir,"pca_appendix/models/fitted_models.rds"))
  add("Demand-indicator-only fits identical across score methods",identical(coef(zm$components_demand$fit),coef(pm$components_demand$fit)))
  add("49 assigned groups and 39 numeric scores across 33 departments",length(unique(zm$score_joint$data$supply_risk))==39&&length(unique(zm$score_joint$data$geo))==33&&length(unique(zm$score_joint$data$source_profile))==49&&
   all(vapply(split(zm$score_joint$data$supply_risk,zm$score_joint$data$source_profile),function(v)length(unique(v))==1,logical(1))))
  # Recompute the primary and median-count classifications independently.
  areas<-sort(unique(d$geo)); hm<-p$heatmap
  for(side in c("Demand","Supply")){
   fields<-if(side=="Demand")D else S
   mat<-sapply(fields,function(k){rr<-hm[hm$side==side&hm$variable==k,];rr$value[match(areas,rr$geo)]})
   if(side=="Supply")mat[,"stockout_burden"]<-100-mat[,"stockout_burden"]
   if(side=="Supply" && isTRUE(p$audit$primary_reference_fixed)){
    tt<-p$component_trace[p$component_trace$side==side,]
    zz<-sapply(seq_along(fields),function(j){rr<-tt[tt$variable==fields[j],];(mat[,j]-unique(rr$indicator_mean))/unique(rr$indicator_SD)})
   }else zz<-scale(mat)
   expected<-rowMeans(zz);rr<-p$scores[p$scores$method=="zscore",];rr<-rr[match(areas,rr$geo),]
   observed<-if(side=="Demand")rr$demand_score else -rr$supply_score
   add(paste("Independent department z-score",side),max(abs(expected-observed))<1e-9)
   threshold<-p$thresholds[p$thresholds$method=="zscore",if(side=="Demand")"demand_cutoff"else"supply_cutoff"]
   add(paste("Final score is centered and primary threshold is zero",side),abs(mean(expected))<1e-12&&threshold==0)
   flag<-if(side=="Demand")expected>threshold+1e-10 else expected<threshold-1e-10
   add(paste("Independent zero-mean classification",side),identical(flag,if(side=="Demand")rr$demand_flag else rr$supply_flag))
  }
  model_rows<-zm$score_joint$data$row
  remaining<-need
  flow<-list(data.frame(step="Original SURGO FPN2 = 1",excluded_at_step=0,remaining=sum(remaining)))
  for(step in c("Demand indicators","Modern-use outcome","Partnership","Other covariates")){
   fields<-switch(step,"Demand indicators"=D,"Modern-use outcome"="y_modern","Partnership"="partnered",
    "Other covariates"=c("age10","age10_sq","education","fertility_goal","urban"))
   remove<-remaining&!complete.cases(d[fields]);remaining<-remaining&!remove
   flow[[length(flow)+1L]]<-data.frame(step=step,excluded_at_step=sum(remove),remaining=sum(remaining))
  }
  add("Complete-case sample flow reproduced",identical(which(remaining),model_rows))
  write.csv(do.call(rbind,flow),file.path(out,"model_sample_flow.csv"),row.names=FALSE)
  samples<-do.call(rbind,lapply(c(TRUE,FALSE),function(in_model){ok<-need&(d$row%in%model_rows)==in_model
   wmean<-function(v){valid<-ok&is.finite(v);weighted.mean(v[valid],d$weight[valid])}
   data.frame(group=if(in_model)"Included in common models"else"Excluded from common models",n=sum(ok),
    weighted_share_of_need=sum(d$weight[ok])/sum(d$weight[need]),modern_use_among_known=wmean(d$y_modern),
    urban_share=wmean(d$urban),fallback_share=wmean(as.numeric(d$fallback)))}))
  write.csv(samples,file.path(out,"model_sample_comparison.csv"),row.names=FALSE)
  save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
  write_json(list(check_count=nrow(save),all_passed=all(save$passed),source_rows=nrow(x),need_n=sum(need),model_n=nrow(zm$score_joint$data),
   models=10,source_raw_columns_verified=length(raw_same),facility_component_checks=nrow(reconciliation),
   woman_supply_value_checks=nrow(d)*length(S),weighted_SVD_max_error=max_svd,all_models_same_women=TRUE,
   all_model_outcomes_modern=TRUE,no_random_effects=TRUE,fp_providers_only=TRUE,residence_fallback_verified=TRUE,health_district_residence_proxy=TRUE,health_PB2A_only=TRUE,primary_threshold_zero=TRUE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
  cat("Passed",nrow(save),"independent checks.\n")

}

# Assign residence supply -----------------------------------------------------
.report_steps[["assign_residence_supply.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Reaggregate FP providers in residence cells; use flagged LGA means for empty cells.
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  input<-arg("--input");facility_input<-arg("--facility-input");raw_facility<-arg("--raw-facility")
  dictionary_file<-arg("--dictionary");data_dir<-arg("--data-dir");out<-arg("--output-dir");residence_lookup<-arg("--residence-lookup")
  script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
  project_root <- .report_root
  root<-file.path(project_root,"administrative_area")
  source(file.path(root,"R/helpers.R"))
  source(file.path(root,"R/aggregate_supply.R"))
  source(file.path(project_root,"analysis/senegal/senegal_definitions.R"))
  source(file.path(project_root,"administrative_area/analysis/senegal/supply_definitions.R"))
  key<-senegal_key
  mean_valid<-function(v){v<-as.numeric(v);if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_}
  source_hash<-unname(tools::md5sum(c(input,facility_input,raw_facility,residence_lookup)))
  x<-readRDS(input);frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  stopifnot(nrow(x)==7169,identical(as.character(x$IDW),d$id),identical(key(x$link_geo_join),d$geo),
   identical(as.character(x$link_rurb),ifelse(as.numeric(x$FV_MILIEU)==1,"Rural",ifelse(as.numeric(x$FV_MILIEU)==2,"Urban",NA_character_))),all(x$link_rurb%in%c("Rural","Urban")))
  f<-readRDS(facility_input);f<-f[f$country=="senegal",]
  r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("fid","s5_6","s1_3",paste0("s5_7",letters[1:7]))))
  ri<-match(f$fid,as.character(r$fid));stopifnot(!anyNA(ri),!anyDuplicated(ri),nrow(f)==2043)
  f$geo<-key(f$assigned_join_area);f$residence<-as.character(f$rurb)
  lut<-readxl::read_xlsx(residence_lookup,range="A1:B79")
  stopifnot(!anyDuplicated(key(lut$s1_3)))
  li<-match(key(as.character(haven::as_factor(r$s1_3[ri]))),key(lut$s1_3))
  rv<-as.numeric(lut$urbanrural[li])
  raw_residence<-ifelse(rv%in%1,"Rural",ifelse(rv%in%2,"Urban",NA_character_))
  stopifnot(identical(tolower(raw_residence),tolower(f$residence)))
  f$context<-paste(f$geo,f$residence,sep="|");fp<-as.numeric(r$s5_6[ri])%in%1
  f<-cbind(f,senegal_frequency(r[ri,]));eligible<-f[fp,]
  stopifnot(nrow(eligible)==1941)
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
  assigned_profile<-ifelse(fallback,paste(d$geo,"Departement",sep="|"),wk)
  joined$link_has_supply<-!is.na(wi)|fallback;joined$link_fp_provider_only<-TRUE
  joined$link_has_exact_fp_supply<-!is.na(wi);joined$link_supply_fallback<-fallback
  joined$link_requested_supply_profile<-wk;joined$link_supply_profile<-assigned_profile
  joined$link_supply_resolution<-ifelse(fallback,"departement-wide fallback","Departement-residence")
  joined$link_geo_mode<-"audited_fp_providers_residence_with_empty_cell_lga_fallback"
  joined$link_status<-ifelse(fallback,"departement-wide FP-provider fallback: zero exact-cell FP providers",
   ifelse(is.na(wi),"No eligible FP profile at either resolution","Matched exact Departement-residence FP profile"))
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
  observed$assignment_resolution<-ifelse(observed$fallback,"departement-wide fallback","Exact residence")
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
  stopifnot(nrow(observed)==49,sum(observed$complete_supply)==49,
   sum(fallback)==1815,sum(fallback&d$need)==522,all(d$has_fp_supply),
   length(unique(d$geo[fallback]))==16,all(d$residence[fallback]=="Rural"),
   all(is.finite(as.matrix(sp))),identical(x$IDW,joined$IDW),nrow(joined)==7169)
  for(k in intersect(names(x),names(joined))){
   if(k%in%replace||grepl("^link_",k))next
   stopifnot(isTRUE(all.equal(x[[k]],joined[[k]],check.attributes=FALSE,tolerance=0)))
  }
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  saveRDS(joined,file.path(out,"senegal_women_manitoba_rural_urban_fp.rds"),compress="gzip")
  saveRDS(profiles,file.path(out,"senegal_rural_urban_fp_profiles.rds"),compress="gzip")
  saveRDS(admin_profiles,file.path(out,"senegal_fp_fallback_profiles.rds"),compress="gzip")
  write.csv(observed,file.path(data_dir,"residence_supply_profiles_and_coverage.csv"),row.names=FALSE,na="")
  write.csv(profiles,file.path(out,"senegal_rural_urban_fp_profiles.csv"),row.names=FALSE,na="")
  frame$women<-d;frame$supply<-sp;frame$residence_profiles<-observed
  frame$assigned_profiles<-observed;frame$supply_resolution<-"urban_rural"
  saveRDS(frame,file.path(data_dir,"analysis_frame.rds"))
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"),simplifyVector=FALSE)
  p$audit$supply_resolution<-"Departement-Rural/Urban FP-provider profiles, with flagged departement-wide fallback only when the exact cell has zero FP providers; departement-wide planning summaries"
  p$audit$model_supply_aggregation<-"s5_6 Yes, valid-response equal-facility means within residence cells; zero-provider cells receive the same departement's FP-provider means. No zero filling or component fallback when eligible exact-cell providers exist."
  p$audit$observed_residence_cells<-49L;p$audit$complete_residence_profiles<-49L
  p$audit$exact_residence_profiles<-33L;p$audit$fallback_residence_profiles<-16L
  p$audit$unmatched_source_women<-0L;p$audit$unmatched_need_women<-0L
  p$audit$fallback_source_women<-1815L;p$audit$fallback_need_women<-522L;p$audit$fallback_departments<-16L;p$audit$assigned_context_profiles<-49L
  p$audit$fallback_area<-"Sixteen rural departement cells have zero eligible matched FP providers under the health-district residence proxy; use flagged departement-wide averages."
  p$audit$planning_resolution<-"All s5_6 Yes facilities in each LGA, combining Rural and Urban, unchanged from current aggregate report."
  p$audit$residence_limitation<-"Facility Rural/Urban is a health-district lookup proxy, not a site-level residence measurement. Nine facility records have unknown proxy residence. Zero rural matches do not establish no rural services."
  p$audit$exact_residence_source_md5<-as.list(source_hash)
  for(i in seq_along(p$assignment_summary)){
   g<-p$assignment_summary[[i]]$geo;ii<-d$geo==g
   p$assignment_summary[[i]]$fallback_women<-sum(ii&fallback)
   p$assignment_summary[[i]]$fallback_need<-sum(ii&fallback&d$need)
  }
  write.csv(do.call(rbind,lapply(p$assignment_summary,as.data.frame)),file.path(data_dir,"department_residence_assignment_summary.csv"),row.names=FALSE,na="")
  write.csv(observed,file.path(data_dir,"assigned_supply_profiles.csv"),row.names=FALSE,na="")
  saveRDS(data.frame(row=d$row,IDW=d$id,department=d$district,residence=d$residence,in_need=d$need,
   exact_profile_available=d$has_exact_fp_supply,department_fallback=fallback,supply_source_key=d$source_profile,
   assigned_facilities=joined$n_facilities),file.path(data_dir,"woman_assignment_audit.rds"))
  jsonlite::write_json(p,file.path(data_dir,"report_data.json"),auto_unbox=TRUE,pretty=FALSE,na="null",null="null",digits=12)
  jsonlite::write_json(p$audit,file.path(data_dir,"analysis_audit.json"),auto_unbox=TRUE,pretty=TRUE,na="null",null="null")
  stopifnot(identical(source_hash,unname(tools::md5sum(c(input,facility_input,raw_facility,residence_lookup)))))
  cat("Residence join: 7169 women retained; 49 complete assigned profiles (33 exact, 16 departement fallbacks); 522 need-group women use flagged fallbacks.\n")

}

# Update data checks -----------------------------------------------------
.report_steps[["update_data_checks.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Append reproducible, aggregate-only Senegal fallback tables to an existing report.
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop("Supply ",k);return(default)};if(i==length(args))stop("Supply ",k);args[i+1L]}
  base_html<-arg("--base-html");data_dir<-arg("--data-dir");output<-arg("--output")
  audit_json<-arg("--audit-json","")
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  coverage<-read.csv(file.path(data_dir,"residence_supply_profiles_and_coverage.csv"),stringsAsFactors=FALSE)
  stopifnot(nrow(d)==7169,sum(d$need)==1905,all(d$residence%in%c("Rural","Urban")),
   sum(d$fallback)==1815,sum(d$fallback&d$need)==522,
   !any(d$fallback&d$residence=="Urban"))
  for(i in seq_len(nrow(coverage))){
   ii<-paste(d$geo,d$residence,sep="|")==coverage$context[i]
   stopifnot(sum(ii)==coverage$women[i],sum(ii&d$need)==coverage$women_in_need[i],
    all(d$fallback[ii]==coverage$fallback[i]),coverage$fallback[i]==(coverage$fp_facilities[i]==0))
  }
  positive<-d$positive&is.finite(d$weight)&d$weight>0
  stopifnot(sum(positive)==7158,all(positive[d$need]))
  wm_share<-function(numerator,denominator){
   ii<-denominator&positive
   if(!any(ii))return(NA_real_)
   100*survey_weighted_mean(as.numeric(numerator[ii]),d$weight[ii])
  }
  stats<-function(pop,residence,geo=NULL){
   residence_group<-pop&d$residence==residence
   selected<-residence_group&d$fallback
   if(!is.null(geo))selected<-selected&d$geo==geo
   list(n=sum(selected),n_residence=sum(residence_group),n_all=sum(pop),
    sample_pct_residence=100*sum(selected)/sum(residence_group),
    sample_pct_all=100*sum(selected)/sum(pop),
    weighted_pct_residence=wm_share(selected,residence_group),
    weighted_pct_all=wm_share(selected,pop))
  }
  esc<-function(x){x<-gsub("&","&amp;",as.character(x),fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
  name<-function(g){
   a<-tools::toTitleCase(tolower(unique(d$district[d$geo==g])))
   special<-c(KEDOUGOU="Kédougou",LINGUERE="Linguère",MBACKE="Mbacké",SEDHIOU="Sédhiou",THIES="Thiès")
   if(g%in%names(special))unname(special[g])else a
  }
  num<-function(v)format(v,big.mark=",",scientific=FALSE,trim=TRUE)
  pct<-function(v)sprintf("%.2f%%",v)
  table_html<-function(id,headers,rows,total=NULL){
   row_html<-function(x,cls="")paste0('<tr',if(nzchar(cls))paste0(' class="',cls,'"')else'',"><td>",paste(esc(x),collapse="</td><td>"),"</td></tr>")
   paste0('<div class="table-wrap"><table class="supply-coverage" id="',id,'"><thead><tr><th>',
    paste(esc(headers),collapse="</th><th>"),"</th></tr></thead><tbody>",
    paste(vapply(rows,row_html,character(1)),collapse="\n"),
    if(!is.null(total))row_html(total,"total-row")else'',"</tbody></table></div>")
  }
  rural_geos<-sort(unique(d$geo[d$fallback&d$residence=="Rural"]))
  stopifnot(length(rural_geos)==16)
  populations<-list(all=rep(TRUE,nrow(d)),need=d$need)
  results<-lapply(populations,function(pop)list(rural_total=stats(pop,"Rural"),
   urban_total=stats(pop,"Urban"),rural_rows=setNames(lapply(rural_geos,function(g)stats(pop,"Rural",g)),rural_geos)))
  rows<-function(x)c(num(x$n),pct(x$sample_pct_residence),pct(x$sample_pct_all),pct(x$weighted_pct_residence),pct(x$weighted_pct_all))
  section<-paste0('\n<!-- BEGIN SENEGAL RESIDENCE CHECKS -->\n<section id="senegal-residence-fallbacks">',
   '<h2>Senegal: département–Rural/Urban supply assignment and fallback</h2>',
   '<p>Supply means use only facilities reporting <code>s5_6 = 1</code> (FP services), with valid answers separately for each measure. A woman receives her own département–Rural/Urban FP-provider profile. If that exact group has zero eligible FP facilities, she receives her own département-wide FP-provider averages with an explicit fallback flag. Her recorded residence is unchanged.</p>',
   '<div class="decision"><strong>Observed coverage</strong><p>Across 33 départements, the 49 observed residence groups have 33 exact assignments and 16 rural département-wide fallbacks. The fallback affects <strong>1,815 source women</strong>, including <strong>522 women in need</strong>. No observed Urban group requires fallback.</p></div>',
   '<p class="note">Facility Rural/Urban is a health-district lookup proxy, not a site-level geographic measurement. Nine facility records have unknown proxy residence. A zero rural match does not establish absence of rural services. The flagged fallback represents broader département evidence, not measured rural conditions.</p>')
  for(k in names(populations)){
   pop<-populations[[k]];r<-results[[k]];all_label<-if(k=="all")"all women"else"all women in need"
   heading<-if(k=="all")"All women: rural groups receiving département-wide averages"else"Women in need of FP: rural groups receiving département-wide averages"
   section<-paste0(section,'<h3 style="font-size:17px;color:#185664">',heading,"</h3>",
    '<p class="note">Population: ',num(sum(pop)),if(k=="all")' original women'else' women with original SURGO FPN2 = 1, consent and positive WEIGHT',
    '; ',num(r$rural_total$n_residence),' Rural and ',num(r$urban_total$n_residence),' Urban.</p>',
    table_html(paste0("senegal-rural-fallback-",k),
     c("Département","Women receiving fallback, n","Sample % of Rural women",paste0("Sample % of ",all_label),"Weighted % of Rural women",paste0("Weighted % of ",all_label)),
     lapply(rural_geos,function(g)c(name(g),rows(r$rural_rows[[g]]))),c("Total",rows(r$rural_total))))
   stopifnot(sum(vapply(r$rural_rows,function(a)a$n,numeric(1)))==r$rural_total$n,
    abs(sum(vapply(r$rural_rows,function(a)a$weighted_pct_all,numeric(1)))-r$rural_total$weighted_pct_all)<1e-10)
  }
  section<-paste0(section,'<h3 style="font-size:17px;color:#185664">Urban groups receiving département-wide averages</h3>',
   '<p>None. All 24 observed Urban département groups have at least one eligible FP provider, so their women receive the matching Urban profile. No Urban assignment is created for départements without Urban women in SURGO.</p>',
   table_html("senegal-urban-fallback-summary",c("Population","Urban women, n","Women receiving fallback, n","Sample % of Urban women","Sample % of population","Weighted % of Urban women","Weighted % of population"),
    lapply(names(populations),function(k){r<-results[[k]]$urban_total;c(if(k=="all")"All women"else"Women in need of FP",num(r$n_residence),rows(r))})),
   '<p class="note"><strong>How to read the percentages.</strong> Sample percentages divide unweighted respondent counts. Weighted percentages use original SURGO <code>WEIGHT</code> and the corresponding residence/population denominator. For all-women weighted estimates, use 7,158 consenting women with positive weights; 11 source records remain in raw counts but are outside weighted estimation, including one fallback record. All 1,905 women in need have valid positive weights. No facility weights are applied. Percentages use valid denominators, not fitted-model complete cases.</p>',
   '<p class="note">The source-group need denominator is 837 Rural women and 1,068 Urban women. These coverage tables concern the full need group, not only the 1,558 women with complete model variables. The LGA/département planning classifications and fitted models are unchanged by this reporting addition.</p>',
   '</section>\n<!-- END SENEGAL RESIDENCE CHECKS -->\n')
  base<-paste(readLines(base_html,warn=FALSE,encoding="UTF-8"),collapse="\n")
  base<-gsub('(?s)\n<!-- BEGIN SENEGAL RESIDENCE CHECKS -->.*?<!-- END SENEGAL RESIDENCE CHECKS -->\n?',"\n",base,perl=TRUE)
  stopifnot(length(gregexpr("<footer",base,fixed=TRUE)[[1]])==1,grepl("<footer",base,fixed=TRUE))
  updated<-sub("<footer",paste0(section,"\n<footer"),base,fixed=TRUE)
  dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
  writeLines(updated,output,useBytes=TRUE)
  if(nzchar(audit_json)){
   dir.create(dirname(audit_json),recursive=TRUE,showWarnings=FALSE)
   jsonlite::write_json(list(populations=results,source_rows=nrow(d),positive_weight_rows=sum(positive),
    need_n=sum(d$need),rural_fallback_groups=16,urban_fallback_groups=0,urban_observed_groups=24,
    policy="Department-wide fallback only for zero exact-cell FP providers; no input or analysis changes."),
    audit_json,auto_unbox=TRUE,pretty=TRUE,na="null",digits=12)
  }
  cat("Updated Data checks: two rural fallback tables, all/need Urban checks, sample and final-weight percentages.\n")

}

# Execute the named blocks above in independent local environments.
run_stage <- function(script,args=character(),python="python3") {
  key<-basename(script)
  if(grepl("\\.R$",key)) {
    if(!key %in% names(.report_steps))stop("Unknown analysis block: ",key)
    message("Running ",key)
    .report_steps[[key]](args)
    return(invisible(TRUE))
  }
  paths<-c("render_analysis.py"="support/renderers/rural_urban/senegal_render.py",
           "verify_html.py"="support/renderers/rural_urban/senegal_verify.py",
           "html_fragment.py"="support/renderers/html_fragment.py",
           "render_regression_sensitivity.py"="support/renderers/render_regression_sensitivity.py")
  if(!key %in% names(paths))stop("Unknown rendering dependency: ",key)
  run_python(.report_root,paths[[key]],args,python)
}

# End-to-end sequence -----------------------------------------------------------
build_senegal_rural_urban <- function(root,join_dir="",cache_dir="data/cache",
 work_dir="output/rural_urban_reports/senegal",python="python3",input_zip="") {
 root<-normalizePath(root,mustWork=TRUE)
use_project_library(root)
 work_dir<-project_path(work_dir,root);cache_dir<-project_path(cache_dir,root)
 if(nzchar(input_zip)){
  input_zip<-project_path(input_zip,root)
  if(nzchar(join_dir))join_dir<-project_path(join_dir,root)
  ctx<-prepare_linkage(root,input_zip,work_dir,cache_dir,join_dir)
  join_dir<-ctx$join_dir;cache_dir<-ctx$cache_dir
 }else{
  if(!nzchar(join_dir))stop("Supply input_zip or a verified urban/rural join_dir plus cache_dir")
  join_dir<-project_path(join_dir,root)
  for(n in c("run_parameters.json","admin_reference/run_parameters.json",
   "checked_geography/senegal_women_manitoba_urban_rural.rds"))
   if(!file.exists(file.path(join_dir,n)))stop("Incomplete verified urban/rural join")
  opts<-jsonlite::read_json(file.path(join_dir,"run_parameters.json"),simplifyVector=TRUE)
  if(opts$geography_version!="both")stop("Use the verified join containing both geography versions")
 }
 verify_report_assets(root)
 module<-dirname(.report_file)
 shared<-dirname(.report_file)
 data<-file.path(work_dir,"Senegal_rural_urban_updated_files")
 prep<-file.path(work_dir,"inputs");dir.create(data,recursive=TRUE,showWarnings=FALSE)
 run<-function(base,name,args)run_stage(file.path(base,name),args,python)
 admin<-file.path(join_dir,"admin_reference")
 fac<-file.path(admin,"audit/facility_linkage_inputs.rds")
 raw<-file.path(cache_dir,"inputs/surgo/senegal/surgo_senegal_women.dta")
 raw_fac<-file.path(cache_dir,"inputs/uom/senegal/uom_senegal_facility_pooled.dta")
 dict<-file.path(cache_dir,"exports/merged_supply_demand_dictionary.csv")
 run(shared,"prepare_fp_provider_data.R",c("--input",file.path(admin,"checked_geography/senegal_women_manitoba.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--raw-women",raw,"--dictionary",dict,"--output-dir",prep))
 run(shared,"build_analysis.R",c("--input",file.path(prep,"senegal_women_manitoba_fp_providers.rds"),
  "--facility-input",fac,"--raw",raw,"--raw-facility",raw_fac,"--department-lookup",
  file.path(cache_dir,"lookups/senegal_health_district_department_evidence.csv"),"--output-dir",data))
 run(shared,"prepare_map.R",c("--boundary",file.path(root,"support/assets/boundaries/senegal_departments.geojson"),"--data-dir",data))
 run(module,"assign_residence_supply.R",c("--input",file.path(join_dir,"checked_geography/senegal_women_manitoba_urban_rural.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--dictionary",dict,"--residence-lookup",file.path(cache_dir,"inputs/SenegalURlookup.xlsx"),"--data-dir",data,"--output-dir",prep))
 run(module,"fit_models.R",c("--data-dir",data,"--output-dir",file.path(data,"models_zscore")))
 pca<-file.path(data,"pca_appendix")
 run(module,"build_pca_scores.R",c("--data-dir",data,"--output-dir",pca))
 run(module,"fit_models.R",c("--data-dir",data,"--output-dir",file.path(pca,"models"),
  "--score-method","pca","--pca-score-file",file.path(pca,"pca_scores.rds")))
 run(module,"verify_analysis.R",c("--input",file.path(prep,"senegal_women_manitoba_rural_urban_fp.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--raw",raw,"--residence-lookup",file.path(cache_dir,"inputs/SenegalURlookup.xlsx"),
  "--cleaned-facility",file.path(cache_dir,"uom_pipeline/uom_summary_facility.csv"),"--data-dir",data))
 native<-file.path(work_dir,"Senegal_rural_urban_updated.html")
 run(module,"render_analysis.py",c("--data-dir",data,"--output",native))
  # Sensitivity analyses: alternative scores and final-composite SD maps.
  render_score_threshold_appendix(root,"senegal",data,native,python,"urban_rural")
 run(module,"verify_html.py",c("--data-dir",data,"--html",native))
 fragment<-file.path(work_dir,"report_fragment.html")
 run_stage(file.path(root,"analysis/common/html_fragment.py"),c("--input",native,"--output",fragment),python)
 list(native=native,fragment=fragment,data_dir=data)
}


if(sys.nframe()==0L) {
  if("--help" %in% commandArgs(TRUE)) {
    cat("Run: Rscript rural_urban/senegal/Senegal_rural_urban_updated.R --input-zip FILE\n",
        "Optional: --work-dir DIR --cache-dir DIR --join-dir VERIFIED_JOIN --output-dir DIR --python EXE --pandoc-dir DIR\n",sep="")
    quit(status=0)
  }
  o<-report_arguments("output/rural_urban/senegal","rural_urban/senegal")
  if(nzchar(o$pandoc_dir))Sys.setenv(RSTUDIO_PANDOC=o$pandoc_dir)
  r<-build_senegal_rural_urban(.report_root,o$join_dir,o$cache_dir,o$work_dir,o$python,o$input_zip)
  output<-project_path(o$output_dir,.report_root);dir.create(output,recursive=TRUE,showWarnings=FALSE)
  target<-rmarkdown::render(file.path(dirname(.report_file),"Senegal_rural_urban_updated.Rmd"),output_file="Senegal_rural_urban_updated.html",
    output_dir=output,params=list(prepared_fragment=r$fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
  run_stage("verify_html.py",c("--data-dir",r$data_dir,"--html",target),o$python)
  message("Completed Senegal_rural_urban_updated.html")
}
