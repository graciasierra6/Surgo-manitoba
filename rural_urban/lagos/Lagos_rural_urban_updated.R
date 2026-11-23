#!/usr/bin/env Rscript
# Lagos Rural/Urban report: complete country analysis.
# Run this one R file from the complete project; output HTML/Rmd is beside it.
# Setup: Rscript setup.R. External data: --input-zip or SURGO_MANITOBA_INPUT_ZIP.
# Exact source filenames: docs/DATASET_FILENAMES.md. No input data are in this repo.
# R packages: haven, dplyr, readr, readxl, jsonlite, digest, sf, survey,
# clubSandwich, ggplot2, svglite, tidyselect, rmarkdown, knitr, base64enc.
# SURGO uses final weights, ids=~1, no strata/FPC; separate area CR2 inference.
# Facility means use confirmed FP providers with valid responses, equally weighted.
# Sections: country definitions; facility preparation; descriptive scores; models;
# sensitivity analyses; independent checks; build and render the matching report.

# Original source weight: weight -> internal weight.
# Selected source files (ZIP-relative members; dictionaries/lookups also required):
#   inputs/surgo/lagos/surgo_lagos_women.csv
#   inputs/uom/lagos/uom_lagos_facility_pooled.dta
#   uom_pipeline/uom_summary_facility.csv
#   uom_pipeline/Facilities data with archetype assignments.csv
#   uom_pipeline/new_demand_vol_facility.csv
#   exports/uom_surgo_geo_crosswalk_reviewed.csv
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

# Country definition: lagos_definitions.R
.report_definitions[["lagos_definitions.R"]] <- function(target) {
  evalq({
    # Shared SURGO definitions for the Lagos FPN2 analysis and outcome models.
    # Input is an existing woman-level join; original columns and need status persist.
    lagos_derive_women <- function(x) {
      nv <- function(k) suppressWarnings(as.numeric(x[[k]]))
      key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
      flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
      any_concern <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1,
        ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
      d <- data.frame(row = seq_len(nrow(x)), geo = key(x$link_geo_join), weight = nv("weight"),
                      need = nv("fpn2") %in% 1, lga = as.character(x$link_geo_join))
      d$agency <- flag("hs7c", 1, 1:2)
      d$stigma <- any_concern(cbind(flag("pb7b", 1:2, 1:4), flag("pb7c", 1:2, 1:4)))
      # Primary health/side-effect belief: perceived health problems, PB2A only.
      d$health <- flag("pb2a", 1:2, 1:4)
      mp <- sapply(paste0("mp4_", letters[1:9]), nv)
      selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
      none <- mp[, 9] %in% 1
      coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & nv("mp4_99") %in% 0 & (selected | none) & !(selected & none)
      d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) > 0), NA_real_)
      d$confidence <- any_concern(cbind(flag("mc5a", 3:4, 1:4), flag("mc5b", 3:4, 1:4)))
      d$opposes <- flag("fp1", 3:4, 1:4)
      prompts <- sapply(paste0("cu5", letters[1:4]), nv)
      self_use <- nv("cu1") %in% c(1, 3)
      current <- ifelse(self_use | rowSums(prompts == 1, na.rm = TRUE) > 0, 1,
                        ifelse(nv("cu1") %in% 2 & rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
      d$user <- current %in% 1
      modern <- rowSums(sapply(paste0("cu4_", 1:8), function(k) nv(k) %in% 1)) > 0 |
        nv("cu5c") %in% 1 | nv("cu5d") %in% 1 | (nv("cu4_11") %in% 1 & nv("cu4_lam") %in% 1)
      traditional <- rowSums(sapply(paste0("cu4_", c(9, 10, 12:15)), function(k) nv(k) %in% 1)) > 0 | nv("cu5a") %in% 1 | nv("cu5b") %in% 1
      d$modern <- d$user & modern
      d$traditional <- d$user & !modern & traditional
      d$intender <- current %in% 0 & nv("itu1") %in% 1:2
      d$no_intent <- current %in% 0 & nv("itu1") %in% c(3, 4, 99)
      d$unresolved <- !(d$user | d$intender | d$no_intent)
      d$wanted_start <- d$user & nv("cub8") %in% 1
      d$other_start <- d$user & nv("cub8") %in% 2
      modern_reference <- tolower(as.character(x$cu12)) %in% c("male condom", "injectables", "daily pills", "iud", "implant", "female sterilization", "male sterilization", "emergency pill", "lactational amenorrhea")
      d$modern_other_start <- d$modern & modern_reference & nv("cub8") %in% 2
      modern_target <- rowSums(sapply(paste0("its2_", 1:8), function(k) nv(k) %in% 1)) > 0
      d$traditional_modern_switch <- d$traditional & nv("itc2") %in% 1:2 & modern_target
      # Modern-use models require a known method classification. Current users
      # without a confirmed modern or traditional classification remain unresolved.
      d$y_modern <- ifelse(current %in% 0, 0,
        ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
      d$residence <- as.character(x$link_rurb)
      d$age10 <- (nv("current_age") - 30) / 10
      d$age10_sq <- d$age10^2
      edu <- ifelse(nv("rb3") %in% c(1, 2, 7), "Primary or less",
        ifelse(nv("rb3") %in% 3:4, "Secondary", ifelse(nv("rb3") %in% 5:6, "Post-secondary", NA_character_)))
      d$education <- factor(edu, levels = c("Primary or less", "Secondary", "Post-secondary"))
      d$partnered <- ifelse(nv("r1") %in% 1:4, as.numeric(nv("r1") %in% 1:3), NA_real_)
      goal <- rep("Uncertain or unstated", nrow(x))
      goal[nv("sf2") %in% 4] <- "After marriage"
      goal[nv("sf2") %in% 3] <- "After 3 years"
      goal[nv("sf2") %in% 2] <- "In 1-3 years"
      goal[nv("sf1a") %in% 2 | nv("sf1b") %in% 2] <- "No more children"
      d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children", "After marriage", "Uncertain or unstated"))
      d$urban <- ifelse(d$residence %in% c("Urban", "Rural"), as.numeric(d$residence == "Urban"), NA_real_)
      d
    }

  },envir=target)
}

# Country definition: supply_definitions.R
.report_definitions[["supply_definitions.R"]] <- function(target) {
  evalq({
    # Lagos-specific definitions for the separate updated aggregate report.
    lagos_domains <- function() list(
     D=c("agency","stigma","health","fertility","confidence","opposes"),
     DL=c("Healthcare permission barriers","Anticipated social stigma","Health / side-effect beliefs",
          "Future-fertility concerns","Low method knowledge","Opposes family planning"),
     S=c("offering","free","training","daily_provision","larc_stock","stockout_burden"),
     SL=c("Short + long-acting offering","Free-product provision","Trained-provider share",
          "Daily provision of offered modern methods","At least one LARC in stock","Stockout burden"))
    lagos_favorable_supply <- function(x) {
     x<-as.data.frame(x);x$stockout_burden<-1-x$stockout_burden;x
    }
    lagos_frequency_fields <- function() paste0("q507a",c("a","b","d","e","f","g","h","i","j","o"))
    lagos_frequency <- function(raw) {
     q<-as.data.frame(lapply(raw[lagos_frequency_fields()],as.numeric))
     collapse<-function(fields)apply(as.matrix(q[fields]),1,function(v){
       offered<-v[v%in%1:4]
       if(length(offered))min(offered)else if(all(v%in%5))5 else NA_real_
     })
     m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah","q507ai")),
       male_condoms=q$q507ae,female_condoms=q$q507af,ecp=q$q507ao,
       iud=collapse(c("q507aa","q507ab")),implants=q$q507aj)
     offered<-rowSums(m>=1&m<=4,na.rm=TRUE);daily<-rowSums(m==1,na.rm=TRUE)
     data.frame(daily_provision=ifelse(offered>0,daily/offered,NA_real_),
       at_least_weekly_provision=ifelse(offered>0,rowSums(m>=1&m<=2,na.rm=TRUE)/offered,NA_real_),
       frequency_daily_categories=daily,frequency_offered_categories=offered,
       frequency_valid_categories=rowSums(m>=1&m<=5,na.rm=TRUE),
       frequency_unknown_categories=7L-rowSums(m>=1&m<=5,na.rm=TRUE),
       frequency_unknown_source_items=rowSums(vapply(q,function(v)!v%in%1:5,logical(nrow(raw)))))
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
  # Restrict supply summaries to confirmed FP providers, preserving original women.
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  input<-arg("--input");facility_input<-arg("--facility-input");raw_facility<-arg("--raw-facility")
  raw_women<-arg("--raw-women");dictionary_file<-arg("--dictionary");out<-arg("--output-dir")
  script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
  project_root <- .report_root
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

  # Data for the standalone Lagos analysis: supplied FPN2, LGA planning summaries.
  # All locations are runtime parameters. No source data or historical report changes.
  args <- commandArgs(trailingOnly = TRUE)
  opt <- function(k) { i <- match(k, args); if (is.na(i) || i == length(args)) stop("Supply ", k); args[i + 1L] }
  optional <- function(k) { i <- match(k, args); if (is.na(i)) return(NULL); if (i == length(args)) stop("Supply a value for ", k); args[i + 1L] }
  input <- opt("--input"); facility_input <- opt("--facility-input"); out <- opt("--output-dir"); raw_facility <- opt("--raw-facility")
  psu_var <- optional("--psu-var"); strata_var <- optional("--strata-var")
  facility_sample <- optional("--facility-sample"); if (is.null(facility_sample)) facility_sample <- "all"
  demand_sample <- optional("--demand-sample"); if (is.null(demand_sample)) demand_sample <- "available"
  reference_dir <- optional("--reference-dir"); if (is.null(reference_dir)) reference_dir <- ""
  stopifnot(facility_sample == "all", demand_sample == "available", reference_dir == "")
  if (!is.null(strata_var) && is.null(psu_var)) stop("--strata-var requires a confirmed --psu-var")
  for (pkg in c("survey", "jsonlite")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  hash_before <- tools::md5sum(c(input, facility_input))
  x <- readRDS(input); fac <- readRDS(facility_input); fac <- fac[fac$country %in% "lagos", ]
  nv <- function(k) as.numeric(x[[k]])
  key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
  wm <- function(v, w) { ok <- is.finite(v) & is.finite(w) & w > 0; if (any(ok)) survey_weighted_mean(v[ok], w[ok]) else NA_real_ }
  mean_valid <- function(v) { v <- as.numeric(v); if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_ }
  bool <- function(v) { a <- tolower(as.character(v)); ifelse(a %in% c("true", "1", "yes"), 1, ifelse(a %in% c("false", "0", "no"), 0, NA_real_)) }
  flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
  any_concern <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
  D <- c("agency", "stigma", "health", "fertility", "confidence", "opposes")
  DL <- c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs", "Future-fertility concerns", "Low method knowledge", "Opposes family planning")
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  script_dir <- dirname(gsub("~+~", " ", sub("^--file=", "", script_arg[1]), fixed=TRUE))
  source(file.path(script_dir, "..", "..", "..", "analysis", "lagos", "lagos_definitions.R"))
  source(file.path(script_dir,"supply_definitions.R"))
  domains<-lagos_domains();S<-domains$S;SL<-domains$SL
  stopifnot(nrow(x) == 4555, !anyDuplicated(x$uid), all(x$link_has_supply))
  x$link_rurb <- as.character(x$settlement)
  d <- lagos_derive_women(x)
  pb2a<-as.numeric(x$pb2a);d$health<-ifelse(pb2a%in%1:4,as.numeric(pb2a%in%1:2),NA_real_)
  d$id <- as.character(x$uid); d$district <- d$lga
  # LGA is used only as the documented geographic inference cluster, not a survey EA.
  d$positive <- is.finite(d$weight) & d$weight > 0
  d$psu <- d$geo; d$fallback <- FALSE; d$source_profile <- d$geo
  valid_weight <- is.finite(d$weight) & d$weight > 0
  stopifnot(all(valid_weight[d$need]), sum(d$need) == 2159)
  options(survey.lonely.psu = "adjust")
  # Supplied weights identify point estimates. Do not infer official PSUs or strata
  # from household-ID digits or probability values. Intervals are enabled only when
  # confirmed design fields are explicitly supplied at run time.
  design_confirmed <- !is.null(psu_var)
  if (design_confirmed) {
    stopifnot(psu_var %in% names(x))
    d$.psu <- as.character(x[[psu_var]])
    stopifnot(all(!is.na(d$.psu[valid_weight]) & nzchar(d$.psu[valid_weight])))
    strata_formula <- NULL
    if (!is.null(strata_var)) {
      stopifnot(strata_var %in% names(x))
      d$.stratum <- as.character(x[[strata_var]])
      stopifnot(all(!is.na(d$.stratum[valid_weight]) & nzchar(d$.stratum[valid_weight])))
      strata_formula <- ~.stratum
    }
    full <- survey::svydesign(ids = ~.psu, strata = strata_formula, weights = ~weight,
                              data = d[valid_weight, ], nest = TRUE)
  } else {
    full <- survey::svydesign(ids = ~1, weights = ~weight, data = d[valid_weight, ])
  }
  need_design <- subset(full, need)
  estimate <- function(des, field) {
    v <- des$variables[[field]]
    p <- survey_weighted_mean(as.numeric(v), des$variables$weight)
    ci <- c(NA_real_, NA_real_)
    if (design_confirmed && p > 0 && p < 1 && survey::degf(des) > 0) {
      des$variables$.event <- as.numeric(v)
      ci <- as.numeric(confint(survey::svyciprop(~.event, des, method = "logit", df = survey::degf(des))))
    }
    list(n = sum(v), denominator = length(v), estimate = p, lo = ci[1], hi = ci[2])
  }
  bar_defs <- data.frame(field = c("user", "wanted_start", "other_start", "modern", "modern_other_start", "traditional", "traditional_modern_switch", "intender", "no_intent", "unresolved"),
    label = c("Current contraceptive users", "Using the method wanted at start*", "Not using the method wanted at start*", "Current modern-method users", "Wanted another method at start*", "Traditional methods (subset of users)", "Wants to switch to a modern method", "Nonusers with intention", "Nonusers without stated intention", "Use / intention unresolved"),
    indent = c(0,1,1,0,1,0,1,0,0,0), y = c(0,1,2,3.8,4.8,6.6,7.6,9.4,11,12.6),
    color = c("#0a7d89", "#579b90", "#ba8743", "#307e60", "#ba8743", "#8c8981", "#b57a32", "#446da4", "#89639a", "#9caab0"), stringsAsFactors = FALSE)
  bars <- lapply(seq_len(nrow(bar_defs)), function(i) c(as.list(bar_defs[i, ]), estimate(need_design, bar_defs$field[i])))
  if (tail(bars, 1)[[1]]$n == 0) bars <- head(bars, -1)
  dn <- d[d$need, ]; areas <- sort(unique(dn$geo))
  rates <- lapply(areas, function(g) {
    a <- subset(need_design, geo == g)
    list(geo = g, n = nrow(a$variables), cpr = estimate(a, "user"), mcpr = estimate(a, "modern"),
         intender = estimate(a, "intender"), no_intent = estimate(a, "no_intent"))
  })
  fac$geo <- ifelse(!is.na(fac$geo_join_pipeline), key(fac$geo_join_pipeline), key(fac$source_geo_name))
  stopifnot(all(!is.na(fac$geo)), length(unique(fac$geo)) == 20)
  # The original inventory remains the coverage universe. Analysis uses Q506 Yes.
  inventory<-fac
  r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","q506",lagos_frequency_fields())))
  ri<-match(fac$fid,as.character(r$key));stopifnot(!anyNA(ri))
  frequency<-lagos_frequency(r[ri,]);fac<-cbind(fac,frequency)
  fp<-as.numeric(r$q506[ri]);fac<-fac[fp%in%1,]
  fac$offering<-bool(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
  fac$training<-as.numeric(fac$p_train);fac$larc_stock<-bool(fac$one_larc_stockin)
  fac$stockout_burden<-as.numeric(fac$p_any_stockout)
  stopifnot(nrow(fac)==1040,sum(fac$geo%in%areas)==882)
  savecsv0<-function(z,name)write.csv(z,file.path(out,name),row.names=FALSE,na="")
  savecsv0(fac[c("fid","geo",names(frequency))],"daily_provision_facility_audit.csv")
  # Validate the single naming alias used by the existing join.
  assigned_key <- key(fac$assigned_join_area)
  assigned_key[assigned_key == "IFAKOIJAIYE"] <- "IFAKOIJAYE"
  stopifnot(all(assigned_key == fac$geo))
  fac$complete_supply <- complete.cases(fac[S])
  fac$included <- if (facility_sample == "complete") fac$complete_supply else TRUE
  savecsv <- function(z, name, rows = FALSE) write.csv(z, file.path(out, name), row.names = rows, na = "")
  savecsv(data.frame(geo=areas,total_facilities=vapply(areas,function(g)sum(fac$geo==g),integer(1)),
   complete_facilities=vapply(areas,function(g)sum(fac$geo==g & fac$complete_supply),integer(1))),"facility_complete_case_coverage.csv")
  dn_complete <- complete.cases(dn[D])
  savecsv(do.call(rbind,lapply(c("LAGOS",areas),function(g){ii<-if(g=="LAGOS")rep(TRUE,nrow(dn))else dn$geo==g
   data.frame(geo=g,women_in_need=sum(ii),complete_demand=sum(ii & dn_complete),excluded=sum(ii & !dn_complete),
   complete_weight_share=survey_weighted_mean(as.numeric(dn_complete[ii]),dn$weight[ii]))})),"demand_complete_case_coverage.csv")
  savecsv(do.call(rbind,lapply(D,function(k)data.frame(variable=k,missing=sum(!is.finite(dn[[k]])),
   total=nrow(dn),missing_pct=100*mean(!is.finite(dn[[k]])),weighted_missing_pct=100*wm(as.numeric(!is.finite(dn[[k]])),dn$weight)))),"demand_missingness.csv")
  savecsv(do.call(rbind,lapply(S,function(k){ii<-fac$geo%in%areas;data.frame(variable=k,valid=sum(is.finite(fac[[k]][ii])),
   missing=sum(!is.finite(fac[[k]][ii])),total=sum(ii),missing_pct=100*mean(!is.finite(fac[[k]][ii])))})),"supply_missingness.csv")
  savecsv(fac[c("fid","geo","source_geo_name","assigned_join_area","geo_join_pipeline")],"facility_geography_audit.csv")
  cells <- list(); meta <- list()
  display_areas <- sort(unique(inventory$geo))
  for (g in c("LAGOS", display_areas)) {
    wi <- if (g == "LAGOS") rep(TRUE, nrow(dn)) else dn$geo %in% g
    fi <- (if (g == "LAGOS") fac$geo %in% areas else fac$geo %in% g) & fac$included
    meta[[g]] <- list(geo = g, n_need = sum(wi), facility_n = sum(fi), linked = g == "LAGOS" || g %in% areas)
    for (j in seq_along(D)) {
      k <- D[j]; ok <- wi & is.finite(dn[[k]]) & (demand_sample == "available" | dn_complete)
      cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Demand", variable = k, label = DL[j],
        value = if (any(ok)) wm(dn[[k]][ok], dn$weight[ok]) * 100 else NA_real_,
        n_valid = sum(ok), n_total = sum(wi), numerator = sum(dn[[k]][ok] == 1))
    }
    for (j in seq_along(S)) {
      k <- S[j]; ok <- fi & is.finite(fac[[k]])
      cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Supply", variable = k, label = SL[j],
        value = mean_valid(fac[[k]][fi]) * 100, n_valid = sum(ok), n_total = sum(fi), numerator = sum(fac[[k]][ok]))
    }
  }
  cells <- do.call(rbind, cells)
  savecsv <- function(z, name, rows = FALSE) write.csv(z, file.path(out, name), row.names = rows, na = "")
  savecsv(cells, "heatmap_values_and_denominators.csv")
  M<-list(); transforms<-list()
  for(side in c("Demand","Supply")){
   cols<-if(side=="Demand")D else S
   original<-sapply(cols,function(k){z<-cells[cells$side==side & cells$variable==k,];z$value[match(areas,z$geo)]})
   m<-if(side=="Supply")as.matrix(100*(1-lagos_favorable_supply(as.data.frame(original/100))))else original
   rownames(m)<-areas;reference<-complete.cases(m)
   means<-colMeans(m[reference,,drop=FALSE]);sds<-apply(m[reference,,drop=FALSE],2,sd)
   meds<-apply(m[reference,,drop=FALSE],2,median);iqrs<-apply(m[reference,,drop=FALSE],2,IQR)
   ranks<-matrix(NA_real_,nrow(m),ncol(m),dimnames=dimnames(m))
   ranks[reference,]<-apply(m[reference,,drop=FALSE],2,rank,ties.method="average")
   stopifnot(all(sds>0),all(iqrs>0))
   M[[side]]<-list(raw=m,rank=ranks,
    robust=sweep(sweep(m,2,meds,"-"),2,iqrs,"/"),zscore=sweep(sweep(m,2,means,"-"),2,sds,"/"))
   for(k in cols)transforms[[length(transforms)+1L]]<-data.frame(geo=areas,side=side,variable=k,original_pct=original[,k],aligned_pct=m[,k],higher_raw_is_worse=side=="Supply"&&k=="stockout_burden",reference_included=reference,reference_n=sum(reference),
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
  scores$median_count<-data.frame(method="median_count",geo=areas,demand_score=dc,supply_score=sc,demand_flag=dc>=4,supply_flag=sc>=4,
   demand_priority_rank=rank(-dc,ties.method="average",na.last="keep"),supply_priority_rank=rank(-sc,ties.method="average",na.last="keep"),category=classify(dc>=4,sc>=4))
  thresholds$median_count<-data.frame(method="median_count",demand_cutoff=4,supply_cutoff=4,demand_rule="4 or more of six above component medians",
   supply_rule="4 or more of six above constraint-component medians",demand_reference_n=sum(is.finite(dc)),supply_reference_n=sum(is.finite(sc)))
  scores<-do.call(rbind,scores);thresholds<-do.call(rbind,thresholds);trace<-do.call(rbind,transforms)
  exact <- cells[cells$geo %in% areas, c("geo", "side", "variable", "value")]
  exact$original_pct_exact <- sprintf("%.17g", exact$value)
  savecsv(exact[c("geo", "side", "variable", "original_pct_exact")], "scoring_inputs_exact.csv")
  savecsv(scores, "all_method_scores_and_classifications.csv"); savecsv(thresholds, "classification_thresholds.csv")
  savecsv(trace, "component_score_trace.csv")
  planning <- lapply(areas, function(g) {
    q <- scores[scores$method == "zscore" & scores$geo == g, ]
    dt <- trace[trace$geo == g & trace$side == "Demand", ]; st <- trace[trace$geo == g & trace$side == "Supply", ]
    dt <- dt[order(-dt$zscore, match(dt$variable, D)), ]; st <- st[order(-st$zscore, match(st$variable, S)), ]
    list(geo = g, category = q$category, demand_flag = q$demand_flag, supply_flag = q$supply_flag,
         demand_score = q$demand_score, supply_score = q$supply_score, n = meta[[g]]$n_need,
         top_demand = head(dt$variable, 3), top_supply = head(st$variable, 3))
  })
  coverage <- lapply(display_areas, function(g) list(geo = g, women = sum(d$geo %in% g), facilities = sum(inventory$geo %in% g), fp_facilities=sum(fac$geo %in% g)))
  bar_table <- do.call(rbind, lapply(bars, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  savecsv(bar_table, "use_distribution_fpn2.csv")
  lga_rates <- do.call(rbind, lapply(rates, function(r) data.frame(geo = r$geo, n_need = r$n,
    cpr = r$cpr$estimate, mcpr = r$mcpr$estimate, intender = r$intender$estimate, no_intent = r$no_intent$estimate)))
  savecsv(lga_rates, "lga_use_rates_fpn2.csv")
  audit <- list(supply_score_orientation="constraints",country="Lagos",source_rows=nrow(x),positive_weight_rows=sum(valid_weight),need_n=nrow(dn),
   need_eligibility="Original SURGO fpn2=1, unchanged; no additional need reconstruction.",women_weight="weight",
   weighted_need_share=wm(as.numeric(d$need[valid_weight]),d$weight[valid_weight]),
   linked_lgas=length(areas),supply_lgas=length(display_areas),all_facility_records=nrow(inventory),
   linked_lga_facility_records=sum(inventory$geo%in%areas),fp_facility_records=nrow(fac),
   linked_fp_facilities=sum(fac$geo%in%areas),
   supply_aggregation="Q506 Yes only; valid-response equal-facility means within LGA, combining Rural and Urban; no facility weights. Genuine zeros included, undefined excluded per measure.",
   free_exception="No new zero recoding. The prior Ibeju-Lekki Urban exception was a Q506 No facility and is excluded under the FP-provider restriction.",
   health_definition="PB2A only: 1/2 concern; 3/4 no concern; otherwise unresolved.",
   stockout_definition="Original cleaned p_any_stockout unchanged. Heatmap shows burden; every score and PCA uses continuity=1-burden.",
   chart_denominator="All 2159 women with original fpn2=1; final weight percentages, unweighted n. Overlapping subsets.",
   use_definition="CU1/CU5 confirmed current use; CU4 and prompts classify modern/traditional; unresolved remains distinct.",
   design_fields_confirmed=design_confirmed,
   survey_variance=if(design_confirmed)"Confirmed runtime PSU/strata Taylor intervals"else"Final weight point estimates; official EA/PSU and strata unconfirmed, so overview intervals are withheld. No clusters inferred from household IDs or sampling probabilities.",
   four_category_labels="Relative above-average concerns and below-average readiness; not causal binding constraints or an absolute standard.",
   scoring_reference="17 linked LGAs equally weighted; each component centered on LGA mean and divided by sample SD. Mean six favorable supply z-scores; reverse stockout to continuity first. Demand>0 and readiness<0; 1e-10 ties unflagged.",
   main_method="zscore",sources=list(joined_file=basename(input),facility_file=basename(facility_input)),
   source_md5=as.list(unname(hash_before)),source_files_unchanged=identical(unname(hash_before),unname(tools::md5sum(c(input,facility_input)))))
  stopifnot(audit$source_files_unchanged)
  profiles <- data.frame(geo=areas,district=areas)
  for(k in S)profiles[[k]] <- vapply(areas,function(g)mean_valid(fac[[k]][fac$geo==g & fac$included]),numeric(1))
  profiles$n_facilities <- vapply(areas,function(g)sum(fac$geo==g & fac$included),integer(1))
  stopifnot(all(profiles$n_facilities > 0))
  sp <- profiles[match(d$geo,profiles$geo),S,drop=FALSE];rownames(sp)<-NULL
  # Reconcile the original administrative join, allowing only the documented
  # precision of the newly reaggregated FP-provider profiles.
  map <- c(offering="share_offer_larc_and_3_sa",free="mean_p_free_prod",training="mean_p_train",daily_provision="mean_daily_modern_provision",larc_stock="share_stocked_1plus_larc",stockout_burden="mean_facility_any_stockout")
  reconcile <- do.call(rbind,lapply(names(map),function(k){a<-as.numeric(x[[map[k]]]);b<-sp[[k]]
   data.frame(variable=k,max_abs_difference=max(abs(a-b),na.rm=TRUE),exception=FALSE,
   passed=if(facility_sample=="all") {ii<-rep(TRUE,nrow(d)); max(abs(a[ii]-b[ii]),na.rm=TRUE)<5.0001e-6} else TRUE)}))
  stopifnot(all(reconcile$passed));savecsv(reconcile,"join_supply_reconciliation.csv")
  audit$country <- "Lagos"; audit$facility_sample <- facility_sample; audit$demand_sample <- demand_sample
   audit$facility_records_used <- sum(fac$included & fac$geo%in%areas)
   audit$complete_facilities <- sum(fac$complete_supply & fac$geo%in%areas)
   audit$complete_demand_women <- sum(dn_complete)
   audit$supply_resolution <- "LGA only: each woman receives all included facilities averaged within her LGA"
   audit$counseling_included <- FALSE
   audit$positive_weight_rows <- sum(d$positive)
  ctx <- unique(cbind(d[c("geo","district","residence","source_profile","fallback")],sp))
  savecsv(profiles,"lga_supply_profiles.csv");savecsv(ctx,"assigned_supply_profiles.csv")
  saveRDS(list(women=d,supply=sp,profiles=profiles,assigned_profiles=ctx,facility_sample=facility_sample),file.path(out,"analysis_frame.rds"))
  payload <- list(audit = audit, demand_variables = D, demand_labels = DL, supply_variables = S, supply_labels = SL,
    bars = bars, rates = rates, heatmap = cells, area_metadata = unname(meta), scores = scores, thresholds = thresholds,
    component_trace = trace, planning = planning, coverage = coverage, profiles=profiles, assigned_profiles=ctx)
  jsonlite::write_json(payload, file.path(out, "report_data.json"), auto_unbox = TRUE, dataframe = "rows", pretty = FALSE, na = "null", digits = 12)
  jsonlite::write_json(audit, file.path(out, "analysis_audit.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  cat("Built data:", nrow(dn), "women in need;", length(areas), "linked LGAs;", nrow(fac), "facility records.\n")
  print(bar_table[, c("field", "n", "estimate")], row.names = FALSE)
  print(thresholds, row.names = FALSE)
  print(table(scores$method, scores$category))

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

  # Exact LGA-residence Lagos models; personal demand and updated FP-provider supply.
  args <- commandArgs(trailingOnly = TRUE)
  opt <- function(k) { i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L] }
  optional <- function(k,default) {i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-opt("--data-dir");out<-opt("--output-dir");score_method<-optional("--score-method","zscore")
  pca_file<-optional("--pca-score-file","");stopifnot(score_method%in%c("zscore","pca"))
  for(pkg in c("survey","clubSandwich","jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
  script_dir<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
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
   data.frame(variable=k,reference_mean_pct=unique(q$indicator_mean),reference_SD_pct=unique(q$indicator_SD),reference_LGAs=length(unique(q$geo)))}))
  write.csv(sref,file.path(out,"supply_score_reference.csv"),row.names=FALSE)

  if(score_method=="pca"){
   pp<-readRDS(pca_file);stopifnot(identical(pp$scores$row,d$row),pp$audit$report_md5==report_hash)
   d$demand_score<-pp$scores$demand_score;d$supply_risk<-pp$scores$supply_risk
  }
  resolution<-"urban_rural_fallback";demand_resolution<-"individual"
  z<-p$scores[p$scores$method=="zscore",]
  profiles<-unique(cbind(d[c("geo","district","source_profile","fallback")],frame$supply,supply_risk=d$supply_risk))
  stopifnot(nrow(profiles)==18,!anyDuplicated(profiles$source_profile))
  stopifnot(all(vapply(split(d$supply_risk,d$source_profile),function(v)length(unique(v))==1,logical(1))))
  write.csv(profiles,file.path(out,"model_supply_profiles.csv"),row.names=FALSE,na="")
  controls<-c("age10","age10_sq","education","partnered","fertility_goal","urban")
  specs<-list(score_demand=c(controls,"demand_score"),supply=c(controls,"supply_risk"),
   score_joint=c(controls,"demand_score","supply_risk"),components_demand=c(controls,D),components_joint=c(controls,D,"supply_risk"))
  positive<-d$positive
  common_vars<-unique(c("y_modern",controls,D,"demand_score","supply_risk"))
  d$eligible<-d$need & positive & complete.cases(d[common_vars]);cc<-droplevels(d[d$eligible,])
  stopifnot(all(cc$y_modern%in%0:1),all(cc$need),length(unique(cc$geo))==17,
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
      leaveout[[length(leaveout) + 1L]] <- data.frame(model = id, omitted_LGA = g, term = fields,
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
  savecsv(leaveout, "leave_one_LGA_out.csv")
  missing <- data.frame(variable = common_vars,
    missing_in_need = vapply(d[d$need, common_vars], function(a) sum(is.na(a)), integer(1)))
  savecsv(missing, "model_missingness.csv")
  savecsv(z[, c("geo", "demand_score", "supply_score")], "model_LGA_scores.csv")
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
  labels<-render_modern_use_plots(coefficients,curves,gaps,cc,p$demand_labels,D,out,score_method,demand_resolution,resolution,"lagos",
   supply_pca_mixed=score_method=="pca" && length(pp$audit$negative_supply_PC1_loadings)>0)
  influence<-do.call(rbind,lapply(split(leaveout,interaction(leaveout$model,leaveout$term,drop=TRUE)),function(a)
   data.frame(model=a$model[1],term=a$term[1],full_estimate=a$full_estimate[1],deletion_min=min(a$estimate),deletion_max=max(a$estimate),deletion_sign_changes=sum(sign(a$estimate)!=sign(a$full_estimate)))))
  savecsv(influence,"influence_summary.csv")
  saveRDS(fits,file.path(out,"fitted_models.rds"))
  audit<-list(regression_function="survey::svyglm",design_ids="~1",design_strata="None",country="Lagos",outcome="Current modern contraceptive use only: confirmed modern=1; traditional-only or confirmed nonuse=0; unresolved status remains missing.",
   need_definition="Original SURGO FPN2=1, unchanged. No reconstructed need flag or permanent-protection additions.",
   need_n=sum(d$need),common_n=nrow(cc),common_events=sum(cc$y_modern),score_method=score_method,
   supply_components=length(S),counseling_included="counselor"%in%S,
   demand_resolution="individual",supply_resolution="LGA-Rural/Urban FP-provider summaries, with flagged LGA-wide fallback only when the exact cell has zero FP providers",districts=length(unique(cc$geo)),
   survey_psus=length(unique(cc$psu)),observed_residence_profiles=18L,complete_residence_profiles=18L,exact_residence_profiles=17L,fallback_residence_profiles=1L,demand_distinct_values=length(unique(cc$demand_score)),supply_distinct_values=length(unique(cc$supply_risk)),
   assigned_context_profiles=length(unique(cc$source_profile)),woman_residence_cells=nrow(unique(cc[c("geo","residence")])),
   fallback_model_women=sum(cc$fallback),fallback_need_women=sum(d$fallback&d$need),
   women_residence_control="settlement, retained regardless of facility source level",
   controls=controls,omitted_controls="None of the agreed adjustment domains omitted; all model designs checked for full rank.",
   weights="Original Lagos weight; common mean-one fitting rescaling. No facility weights.",
   inference="Weighted logistic coefficients with LGA-clustered CR2 covariance, coefficient-specific Satterthwaite intervals, HTZ block tests and Holm adjustment within each joint model. No random effects or interactions.",
   design="survey::svyglm with ids=~1, no strata or FPC, original SURGO weights; administrative-area CR2 covariance and Satterthwaite tests. Original sampling PSUs are not specified in this model.",
   score_definition=if(score_method=="zscore")"Demand: mean of six personal binary indicators standardized with item-specific weighted mean/population SD in the need group. Supply: negative mean of selected assigned LGA-residence favorable measures (stock continuity=1-burden) standardized against the fixed 17-LGA component references. Scores enter per one original average-z point." else "Separate weighted demand and supply PCAs; personal demand PC1 and negative assigned LGA-residence supply PC1. Mixed supply signs make it a pattern score, not a uniformly ordered risk index. Scores enter per one original PC1 unit.",
   supply_missing="Ibeju-Lekki Urban has zero Q506 Yes facilities; 157 source women, 62 in need, receive flagged LGA-wide FP-provider means. All 18 assigned profiles are complete. No zero recoding or filling of undefined components in cells with eligible FP providers.",
   excluded_need=sum(d$need)-nrow(cc),eligible_weight_share=survey_weighted_mean(as.numeric(d$eligible[d$need]),d$weight[d$need]),
   excluded_LGAs=setdiff(unique(d$geo[d$need]),unique(cc$geo)),
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
  # Separate weighted individual-demand and LGA-residence-supply PCA sensitivity.
  args<-commandArgs(trailingOnly=TRUE)
  opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-opt("--data-dir");out<-opt("--output-dir")
  for(pkg in c("jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  report_file<-file.path(data_dir,"report_data.json");report<-jsonlite::fromJSON(report_file)
  report_hash<-unname(tools::md5sum(report_file));frame<-readRDS(file.path(data_dir,"analysis_frame.rds"))
  d<-frame$women;D<-report$demand_variables;S<-report$supply_variables
  eligible<-d$need;dm<-as.matrix(d[D]);sm0<-frame$supply[S];sm0$stockout_burden<-1-sm0$stockout_burden;sm<-as.matrix(sm0)
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
    stability[[length(stability) + 1L]] <- data.frame(side = side, omitted_LGA = g,
      loading_cosine = sum(f$rotation[, 1] * fit$rotation[, 1]),
      score_rank_correlation = cor(full_score, alt_score, method = "spearman"),
      pc1_variance_pct = 100 * f$eigenvalues[1] / sum(f$eigenvalues))
  }
  stability <- do.call(rbind, stability); savecsv(stability, "pca_leave_one_LGA_out.csv")
  pc1 <- loads[loads$PC == 1, ]
  audit<-list(method="Separate final-weighted correlation PCAs on individual demand and assigned LGA-residence supply; PC1 prespecified as sensitivity.",
   need_n=sum(eligible),demand_PCA_n=sum(di),supply_PCA_women_n=sum(si),supply_profiles=nrow(profiles),
   demand_PC1_variance=variance$variance_pct[variance$side=="Demand" & variance$PC==1],
   supply_PC1_variance=variance$variance_pct[variance$side=="Supply" & variance$PC==1],
   negative_demand_PC1_loadings=pc1$variable[pc1$side=="Demand" & pc1$loading<0],
   negative_supply_PC1_loadings=pc1$variable[pc1$side=="Supply" & pc1$loading<0],
   supply_collapse_equivalence_error=collapse_error,report_md5=report_hash,
   weighting="SURGO weight on both sides. Supply PCA is equivalent to assigned LGA-residence profiles weighted by summed weights of represented women; these are not facility weights.",
   missingness="Require all six demand and all selected supply indicators. No new zero recoding or partial scores. A flagged LGA-wide fallback is used only for a residence cell with zero FP providers. Only Q506 Yes facilities contribute; no undefined facility value is zero-filled.",
   alignment="Stockout burden becomes continuity=1-burden before PCA; all supply inputs favorable.",
   sign="Demand PC1 oriented toward average concern; supply PC1 toward average favorable provision and negated for modeling. Mixed signs remain and are explicitly reported.",
   uncertainty="Permutation and LGA-deletion checks assess exploratory construction stability, not validated survey-design inference. Regression intervals condition on estimated scores.",
   source_unchanged=report_hash==unname(tools::md5sum(report_file)))
  obj<-list(audit=audit,scores=scores,demand_fit=fd,supply_fit=fs,profiles=profiles,loadings=loads,variance=variance,moments=moments,stability=stability)
  saveRDS(obj,file.path(out,"pca_scores.rds"))
  jsonlite::write_json(obj[setdiff(names(obj),c("scores","demand_fit","supply_fit"))],file.path(out,"pca_results.json"),dataframe="rows",auto_unbox=TRUE,na="null",digits=12)
  jsonlite::write_json(audit,file.path(out,"pca_audit.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
  labels <- setNames(c(report$demand_labels, report$supply_labels), c(D, S))
  labels["stockout_burden"] <- "Stock continuity (1 - burden)"
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
    ggplot2::labs(title = "What the PCA scores emphasize", subtitle = "Signed coefficients on standardized inputs; demand uses individual responses, supply uses matched LGA-residence exposures",
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

# Verify analysis -----------------------------------------------------
.report_steps[["verify_analysis.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Independent checks of raw inputs, facility means, assignments, scores and models.
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-arg("--data-dir");input<-arg("--input");facility_input<-arg("--facility-input")
  raw_facility<-arg("--raw-facility");raw_women<-arg("--raw-women");cleaned<-arg("--cleaned-facility")
  out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));x<-readRDS(input)
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women;D<-p$demand_variables;S<-p$supply_variables
  checks<-list();verify<-function(name,ok){
   if(!isTRUE(ok))stop("Verification failed: ",name)
   checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE)
  }
  same<-function(a,b,tol=1e-10)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=tol))
  key<-function(v){v<-toupper(gsub("[^A-Za-z0-9]","",as.character(v)));v[v%in%"IFAKOIJAIYE"]<-"IFAKOIJAYE";v}
  valid_mean<-function(v){v<-as.numeric(v);if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_}
  bool<-function(v)ifelse(tolower(v)%in%c("1","true","yes"),1,ifelse(tolower(v)%in%c("0","false","no"),0,NA_real_))
  raw<-readr::read_csv(raw_women,show_col_types=FALSE,guess_max=20000,name_repair="minimal",na=c("","NA"))
  verify("Original 4555 women and IDs",nrow(x)==4555&&identical(x$uid,raw$uid))
  for(k in names(raw))verify(paste("Original SURGO field",k),same(x[[k]],raw[[k]],0))
  verify("Original SURGO need status",identical(d$need,as.numeric(raw$fpn2)%in%1)&&sum(d$need)==2159)
  verify("Original final woman weights",same(d$weight,raw$weight,0))
  verify("Every bar uses all need women",all(p$bars$denominator==2159))
  verify("Raw woman LGA is the join LGA",identical(key(raw$lga),d$geo)&&identical(d$geo,key(x$link_geo_join)))
  agency<-ifelse(raw$hs7c%in%1:2,as.numeric(raw$hs7c%in%1),NA_real_)
  health<-ifelse(raw$pb2a%in%1:4,as.numeric(raw$pb2a%in%1:2),NA_real_)
  verify("Health uses PB2A only",same(d$health,health))
  verify("Agency uses general healthcare permission",same(d$agency,agency))
  for(k in c("stigma","confidence")){
   cols<-if(k=="stigma")c("pb7b","pb7c")else c("mc5a","mc5b")
   mat<-as.matrix(raw[cols]);positive<-if(k=="stigma")mat%in%1:2 else mat%in%3:4
   dim(positive)<-dim(mat);known<-mat%in%1:4;dim(known)<-dim(mat)
   expected<-ifelse(rowSums(positive)>0,1,ifelse(rowSums(known)==2,0,NA_real_))
   verify(paste("Raw demand domain",k),same(expected,d[[k]]))
  }
  mp<-as.matrix(raw[paste0("mp4_",letters[1:9])]);sel<-rowSums(mp[,1:8,drop=FALSE]==1,na.rm=TRUE)>0
  none<-mp[,9]%in%1;ok<-apply(mp,1,function(v)all(v%in%0:1))&raw$mp4_99%in%0&(sel|none)&!(sel&none)
  fert<-ifelse(ok,as.numeric(rowSums(mp[,1:6,drop=FALSE]==1,na.rm=TRUE)>0),NA_real_)
  verify("Raw coherent fertility concerns",same(fert,d$fertility))
  verify("Raw opposition to FP",same(ifelse(raw$fp1%in%1:4,as.numeric(raw$fp1%in%3:4),NA_real_),d$opposes))
  f<-readRDS(facility_input);f<-f[f$country=="lagos",];r<-haven::read_dta(raw_facility)
  ri<-match(f$fid,as.character(r$key));verify("Facility raw IDs one-to-one",!anyNA(ri)&&!anyDuplicated(ri)&&length(ri)==1741)
  f$geo<-key(f$assigned_join_area)
  verify("Raw facility LGA assignments",identical(key(haven::as_factor(r$district_name[ri])),f$geo))
  fp<-as.numeric(r$q506[ri]);verify("FP eligibility counts",sum(fp%in%1)==1040&&sum(fp%in%2)==465&&sum(is.na(fp))==236)
  areas<-sort(unique(d$geo[d$need]));verify("882 linked FP providers",sum(fp%in%1&f$geo%in%areas)==882)
  exception<-f$geo=="IBEJULEKKI"&f$rurb%in%"Urban"
  verify("Previous zero exception excluded by FP eligibility",sum(exception)==1&&fp[exception]==2&&is.na(f$p_free_prod[exception]))
  # Independently collapse frequency variants; unknowns never become absent or zero.
  q<-lapply(r[ri,grep("^q507a",names(r),value=TRUE)],as.numeric)
  collapse<-function(cols)vapply(seq_len(nrow(f)),function(i){
   v<-vapply(cols,function(k)q[[k]][i],numeric(1));a<-v[v%in%1:4]
   if(length(a))min(a)else if(all(v%in%5))5 else NA_real_
  },numeric(1))
  m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah","q507ai")),male_condoms=q$q507ae,
   female_condoms=q$q507af,ecp=q$q507ao,iud=collapse(c("q507aa","q507ab")),implants=q$q507aj)
  den<-rowSums(m>=1&m<=4,na.rm=TRUE);num<-rowSums(m==1,na.rm=TRUE)
  daily<-ifelse(den>0,num/den,NA_real_)
  daily_audit<-read.csv(file.path(data_dir,"daily_provision_facility_audit.csv"))
  verify("Independent daily-frequency formula",same(daily_audit$daily_provision,daily[match(daily_audit$fid,f$fid)]))
  f$offering<-bool(f$one_larc_3_sa_offer);f$free<-as.numeric(f$p_free_prod);f$training<-as.numeric(f$p_train)
  f$larc_stock<-bool(f$one_larc_stockin);f$stockout_burden<-as.numeric(f$p_any_stockout);f$daily_provision<-daily
  cl<-readr::read_csv(cleaned,show_col_types=FALSE);ci<-match(f$fid,as.character(cl$fid))
  verify("Stockout source coverage",!anyNA(ci))
  verify("Original cleaned stockout values preserved",same(f$stockout_burden,as.numeric(cl$p_any_stockout[ci])))
  inventory<-f;f<-f[fp%in%1,]
  for(k in S)for(g in areas){
   v<-f[[k]][f$geo==g];h<-p$heatmap[p$heatmap$side=="Supply"&p$heatmap$variable==k&p$heatmap$geo==g,]
   verify(paste("FP-only facility mean",g,k),same(valid_mean(v)*100,h$value))
   verify(paste("FP-only denominator",g,k),h$n_total==sum(f$geo==g)&&h$n_valid==sum(is.finite(v)))
   verify(paste("Genuine zeros retained",g,k),same(h$numerator,sum(v[is.finite(v)])))
   for(a in unique(d$residence[d$geo==g])){
    ii<-d$geo==g&d$residence==a;use<-f$geo==g&f$rurb%in%a
    expected_fallback<-sum(use)==0
    if(expected_fallback)use<-f$geo==g
    value<-valid_mean(f[[k]][use])
    verify(paste("Residence or authorized LGA supply",g,a,k),same(frame$supply[[k]][ii],rep(value,sum(ii))))
    verify(paste("Fallback only for zero eligible providers",g,a,k),all(d$fallback[ii]==expected_fallback))
   }
  }
  for(k in D)for(g in areas){
   ii<-d$need&d$geo==g&is.finite(d[[k]]);h<-p$heatmap[p$heatmap$side=="Demand"&p$heatmap$variable==k&p$heatmap$geo==g,]
   verify(paste("Need-only weighted concern",g,k),same(h$value,100*weighted.mean(d[[k]][ii],d$weight[ii]))&&h$n_valid==sum(ii))
  }
  for(side in c("Demand","Supply")){
   fields<-if(side=="Demand")D else S
   mat<-sapply(fields,function(k){a<-p$heatmap[p$heatmap$side==side&p$heatmap$variable==k,];a$value[match(areas,a$geo)]})
   if(side=="Supply")mat[,"stockout_burden"]<-100-mat[,"stockout_burden"]
   zz<-scale(mat);expected<-rowMeans(zz);z<-p$scores[p$scores$method=="zscore",];z<-z[match(areas,z$geo),]
   observed<-if(side=="Demand")z$demand_score else -z$supply_score
   verify(paste("Independent mean z-score",side),same(expected,observed)&&abs(mean(observed))<1e-12)
   flag<-if(side=="Demand")expected>1e-10 else expected< -1e-10
   verify(paste("Zero-mean classification",side),identical(as.logical(flag),if(side=="Demand")z$demand_flag else z$supply_flag))
   for(k in fields){t<-p$component_trace[p$component_trace$side==side&p$component_trace$variable==k,]
    verify(paste("Aligned component z",side,k),same(if(side=="Supply")-t$zscore else t$zscore,zz[match(t$geo,areas),k]))}
  }
  cut<-p$thresholds[p$thresholds$method=="zscore",];verify("Primary cutoffs zero",cut$demand_cutoff==0&&cut$supply_cutoff==0)
  z<-p$scores[p$scores$method=="zscore",]
  verify("Classification categories",identical(z$category,ifelse(z$demand_flag&z$supply_flag,"both",ifelse(z$demand_flag,"demand",ifelse(z$supply_flag,"supply","neither")))))
  previous<-NULL
  for(folder in c("models_zscore","pca_appendix/models")){
   models<-readRDS(file.path(data_dir,folder,"fitted_models.rds"));co<-read.csv(file.path(data_dir,folder,"model_coefficients.csv"))
   tt<-read.csv(file.path(data_dir,folder,"model_block_tests.csv"))
   for(id in names(models)){
    ob<-models[[id]];aa<-ob$data;fo<-ob$formula
    verify(paste(folder,id,"common women across all ten fits"),is.null(previous)||identical(previous,aa$row));previous<-aa$row
    verify(paste(folder,id,"modern use and original need"),all(aa$need)&&all(aa$y_modern%in%0:1)&&same(aa$y_modern,d$y_modern[aa$row]))
    verify(paste(folder,id,"agreed covariates"),all(c("age10","age10_sq","education","partnered","fertility_goal","urban")%in%all.vars(fo)))
    verify(paste(folder,id,"no random effects or interactions"),!grepl("[|:*]",paste(deparse(fo),collapse=" ")))
    fit<-glm(fo,data=aa,weights=aa$weight/mean(aa$weight),family=quasibinomial(),control=glm.control(maxit=100))
    verify(paste(folder,id,"weighted coefficients"),same(coef(fit),coef(ob$fit)))
    V<-clubSandwich::vcovCR(fit,cluster=aa$geo,type="CR2",inverse_var=FALSE)
    verify(paste(folder,id,"LGA CR2 covariance"),same(as.vector(V),as.vector(ob$V)))
    for(block in tt$block[tt$model==id]){
     fields<-if(block=="Supply")"supply_risk"else if(grepl("^score",id))"demand_score"else D
     K<-diag(length(coef(fit)))[match(fields,names(coef(fit))),,drop=FALSE]
     t<-clubSandwich::Wald_test(fit,constraints=K,vcov=V,test="HTZ");saved<-tt[tt$model==id&tt$block==block,]
     verify(paste(folder,id,block,"HTZ block test"),abs(t$p_val-saved$p)<1e-10&&abs(t$df_denom-saved$df_denom)<1e-9)
    }
   }
  }
  models<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"));aa<-models$score_joint$data
  refs<-read.csv(file.path(data_dir,"models_zscore/individual_demand_score_reference.csv"))
  zz<-sapply(D,function(k){v<-d[[k]];ii<-d$need&is.finite(v);mu<-weighted.mean(v[ii],d$weight[ii]);sd<-sqrt(weighted.mean((v[ii]-mu)^2,d$weight[ii]));(v-mu)/sd})
  verify("Personal demand score from own six answers",same(aa$demand_score,rowMeans(zz)[aa$row]))
  sup<-sapply(S,function(k){ref<-p$component_trace[p$component_trace$side=="Supply"&p$component_trace$variable==k,]
   v<-frame$supply[[k]];if(k!="stockout_burden")v<-1-v
   (100*v-unique(ref$indicator_mean))/unique(ref$indicator_SD)})
  verify("Supply risk uses exact residence inputs and fixed LGA reference",same(aa$supply_risk,rowMeans(sup)[aa$row]))
  verify("18 assigned supply profiles in 17 modeled LGAs",length(unique(aa$supply_risk))==18&&length(unique(aa$geo))==17)
  verify("All 4555 women retained; 62 need women use fallback",nrow(x)==4555&&all(d$has_fp_supply)&&sum(d$fallback)==157&&sum(d$fallback&d$need)==62)
  verify("Fallback explicitly flagged without changing residence",all(d$fallback==(d$geo=="IBEJULEKKI"&d$residence=="Urban"))&&all(d$residence==as.character(raw$settlement)))
  expected_model<-d$need&d$positive&complete.cases(d[c("y_modern","age10","age10_sq","education","partnered","fertility_goal","urban",D)])
  verify("Fallback women included when other predictors complete",sum(aa$fallback)==sum(expected_model&d$fallback)&&identical(aa$row,d$row[expected_model]))
  verify("Badagry women all rural; Epe has both",all(d$residence[d$geo=="BADAGRY"]=="Rural")&&length(unique(d$residence[d$geo=="EPE"]))==2)
  verify("Fallback supply risk equals LGA-wide constraints on fixed scale",same(aa$supply_risk[aa$fallback],rep(z$supply_score[z$geo=="IBEJULEKKI"],sum(aa$fallback))))
  for(k in S)verify(paste("No supply missing after approved fallback",k),all(is.finite(frame$supply[[k]])))
  pca<-readRDS(file.path(data_dir,"pca_appendix/pca_scores.rds"))
  for(side in c("demand","supply")){
   fit<-pca[[paste0(side,"_fit")]];mat<-if(side=="demand")as.matrix(d[D])else as.matrix(frame$supply[S])
   if(side=="supply")mat[,"stockout_burden"]<-1-mat[,"stockout_burden"]
   ii<-d$need&complete.cases(mat);w<-d$weight[ii]/sum(d$weight[ii]);mu<-colSums(mat[ii,,drop=FALSE]*w)
   sd<-sqrt(colSums(sweep(mat[ii,,drop=FALSE],2,mu,"-")^2*w));Z<-sweep(sweep(mat[ii,,drop=FALSE],2,mu,"-"),2,sd,"/")
   sv<-svd(Z*sqrt(w));loading<-sv$v[,1];if(sum(loading*fit$rotation[,1])<0)loading<- -loading
   verify(paste(side,"PCA weighted center"),same(mu,fit$center))
   verify(paste(side,"PCA weighted scale"),same(sd,fit$scale))
   verify(paste(side,"PCA independent weighted SVD"),same(loading,fit$rotation[,1],1e-8))
  }
  save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
  jsonlite::write_json(list(all_passed=all(save$passed),check_count=nrow(save),source_rows=4555,need_n=2159,
   fp_facilities=1040,linked_fp_facilities=882,model_n=nrow(aa),models=10,
   fp_providers_only=TRUE,exact_residence_assignment=TRUE,fallback_need_women=62L,health_PB2A_only=TRUE,primary_cutoffs_zero=TRUE,all_models_same_women=TRUE,
   no_random_effects=TRUE,limitations="Implementation verification; not causal validation, full survey-design reconstruction or a formal power analysis."),
   file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
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
  dictionary_file<-arg("--dictionary");data_dir<-arg("--data-dir");out<-arg("--output-dir")
  script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
  project_root <- .report_root
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
  paths<-c("render_analysis.py"="support/renderers/rural_urban/lagos_render.py",
           "verify_html.py"="support/renderers/rural_urban/lagos_verify.py",
           "html_fragment.py"="support/renderers/html_fragment.py",
           "render_regression_sensitivity.py"="support/renderers/render_regression_sensitivity.py")
  if(!key %in% names(paths))stop("Unknown rendering dependency: ",key)
  run_python(.report_root,paths[[key]],args,python)
}

# End-to-end sequence -----------------------------------------------------------
build_lagos_rural_urban <- function(root,join_dir="",cache_dir="data/cache",
 work_dir="output/rural_urban_reports/lagos",python="python3",input_zip="") {
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
   "checked_geography/lagos_women_manitoba_urban_rural.rds"))
   if(!file.exists(file.path(join_dir,n)))stop("Incomplete verified urban/rural join")
  opts<-jsonlite::read_json(file.path(join_dir,"run_parameters.json"),simplifyVector=TRUE)
  if(opts$geography_version!="both")stop("Use the verified join containing both geography versions")
 }
 verify_report_assets(root)
 module<-dirname(.report_file)
 shared<-dirname(.report_file)
 data<-file.path(work_dir,"Lagos_rural_urban_updated_files")
 prep<-file.path(work_dir,"inputs");dir.create(data,recursive=TRUE,showWarnings=FALSE)
 run<-function(base,name,args)run_stage(file.path(base,name),args,python)
 admin<-file.path(join_dir,"admin_reference")
 fac<-file.path(admin,"audit/facility_linkage_inputs.rds")
 raw<-file.path(cache_dir,"inputs/surgo/lagos/surgo_lagos_women.csv")
 raw_fac<-file.path(cache_dir,"inputs/uom/lagos/uom_lagos_facility_pooled.dta")
 dict<-file.path(cache_dir,"exports/merged_supply_demand_dictionary.csv")
 run(shared,"prepare_fp_provider_data.R",c("--input",file.path(admin,"checked_geography/lagos_women_manitoba.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--raw-women",raw,"--dictionary",dict,"--output-dir",prep))
 run(shared,"build_analysis.R",c("--input",file.path(prep,"lagos_women_manitoba_fp_providers.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--output-dir",data))
 run(module,"assign_residence_supply.R",c("--input",file.path(join_dir,"checked_geography/lagos_women_manitoba_urban_rural.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--dictionary",dict,"--data-dir",data,"--output-dir",prep))
 run(module,"fit_models.R",c("--data-dir",data,"--output-dir",file.path(data,"models_zscore")))
 pca<-file.path(data,"pca_appendix")
 run(module,"build_pca_scores.R",c("--data-dir",data,"--output-dir",pca))
 run(module,"fit_models.R",c("--data-dir",data,"--output-dir",file.path(pca,"models"),
  "--score-method","pca","--pca-score-file",file.path(pca,"pca_scores.rds")))
 run(module,"verify_analysis.R",c("--input",file.path(prep,"lagos_women_manitoba_rural_urban_fp.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--raw-women",raw,
  "--cleaned-facility",file.path(cache_dir,"uom_pipeline/uom_summary_facility.csv"),"--data-dir",data))
 native<-file.path(work_dir,"Lagos_rural_urban_updated.html")
 run(module,"render_analysis.py",c("--data-dir",data,"--reference-html",file.path(root,"support/assets/lagos_map_template.html"),"--output",native))
  # Sensitivity analyses: alternative scores and final-composite SD maps.
  render_score_threshold_appendix(root,"lagos",data,native,python,"urban_rural")
 run(module,"verify_html.py",c("--data-dir",data,"--html",native))
 fragment<-file.path(work_dir,"report_fragment.html")
 run_stage(file.path(root,"analysis/common/html_fragment.py"),c("--input",native,"--output",fragment),python)
 list(native=native,fragment=fragment,data_dir=data)
}


if(sys.nframe()==0L) {
  if("--help" %in% commandArgs(TRUE)) {
    cat("Run: Rscript rural_urban/lagos/Lagos_rural_urban_updated.R --input-zip FILE\n",
        "Optional: --work-dir DIR --cache-dir DIR --join-dir VERIFIED_JOIN --output-dir DIR --python EXE --pandoc-dir DIR\n",sep="")
    quit(status=0)
  }
  o<-report_arguments("output/rural_urban/lagos","rural_urban/lagos")
  if(nzchar(o$pandoc_dir))Sys.setenv(RSTUDIO_PANDOC=o$pandoc_dir)
  r<-build_lagos_rural_urban(.report_root,o$join_dir,o$cache_dir,o$work_dir,o$python,o$input_zip)
  output<-project_path(o$output_dir,.report_root);dir.create(output,recursive=TRUE,showWarnings=FALSE)
  target<-rmarkdown::render(file.path(dirname(.report_file),"Lagos_rural_urban_updated.Rmd"),output_file="Lagos_rural_urban_updated.html",
    output_dir=output,params=list(prepared_fragment=r$fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
  run_stage("verify_html.py",c("--data-dir",r$data_dir,"--html",target),o$python)
  message("Completed Lagos_rural_urban_updated.html")
}
