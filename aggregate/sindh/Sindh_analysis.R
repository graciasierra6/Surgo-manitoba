#!/usr/bin/env Rscript
# Sindh | aggregate report
#
# Required R packages (restore once: Rscript setup.R):
#   haven, dplyr, readr, readxl, jsonlite, digest, sf, survey, clubSandwich, ggplot2, svglite,
#   tidyselect, rmarkdown, knitr, base64enc.
# Exact package versions: renv.lock. Project dependencies: DESCRIPTION.
# setup.R uses renv to restore the recorded project package environment.
# stats, utils, tools, grid, graphics and grDevices are bundled with R where used.
#
# Original country surveys:
#   inputs/surgo/sindh/20250124_forms_wt.dta
#   inputs/uom/sindh/uom_sindh_facility_pooled.dta
#
# Shared facility summaries, archetypes, volumes and geographic dictionary:
#   uom_pipeline/uom_summary_facility.csv
#   uom_pipeline/Facilities data with archetype assignments.csv
#   uom_pipeline/new_demand_vol_facility.csv
#   exports/uom_surgo_geo_crosswalk_reviewed.csv
#   exports/merged_supply_demand_dictionary.csv
#
# Country-specific lookup / need inputs:
#   lookups/sindh_district_aliases.csv
#   surgo_pipeline/sindh/sindh_wra_design_ready.rds
#
# Shared upstream reconciliation inputs (read during automatic join preparation):
#   merge_pipeline/uom_facility_geo.csv
#   merge_pipeline/uom_summary_facility_crosswalked.csv
#   exports/merged_supply_demand_department.csv
#   exports/merged_supply_demand_coverage.csv
#   curated/Facilities data with archetype assignments.csv
#   surgo_pipeline/lagos/wra_design_ready.rds
#   surgo_pipeline/senegal/wra_design_ready.rds
#   surgo_pipeline/cotedivoire/wra_design_ready.rds
#   surgo_pipeline/sindh/wra_design_ready.rds
#
# Bundle integrity / geographic provenance:
#   bundle_manifest.json
#   lookups/geographic_sources.json
#
# Supply the complete SURGO_Manitoba_linkage_inputs_complete.zip through --input-zip or
#   SURGO_MANITOBA_INPUT_ZIP. All 56 members must remain intact; common preparation also
#   validates/reads the other countries and reference files.
# No prejoined dataset must be chosen manually. This script prepares whole-area FP-provider
#   profiles.
# Original SURGO weight: Normalizedweights -> internal weight. Survey ids=~1; no strata/FPC;
#   separate administrative-area CR2 inference. Facility means are unweighted.
# Python 3 (standard library only) and Pandoc are also required for report rendering.
# Output: Sindh_analysis.html via the adjacent Sindh_analysis.Rmd. Keep the complete project,
#   including support/.
# Full filename inventory: docs/DATASET_FILENAMES.md; roles/checksums: docs/INPUTS.md.
# Input locations and credentials are runtime parameters, never saved personal paths.

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

# Country definition: sindh_definitions.R
.report_definitions[["sindh_definitions.R"]] <- function(target) {
  evalq({
    # Country-specific coding from the supplied Pakistan questionnaire and need file.
    sindh_definitions <- function(x, need_file) {
      nv <- function(k) { if (!k %in% names(x)) stop("Missing Sindh field: ", k); as.numeric(x[[k]]) }
      key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
      flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
      any_flag <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1,
        ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
      ref <- readRDS(need_file)
      stopifnot(!anyDuplicated(x$col_id), !anyDuplicated(ref$col_id), "fpn2_derived" %in% names(ref))
      ii <- match(x$col_id, ref$col_id)
      d <- data.frame(row = seq_len(nrow(x)), id = x$col_id, geo = key(x$link_geo_join),
        district = as.character(x$link_geo_join), weight = nv("Normalizedweights"),
        consent = nv("irb1") %in% 1, psu = as.character(x$clusterCode))
      d$positive <- d$consent & is.finite(d$weight) & d$weight > 0
      d$need_code <- ref$fpn2_derived[ii]
      d$need <- d$positive & d$need_code %in% 1
      # Verify the authorized upstream derivation independently; retain its SA1 policy.
      exclude <- nv("sf0b") %in% 1 | nv("sf0c") %in% 1 |
        nv("sf1a") %in% 3 | nv("sf1b") %in% 3 | nv("sf1c") %in% 3 |
        nv("sa0") %in% 2 | nv("sa1") %in% c(5, 6, 95, 97) |
        nv("pc0") %in% 1 | nv("sf2") %in% 1 | nv("pc18") %in% 1 | nv("pc19") %in% 2
      stopifnot(all(!is.na(ii[d$positive])), all(d$need_code[d$positive] == as.integer(!exclude[d$positive])),
        sum(d$need) == 3274, all(nv("fpn2") == 95),
        identical(as.numeric(x$rb0e), readBin(writeBin(as.double(x$clusterCode), raw(), size = 4),
          what = "double", n = nrow(x), size = 4)))
      d$agency <- flag("hs7c", 1, 1:2)
      d$stigma <- any_flag(cbind(flag("pb7b", 1:2, 1:4), flag("pb7c", 1:2, 1:4)))
      # Primary health/side-effect belief: perceived health problems, PB2A only.
      d$health <- flag("pb2a", 1:2, 1:4)
      mp <- sapply(paste0("mp4_", 1:9), nv)
      selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
      none <- mp[, 9] %in% 1
      coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & (selected | none) & !(selected & none)
      d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) > 0), NA_real_)
      d$confidence <- any_flag(cbind(flag("mc5a", 3:4, 1:4), flag("mc5b", 3:4, 1:4)))
      d$opposes <- flag("fp1", 3:4, 1:4)
      prompts <- sapply(paste0("cu5", letters[1:4]), nv)
      current <- ifelse(nv("cu1") %in% c(1, 3) | rowSums(prompts == 1, na.rm = TRUE) > 0, 1,
        ifelse(nv("cu1") %in% 2 & rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
      d$user <- current %in% 1
      modern <- rowSums(sapply(paste0("cu4_", 1:8), function(k) nv(k) %in% 1)) > 0 |
        nv("cu5c") %in% 1 | nv("cu5d") %in% 1 | (nv("cu4_11") %in% 1 & nv("cu4_lam") %in% 1)
      traditional <- rowSums(sapply(paste0("cu4_", c(9,10,12:15)), function(k) nv(k) %in% 1)) > 0 |
        nv("cu5a") %in% 1 | nv("cu5b") %in% 1
      d$modern <- d$user & modern
      d$traditional <- d$user & !modern & traditional
      d$y_modern <- ifelse(current %in% 0, 0, ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
      d$intender <- current %in% 0 & nv("itu1") %in% 1:2
      d$no_intent <- current %in% 0 & nv("itu1") %in% c(3, 4, 99)
      d$unresolved <- !(d$user | d$intender | d$no_intent)
      d$wanted_start <- d$user & nv("cub8") %in% 1
      d$other_start <- d$user & nv("cub8") %in% 2
      d$modern_other_start <- d$modern & nv("cub8") %in% 2
      future_modern <- rowSums(sapply(paste0("its2_", 1:8), function(k) nv(k) %in% 1)) > 0
      d$traditional_modern_switch <- d$traditional & nv("itc2") %in% 1:2 & future_modern
      age <- nv("rb2"); d$age10 <- ifelse(age >= 15 & age <= 49, (age - 30) / 10, NA_real_)
      d$age10_sq <- d$age10^2
      # Pakistan education categories differ from the Nigeria instrument.
      edu <- ifelse(nv("rb3") %in% 1:4, "Primary or less",
        ifelse(nv("rb3") %in% 5:7, "Middle / secondary", ifelse(nv("rb3") %in% 8:9, "Tertiary / professional", NA_character_)))
      d$education <- factor(edu, levels = c("Primary or less", "Middle / secondary", "Tertiary / professional"))
      goal <- rep("Uncertain or unstated", nrow(x))
      goal[nv("sf2") %in% 2] <- "In 1-3 years"; goal[nv("sf2") %in% 3] <- "After 3 years"
      goal[nv("sf1a") %in% 2 | nv("sf1b") %in% 2 | nv("sf1c") %in% 2] <- "No more children"
      d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children", "Uncertain or unstated"))
      d$partnered <- flag("r1", 1, 1:2)
      # Named cluster is present; do not invent urban/rural or sampling strata.
      stopifnot(!anyNA(d$psu[d$positive]), all(nzchar(d$psu[d$positive])),
        all(vapply(split(d$geo[d$positive], d$psu[d$positive]), function(v) length(unique(v)) == 1, logical(1))))
      d
    }
    sindh_domains <- function() list(
      D = c("agency", "stigma", "health", "fertility", "confidence", "opposes"),
      DL = c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs", "Future-fertility concerns", "Low method knowledge", "Opposes family planning"),
      S = c("offering", "free", "training", "counselor", "larc_stock", "popular_stock"),
      SL = c("Short + long-acting offering", "Free-product provision", "Trained-provider share", "FP counselor availability", "At least one LARC in stock", "Most-popular product in stock"))
    sindh_supply <- function(x) {
      map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
        counselor = "share_fp_counselor", larc_stock = "share_stocked_1plus_larc")
      a <- as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
      yes <- as.numeric(x$n_mp_in_stock_true); no <- as.numeric(x$n_mp_in_stock_false)
      a$popular_stock <- ifelse(yes + no > 0, yes / (yes + no), NA_real_)
      a
    }

  },envir=target)
}

# Country definition: supply_definitions.R
.report_definitions[["supply_definitions.R"]] <- function(target) {
  evalq({
    # Country-specific selected inputs for the updated district-only Sindh report.
    sindh_updated_domains <- function() list(
     D=c("agency","stigma","health","fertility","confidence","opposes"),
     DL=c("Healthcare permission barriers","Anticipated social stigma","Health / side-effect beliefs",
          "Future-fertility concerns","Low method knowledge","Opposes family planning"),
     S=c("offering","free","training","daily_provision","larc_stock","stockout_burden"),
     SL=c("Short + long-acting offering","Free-product provision","Trained-provider share",
          "Daily provision of offered modern methods","At least one LARC in stock","Stockout burden"))
    sindh_updated_supply <- function(x) {
     map<-c(offering="share_offer_larc_and_3_sa",free="mean_p_free_prod",training="mean_p_train",
            daily_provision="mean_daily_modern_provision",larc_stock="share_stocked_1plus_larc",
            stockout_burden="mean_facility_any_stockout")
     stopifnot(all(unname(map)%in%names(x)))
     as.data.frame(lapply(map,function(k)as.numeric(x[[k]])))
    }
    sindh_favorable_supply <- function(x) {x<-as.data.frame(x);x$stockout_burden<-1-x$stockout_burden;x}
    sindh_frequency_fields <- function() c("q507aa","q507aa1","q507aa2","q507ad","q507ae","q507af",
                                           "q507ag","q507ah","q507aj","q507ao")
    sindh_frequency <- function(raw) {
     q<-as.data.frame(lapply(raw[sindh_frequency_fields()],as.numeric))
     collapse<-function(fields)apply(as.matrix(q[fields]),1,function(v){
      offered<-v[v%in%1:4]
      if(length(offered))min(offered)else if(all(v%in%5))5 else NA_real_
     })
     # Seven classes. IUD contexts and injectable variants each count only once.
     # q507ab/ai are entirely unpopulated in this source; neither is coded as No.
     m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah")),
       male_condoms=q$q507ae,female_condoms=q$q507af,ecp=q$q507ao,
       iud=collapse(c("q507aa","q507aa1","q507aa2")),implants=q$q507aj)
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

# Country definition: training_reconstruction.R
.report_definitions[["training_reconstruction.R"]] <- function(target) {
  evalq({
    # Reconstruct the selected Sindh training topics from both survey encodings.
    sindh_training_fields <- function(fields) {
      grep("^q406(_[a-hyz])?_[0-9]+$", fields, value = TRUE)
    }

    sindh_training <- function(raw) {
      selected <- c("a", "c", "d", "g", "h")
      topics <- c(letters[1:8], "y", "z")
      slots <- sort(unique(sub("^q406_([a-hyz]_)?", "",
                               sindh_training_fields(names(raw)))))
      n <- nrow(raw)
      counts <- as.data.frame(setNames(rep(list(integer(n)), 12L),
        c("n_known", "n_trained", "n_dont_know", "n_conflict", "n_malformed",
          "n_unresolved_recorded", "n_text_recovered", "n_original_known", "n_original_trained",
          "n_overlap_staff", "n_compared_topics", "n_topic_disagreements")))
      for (slot in slots) {
        get_binary <- function(topic) {
          nm <- paste0("q406_", topic, "_", slot)
          if (nm %in% names(raw)) as.numeric(raw[[nm]]) else rep(NA_real_, n)
        }
        binary <- vapply(topics, get_binary, numeric(n))
        m <- binary[, selected, drop = FALSE]
        original_known <- rowSums(!is.na(m) & (m == 0 | m == 1)) > 0
        original_yes <- rowSums(m == 1, na.rm = TRUE) > 0
        counts$n_original_known <- counts$n_original_known + original_known
        counts$n_original_trained <- counts$n_original_trained + original_yes

        nm <- paste0("q406_", slot)
        text <- if (nm %in% names(raw)) as.character(raw[[nm]]) else rep("", n)
        text[is.na(text)] <- ""
        text <- toupper(trimws(gsub("[[:space:]]+", " ", text)))
        has_text <- nzchar(text)
        syntax <- has_text & grepl("^[A-HYZ]( [A-HYZ])*$", text)
        tokens <- vapply(toupper(topics), function(k)
          grepl(paste0("(^| )", k, "( |$)"), text), logical(n))
        colnames(tokens) <- topics
        # A known No in a complete binary set is valid. A partial all-No set is
        # insufficient to rule out training in the remaining included topics.
        binary_known <- original_yes | rowSums(!is.na(m) & (m == 0 | m == 1)) == length(selected)
        explicit_dk <- tokens[, "y"] | binary[, "y"] %in% 1
        overlap <- syntax & !explicit_dk & original_known
        compared <- !is.na(m) & (m == 0 | m == 1)
        counts$n_overlap_staff <- counts$n_overlap_staff + overlap
        counts$n_compared_topics <- counts$n_compared_topics + overlap * rowSums(compared)
        counts$n_topic_disagreements <- counts$n_topic_disagreements + overlap *
          rowSums(compared & (m != tokens[, selected, drop = FALSE]), na.rm = TRUE)
        explicit_none <- tokens[, "z"] | binary[, "z"] %in% 1
        substantive <- rowSums(tokens[, letters[1:8], drop = FALSE]) > 0 |
          rowSums(binary[, letters[1:8], drop = FALSE] == 1, na.rm = TRUE) > 0
        # Where both encodings are supplied, each recorded checkbox must agree
        # with the corresponding text selection, including None and Don't know.
        mismatch <- rowSums((!is.na(binary) & (binary == 0 | binary == 1)) &
          (binary != tokens), na.rm = TRUE) > 0
        conflict <- ((explicit_dk | explicit_none) & substantive) |
          (explicit_dk & explicit_none) | (syntax & mismatch)
        invalid_binary <- rowSums(!is.na(binary) & !(binary == 0 | binary == 1)) > 0
        malformed <- (has_text & !syntax) | invalid_binary
        known <- !explicit_dk & !conflict & !malformed &
          (syntax | binary_known | explicit_none)
        yes <- original_yes | rowSums(tokens[, selected, drop = FALSE]) > 0
        recorded <- has_text | rowSums(!is.na(binary)) > 0
        counts$n_known <- counts$n_known + known
        counts$n_trained <- counts$n_trained + (known & yes)
        counts$n_dont_know <- counts$n_dont_know + explicit_dk
        counts$n_conflict <- counts$n_conflict + conflict
        counts$n_malformed <- counts$n_malformed + malformed
        counts$n_unresolved_recorded <- counts$n_unresolved_recorded + (recorded & !known)
        counts$n_text_recovered <- counts$n_text_recovered + (known & !original_known & syntax)
      }
      counts$proportion <- ifelse(counts$n_known > 0, counts$n_trained / counts$n_known, NA_real_)
      counts$original_rebuild <- ifelse(counts$n_original_known > 0,
        counts$n_original_trained / counts$n_original_known, NA_real_)
      counts
    }

    # Revise an in-memory analysis copy, preserving the supplied cleaned inputs.
    apply_sindh_training <- function(facilities, raw, raw_index) {
      training <- sindh_training(raw[raw_index, , drop = FALSE])
      original <- as.numeric(facilities$p_train)
      stopifnot(identical(is.finite(original), is.finite(training$original_rebuild)),
        max(abs(original - training$original_rebuild), na.rm = TRUE) < 1e-12)
      facilities$training_original_p_train <- original
      facilities$training_original_n_tot <- facilities$n_tot
      facilities$training_original_n_train <- facilities$n_train
      facilities$p_train <- training$proportion
      facilities$n_tot <- ifelse(training$n_known > 0, training$n_known, NA_real_)
      facilities$n_train <- ifelse(training$n_known > 0, training$n_trained, NA_real_)
      for (k in names(training)) facilities[[paste0("training_", k)]] <- training[[k]]
      facilities
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
  source(file.path(dirname(script),"training_reconstruction.R"))
  inputs<-c(input,facility_input,raw_facility,raw_women,dictionary_file);hash<-unname(tools::md5sum(inputs))
  fac<-readRDS(facility_input);fac<-fac[fac$country=="sindh",]
  header<-haven::read_dta(raw_facility,n_max=0)
  r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","facility_id","district","q506","q702","q507ab","q507ai",sindh_frequency_fields(),sindh_training_fields(names(header)))))
  raw_id<-as.character(r$key);empty<-is.na(raw_id)|trimws(raw_id)=="";raw_id[empty]<-as.character(r$facility_id[empty])
  assert_unique(data.frame(fid=raw_id),"fid","Raw Sindh facilities")
  ix<-match(fac$fid,raw_id);stopifnot(!anyNA(ix),nrow(fac)==nrow(r),nrow(fac)==4308,sum(empty)==13,
   all(is.na(r$q507ab)|trimws(r$q507ab)==""),all(is.na(r$q507ai)|trimws(r$q507ai)==""))
  fp<-num(r$q506[ix]);fac$fp_provider<-fp%in%1
  fac<-apply_sindh_training(fac,r,ix)
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
   training_definition="Reconstructed Q406 selected topics A/C/D/G/H; Yes in any included topic is trained. Known complete No/None or a valid full selection with no included topic is untrained. Explicit Don't know, malformed or conflicting selections are unresolved in either encoding. Facility ratio uses staff with known status; average valid ratios across Q506 Yes facilities.",
   training_original_valid=sum(eligible$fp_provider&is.finite(eligible$training_original_p_train)),
   training_reconstructed_valid=sum(is.finite(eligible$p_train)),
   training_dont_know_records=sum(eligible$training_n_dont_know),
   training_conflicting_records=sum(eligible$training_n_conflict),
   training_malformed_records=sum(eligible$training_n_malformed),
   training_encoding_overlap_staff=sum(eligible$training_n_overlap_staff),
   training_encoding_topic_comparisons=sum(eligible$training_n_compared_topics),
   training_encoding_topic_disagreements=sum(eligible$training_n_topic_disagreements),
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
  write.csv(fac[c("fid","assigned_join_area","fp_provider",grep("^training_",names(fac),value=TRUE))],file.path(out,"training_facility_audit.csv"),row.names=FALSE,na="")
  jsonlite::write_json(audit,file.path(out,"dataset_verification.json"),auto_unbox=TRUE,pretty=TRUE)
  cat("Verified original women; reaggregated supply among",audit$linked_fp_providers,"FP facilities across 30 linked districts.\n")

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

  # Build Sindh's weighted need-group summaries and district planning scores.
  args <- commandArgs(trailingOnly = TRUE)
  opt <- function(k) { i <- match(k,args); if(is.na(i)||i==length(args)) stop("Supply ",k); args[i+1L] }
  input <- opt("--input"); need_file <- opt("--need-file"); facility_file <- opt("--facility-input"); out <- opt("--output-dir"); raw_facility<-opt("--raw-facility")
  for(pkg in c("survey","jsonlite")) if(!requireNamespace(pkg,quietly=TRUE)) stop("Install ",pkg)
  here <- dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
  source(file.path(here,"..","..","..","analysis/sindh/sindh_definitions.R"));source(file.path(here,"supply_definitions.R"));source(file.path(here,"training_reconstruction.R")); dir.create(out,recursive=TRUE,showWarnings=FALSE)
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
  header<-haven::read_dta(raw_facility,n_max=0)
  r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("key","facility_id","q506",sindh_frequency_fields(),sindh_training_fields(names(header)))))
  raw_id<-as.character(r$key);empty<-is.na(raw_id)|trimws(raw_id)=="";raw_id[empty]<-as.character(r$facility_id[empty])
  ri<-match(fac$fid,raw_id);stopifnot(!anyNA(ri))
  fac<-apply_sindh_training(fac,r,ri)
  frequency<-sindh_frequency(r[ri,]);fac<-cbind(fac,frequency)
  fp<-as.numeric(r$q506[ri]);fac<-fac[fp%in%1,]
  mapped<-!is.na(fac$geo)&nzchar(fac$geo)
  bool<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
  fac$offering<-bool(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
  fac$training<-as.numeric(fac$p_train);fac$larc_stock<-bool(fac$one_larc_stockin)
  fac$stockout_burden<-as.numeric(fac$p_any_stockout)
  stopifnot(nrow(fac)==2823,all(mapped),length(unique(fac$geo))==30)
  valid_mean <- function(v) if(any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
  wm <- function(v,w) {ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))survey_weighted_mean(v[ok],w[ok]) else NA_real_}
  savecsv <- function(v,name) write.csv(v,file.path(out,name),row.names=FALSE,na="")
  areas <- sort(profiles$geo); names_map <- setNames(profiles$district,profiles$geo)
  savecsv(fac[c("fid","geo",names(frequency))],"daily_provision_facility_audit.csv")
  savecsv(fac[c("fid","geo",grep("^training_",names(fac),value=TRUE))],"training_facility_audit.csv")
  training_comparison<-do.call(rbind,lapply(areas,function(g){
   ff<-fac[fac$geo==g,]
   data.frame(geo=g,district=names_map[[g]],fp_facilities=nrow(ff),
     original_valid=sum(is.finite(ff$training_original_p_train)),reconstructed_valid=sum(is.finite(ff$p_train)),
     original_mean=valid_mean(ff$training_original_p_train),reconstructed_mean=valid_mean(ff$p_train),
     known_staff=sum(ff$training_n_known),trained_staff=sum(ff$training_n_trained),
     dont_know_records=sum(ff$training_n_dont_know),conflicting_records=sum(ff$training_n_conflict),
     malformed_records=sum(ff$training_n_malformed))
  }))
  savecsv(training_comparison,"training_reconstruction_by_district.csv")
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
  full <- surgo_design(d[d$positive, ])
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
   aligned<-original
   if(side=="Supply")for(k in setdiff(fields,"stockout_burden"))aligned[,k]<-100-aligned[,k]
   rownames(aligned)<-areas;reference<-complete.cases(aligned)
   mu<-colMeans(aligned[reference,,drop=FALSE]);sd0<-apply(aligned[reference,,drop=FALSE],2,sd)
   stopifnot(all(sd0>0),sum(reference)==30)
   zz<-sweep(sweep(aligned,2,mu,"-"),2,sd0,"/")
   M[[side]]<-list(zscore=zz)
   for(k in fields)transforms[[length(transforms)+1L]]<-data.frame(geo=areas,side=side,variable=k,
    original_pct=original[,k],aligned_pct=aligned[,k],higher_raw_is_worse=side=="Supply"&&k=="stockout_burden",
    reference_included=reference,reference_n=sum(reference),indicator_mean=mu[k],indicator_SD=sd0[k],zscore=zz[,k])
  }
  ds<-rowMeans(M$Demand$zscore);ss<-rowMeans(M$Supply$zscore);df<-ds>1e-10;sf<-ss>1e-10
  scores<-data.frame(method="zscore",geo=areas,demand_score=ds,supply_score=ss,demand_flag=df,supply_flag=sf,
   demand_priority_rank=rank(-ds,ties.method="average"),supply_priority_rank=rank(-ss,ties.method="average"),
   category=ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
  # Isolate the effect of reconstructing training; all other inputs stay fixed.
  original_aligned<-sapply(S,function(k){
   if(k=="training")training_comparison$original_mean[match(areas,training_comparison$geo)]
   else profiles[[k]][match(areas,profiles$geo)]
  })
  for(k in setdiff(S,"stockout_burden"))original_aligned[,k]<-1-original_aligned[,k]
  original_score<-rowMeans(scale(original_aligned))
  category<-function(a,b)ifelse(a&b,"both",ifelse(a,"demand",ifelse(b,"supply","neither")))
  training_flags<-do.call(rbind,lapply(c(0,.5,1,1.5),function(cutoff){
   demand_flag<-if(cutoff==0)ds>1e-10 else as.numeric(scale(ds))>=cutoff
   previous_flag<-if(cutoff==0)original_score>1e-10 else as.numeric(scale(original_score))>=cutoff
   revised_flag<-if(cutoff==0)ss>1e-10 else as.numeric(scale(ss))>=cutoff
   data.frame(geo=areas,district=unname(names_map[areas]),threshold_sd=cutoff,
    original_supply_score=original_score,reconstructed_supply_score=ss,
    original_category=category(demand_flag,previous_flag),reconstructed_category=category(demand_flag,revised_flag),
    changed=previous_flag!=revised_flag)
  }))
  savecsv(training_flags,"training_classification_comparison.csv")
  thresholds<-data.frame(method="zscore",demand_cutoff=0,supply_cutoff=0,demand_rule="strictly above final-score mean (zero)",
   supply_rule="strictly above constraints-score mean (zero)",demand_reference_n=30,supply_reference_n=30)
  trace<-do.call(rbind,transforms)
  stopifnot(abs(mean(ds))<1e-12,abs(mean(ss))<1e-12)
  planning<-lapply(areas,function(g){q<-scores[scores$method=="zscore" & scores$geo==g,];dt<-trace[trace$geo==g & trace$side=="Demand",];st<-trace[trace$geo==g & trace$side=="Supply",]
   list(geo=g,name=names_map[[g]],category=q$category,demand_flag=q$demand_flag,supply_flag=q$supply_flag,demand_score=q$demand_score,supply_score=q$supply_score,
    top_demand=head(dt$variable[order(-dt$zscore,match(dt$variable,D),na.last=TRUE)],3),
    top_supply=if(all(is.finite(st$zscore)))head(st$variable[order(-st$zscore,match(st$variable,S))],3) else character(),
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
    cv<-survey_weighted_mean((a-survey_weighted_mean(a,w))*(b-survey_weighted_mean(b,w)),w)
    den<-sqrt(survey_weighted_mean((a-survey_weighted_mean(a,w))^2,w)*survey_weighted_mean((b-survey_weighted_mean(b,w))^2,w))
    correlations[[length(correlations)+1L]]<-data.frame(side=side,first=pair[1],second=pair[2],correlation=cv/den,paired_records=sum(ok))
   }
  }
  savecsv(do.call(rbind,correlations),"component_correlations.csv")
  audit<-list(supply_score_orientation="constraints",country="Sindh",source_rows=nrow(x),positive_weight_rows=sum(d$positive),consenting_positive_weight=sum(d$positive),need_n=sum(d$need),
   need_variable="Authorized fpn2_derived matched by col_id; original source fpn2 is entirely code 95 and is not used as a need flag.",
   need_qualification="Retain the approved upstream instrument-based rule, including SA1=95/97 exclusions. Not a DHS unmet-need algorithm; no need_n permanent-protection additions.",
   women_weight="Normalizedweights",weight_field="Normalizedweights",psu_field="Recorded unrounded clusterCode",
   survey_psus=length(unique(d$psu[d$positive])),design_fields_confirmed=TRUE,
   survey_design="Final normalized weights and recorded clusterCode; no inferred strata or FPC. Full consenting positive-weight design before need-domain subsetting. Full original sampling design remains unavailable.",
   linked_districts=length(areas),districts=length(areas),all_facility_records=nrow(inventory),linked_source_facilities=nrow(inventory),
   fp_facility_records=nrow(fac),linked_fp_facilities=nrow(fac),daily_valid_linked=sum(is.finite(fac$daily_provision)),daily_undefined_linked=sum(!is.finite(fac$daily_provision)),
   training_original_valid=sum(is.finite(fac$training_original_p_train)),training_reconstructed_valid=sum(is.finite(fac$p_train)),
   training_known_staff=sum(fac$training_n_known),training_trained_staff=sum(fac$training_n_trained),
   training_dont_know_records=sum(fac$training_n_dont_know),training_conflicting_records=sum(fac$training_n_conflict),
   training_malformed_records=sum(fac$training_n_malformed),
   training_encoding_overlap_staff=sum(fac$training_n_overlap_staff),
   training_encoding_topic_comparisons=sum(fac$training_n_compared_topics),
   training_encoding_topic_disagreements=sum(fac$training_n_topic_disagreements),
   training_definition="Reconstructed raw Q406 topics A/C/D/G/H, same included topics as the original Sindh code. Explicit Don't know and conflicting/malformed selections are unresolved across both encodings. Known staff form each facility denominator; district means weight valid FP facilities equally. Original cleaned values retained in the reconstruction audit.",
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

  # Sindh weighted modern-use models; district supply and individual demand.
  args <- commandArgs(trailingOnly = TRUE)
  opt <- function(k) { i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L] }
  optional <- function(k,default) {i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-opt("--data-dir");out<-opt("--output-dir");score_method<-optional("--score-method","zscore");stopifnot(score_method=="zscore")
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
  resolution<-"district";demand_resolution<-"individual"
  z<-p$scores[p$scores$method=="zscore",]
  profiles<-unique(cbind(d[c("geo","district")],frame$supply,supply_risk=d$supply_risk))
  stopifnot(nrow(profiles)==30,!anyDuplicated(profiles$geo))
  write.csv(profiles,file.path(out,"model_supply_profiles.csv"),row.names=FALSE,na="")
  controls<-c("age10","age10_sq","education","fertility_goal")
  specs<-list(score_demand=c(controls,"demand_score"),supply=c(controls,"supply_risk"),
   score_joint=c(controls,"demand_score","supply_risk"),components_demand=c(controls,D),components_joint=c(controls,D,"supply_risk"))
  positive<-d$positive
  common_vars<-unique(c("y_modern",controls,D,"demand_score","supply_risk"))
  d$eligible<-d$need & positive & complete.cases(d[common_vars]);cc<-droplevels(d[d$eligible,])
  stopifnot(all(cc$y_modern%in%0:1),length(unique(cc$partnered[is.finite(cc$partnered)]))==1,
   all(cc$partnered[is.finite(cc$partnered)]==1),length(unique(cc$geo))==30)
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
      leaveout[[length(leaveout) + 1L]] <- data.frame(model = id, omitted_district = g, term = fields,
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
  savecsv(leaveout, "leave_one_district_out.csv")
  missing <- data.frame(variable = common_vars,
    missing_in_need = vapply(d[d$need, common_vars], function(a) sum(is.na(a)), integer(1)))
  savecsv(missing, "model_missingness.csv")
  savecsv(z[, c("geo", "demand_score", "supply_score")], "model_district_scores.csv")
  # Each score-model check uses only its formula variables; supply-only must not
  # require the six personal demand responses.
  sensitivity <- list()
  for (id in c("score_demand", "supply", "score_joint")) {
    available <- d$need & positive & complete.cases(d[all.vars(fits[[id]]$formula)])
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
    v <- if (k == "supply_risk") unique(cc[, c("geo", k)])[[k]] else unique(cc[[k]])
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

  source(file.path(script_dir,"..","plot_models.R"))
  labels<-render_modern_use_plots(coefficients,curves,gaps,cc,p$demand_labels,D,out,score_method,demand_resolution,resolution,"sindh")
  influence<-do.call(rbind,lapply(split(leaveout,interaction(leaveout$model,leaveout$term,drop=TRUE)),function(a)
   data.frame(model=a$model[1],term=a$term[1],full_estimate=a$full_estimate[1],deletion_min=min(a$estimate),deletion_max=max(a$estimate),deletion_sign_changes=sum(sign(a$estimate)!=sign(a$full_estimate)))))
  savecsv(influence,"influence_summary.csv")
  saveRDS(fits,file.path(out,"fitted_models.rds"))
  audit<-list(regression_function="survey::svyglm",design_ids="~1",design_strata="None",country="Sindh",outcome="Current modern contraceptive use only: confirmed modern=1; traditional-only or confirmed nonuse=0; unresolved status remains missing.",
   need_definition="fpn2_derived=1, exact supplied derivation matched by col_id. No need_n permanent-protection additions.",
   need_n=sum(d$need),common_n=nrow(cc),common_events=sum(cc$y_modern),score_method=score_method,
   demand_resolution="individual",supply_resolution="district",districts=length(unique(cc$geo)),
   survey_psus=length(unique(cc$psu)),demand_distinct_values=length(unique(cc$demand_score)),supply_distinct_values=length(unique(cc$supply_risk)),
   controls=controls,omitted_controls="Urban/rural unavailable; partnership constant among the modeled women. Do not add an unidentified or constant term.",
   weights="Normalizedweights; common mean-one fitting rescaling. No facility weights.",
   inference="Weighted logistic coefficients with district-clustered CR2 covariance, coefficient-specific Satterthwaite intervals, HTZ block tests and Holm adjustment within each joint model. No random effects or interactions.",
   design="survey::svyglm with ids=~1, no strata or FPC, original SURGO weights; administrative-area CR2 covariance and Satterthwaite tests. Original sampling PSUs are not specified in this model.",
   score_definition="Demand: mean of six personal binary indicators standardized with item-specific weighted need-group means/population SDs. Supply: negative mean of six favorable district measures, after burden becomes continuity, standardized against the same 30 district references. One original average-z point; no final-score rescaling.",
   supply_missing="All 30 districts have complete selected Q506-Yes facility profiles. No zero filling, partial component scores, imputation or Rural/Urban split.",
   excluded_need=sum(d$need)-nrow(cc),eligible_weight_share=survey_weighted_mean(as.numeric(d$eligible[d$need]),d$weight[d$need]),
   excluded_districts=setdiff(unique(d$geo[d$need]),unique(cc$geo)),
   uncertainty="Intervals condition on estimated scores and do not propagate their construction uncertainty; full original sampling design is not available. Diagnostics are not a formal power analysis or causal identification.",
   probability_checks=list(maximum_direct_prediction_error=max_prediction_error,maximum_numeric_gradient_error=max_gradient_error),
   report_md5=report_hash,source_hashes=p$audit$source_md5)
  payload<-list(audit=audit,coefficients=coefficients,samples=samples,tests=tests,vifs=diagnostics,curves=curves,gaps=gaps,influence=influence,labels=as.list(labels))
  jsonlite::write_json(payload,file.path(out,"model_results.json"),dataframe="rows",auto_unbox=TRUE,digits=12,na="null")
  jsonlite::write_json(audit,file.path(out,"model_audit.json"),auto_unbox=TRUE,pretty=TRUE,digits=12,na="null")
  print(samples);print(tests);print(coefficients[coefficients$term%in%c("demand_score","supply_risk"),])

}

# Prepare map -----------------------------------------------------
.report_steps[["prepare_map.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Verify a one-to-one named district boundary crosswalk and project for display.
  args<-commandArgs(trailingOnly=TRUE)
  opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  boundary<-opt("--boundary");data_dir<-opt("--data-dir")
  for(pkg in c("sf","jsonlite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"))
  b<-sf::st_read(boundary,quiet=TRUE);stopifnot(nrow(b)==30)
  key<-function(v)toupper(gsub("[^A-Za-z0-9]","",v))
  aliases<-c(KAMBARSHAHDADKOT="QAMBARSHAHDADKOT",CENTRALKARACHI="KARACHICENTRALDISTRICT",EASTKARACHI="KARACHIEAST",
   KORANGIKARACHI="DISTRICTKORANGI",MALIRKARACHI="DISTRICTMALIR",WESTKARACHI="KARACHIWEST",KEAMARI="DISTRICTKEMARI",SOUTHKARACHI="KARACHISOUTH")
  b$geo<-key(b$districts);ii<-match(b$geo,names(aliases));b$geo[!is.na(ii)]<-aliases[ii[!is.na(ii)]]
  stopifnot(!anyDuplicated(b$geo),setequal(b$geo,p$profiles$geo))
  b$name<-p$profiles$district[match(b$geo,p$profiles$geo)]
  write.csv(data.frame(boundary_name=b$districts,join_geo=b$geo,join_name=b$name,exact_normalized_name=key(b$districts)==b$geo),
   file.path(data_dir,"map_name_crosswalk.csv"),row.names=FALSE)
  utm<-sf::st_transform(sf::st_make_valid(b),32642)
  ct<-sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(utm)))
  features<-lapply(seq_len(nrow(utm)),function(i){g<-sf::st_geometry(utm)[[i]]
   rings<-if(inherits(g,"POLYGON"))g else unlist(g,recursive=FALSE)
   list(geo=utm$geo[i],name=utm$name[i],rings=unname(rings),center=unname(ct[i,1:2]),
    karachi=grepl("KARACHI|MALIR|KORANGI|KEMARI",utm$geo[i]))})
  jsonlite::write_json(list(crs="EPSG:32642",features=features),file.path(data_dir,"map_layout.json"),
   auto_unbox=TRUE,digits=10)
  cat('Verified 30 district polygons and explicit name aliases; no supply assignments changed.\n')

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
  raw_facility<-arg("--raw-facility");raw_women<-arg("--raw-women");cleaned<-arg("--cleaned-facility");need_file<-arg("--need-file");aliases_file<-arg("--district-aliases")
  out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));x<-readRDS(input)
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women;D<-p$demand_variables;S<-p$supply_variables
  checks<-list();verify<-function(name,ok){
   if(!isTRUE(ok))stop("Verification failed: ",name)
   checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE)
  }
  same<-function(a,b,tol=1e-10)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=tol))
  key<-function(v)toupper(gsub("[^A-Za-z0-9]","",as.character(v)))
  valid_mean<-function(v){v<-as.numeric(v);if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_}
  bool<-function(v)ifelse(tolower(v)%in%c("1","true","yes"),1,ifelse(tolower(v)%in%c("0","false","no"),0,NA_real_))
  raw<-haven::read_dta(raw_women)
  verify("Original 10364 women and IDs",nrow(x)==10364&&identical(x$col_id,raw$col_id))
  for(k in names(raw))verify(paste("Original SURGO field",k),same(x[[k]],raw[[k]],0))
  ref<-readRDS(need_file);nr<-match(x$col_id,ref$col_id)
  exclude<-raw$sf0b%in%1|raw$sf0c%in%1|raw$sf1a%in%3|raw$sf1b%in%3|raw$sf1c%in%3|
   raw$sa0%in%2|raw$sa1%in%c(5,6,95,97)|raw$pc0%in%1|raw$sf2%in%1|raw$pc18%in%1|raw$pc19%in%2
  positive<-raw$irb1%in%1&is.finite(as.numeric(raw$Normalizedweights))&raw$Normalizedweights>0
  verify("Authorized need flag and unchanged policy",all(!is.na(nr[positive]))&&
   all(d$need[positive]==ref$fpn2_derived[nr[positive]])&&identical(d$need,positive&!exclude)&&sum(d$need)==3274)
  verify("Original final woman weights",same(d$weight,as.numeric(raw$Normalizedweights),0))
  verify("Recorded sampling clusterCode retained",identical(d$psu,as.character(raw$clusterCode)))
  verify("Sampling clusters nested within district",all(vapply(split(d$geo[positive],d$psu[positive]),function(v)length(unique(v))==1,logical(1))))
  verify("Every bar uses all need women",all(p$bars$denominator==3274))
  verify("Raw woman district is join district",identical(key(haven::as_factor(raw$rb0c)),d$geo)&&identical(d$geo,key(x$link_geo_join)))
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
  mp<-as.matrix(raw[paste0("mp4_",1:9)]);sel<-rowSums(mp[,1:8,drop=FALSE]==1,na.rm=TRUE)>0
  none<-mp[,9]%in%1;ok<-apply(mp,1,function(v)all(v%in%0:1))&(sel|none)&!(sel&none)
  fert<-ifelse(ok,as.numeric(rowSums(mp[,1:6,drop=FALSE]==1,na.rm=TRUE)>0),NA_real_)
  verify("Raw coherent fertility concerns",same(fert,d$fertility))
  verify("Raw opposition to FP",same(ifelse(raw$fp1%in%1:4,as.numeric(raw$fp1%in%3:4),NA_real_),d$opposes))
  f<-readRDS(facility_input);f<-f[f$country=="sindh",];r<-haven::read_dta(raw_facility)
  raw_id<-as.character(r$key);empty<-is.na(raw_id)|trimws(raw_id)=="";raw_id[empty]<-as.character(r$facility_id[empty])
  ri<-match(f$fid,raw_id);verify("Facility raw IDs one-to-one",!anyNA(ri)&&!anyDuplicated(ri)&&length(ri)==4308&&sum(empty)==13)
  f$geo<-key(f$assigned_join_area)
  ali<-read.csv(aliases_file,stringsAsFactors=FALSE);expected_geo<-key(haven::as_factor(r$district[ri]))
  ai<-match(expected_geo,ali$source_normalized);expected_geo[!is.na(ai)]<-ali$target_normalized[ai[!is.na(ai)]]
  verify("Raw named district plus supplied aliases",identical(expected_geo,f$geo))
  verify("Previously matched districts unchanged",all(f$geo[!is.na(f$geo_join_pipeline)]==key(f$geo_join_pipeline[!is.na(f$geo_join_pipeline)])))
  verify("881 missing source-crosswalk facilities now located",sum(is.na(f$geo_join_pipeline))==881&&all(!is.na(f$geo)))
  fp<-as.numeric(r$q506[ri]);verify("FP eligibility counts",sum(fp%in%1)==2823&&sum(fp%in%2)==1035&&sum(is.na(fp))==450)
  areas<-sort(unique(d$geo[d$need]));verify("2823 linked FP providers in 30 districts",sum(fp%in%1&f$geo%in%areas)==2823&&length(areas)==30)
  verify("Uncollected frequency variants not coded No",all(is.na(r$q507ab)|trimws(r$q507ab)=="")&&all(is.na(r$q507ai)|trimws(r$q507ai)==""))
  # Independently collapse frequency variants; unknowns never become absent or zero.
  q<-lapply(r[ri,grep("^q507a",names(r),value=TRUE)],as.numeric)
  collapse<-function(cols)vapply(seq_len(nrow(f)),function(i){
   v<-vapply(cols,function(k)q[[k]][i],numeric(1));a<-v[v%in%1:4]
   if(length(a))min(a)else if(all(v%in%5))5 else NA_real_
  },numeric(1))
  m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah")),male_condoms=q$q507ae,
   female_condoms=q$q507af,ecp=q$q507ao,iud=collapse(c("q507aa","q507aa1","q507aa2")),implants=q$q507aj)
  den<-rowSums(m>=1&m<=4,na.rm=TRUE);num<-rowSums(m==1,na.rm=TRUE)
  daily<-ifelse(den>0,num/den,NA_real_)
  daily_audit<-read.csv(file.path(data_dir,"daily_provision_facility_audit.csv"))
  verify("Independent daily-frequency formula",same(daily_audit$daily_provision,daily[match(daily_audit$fid,f$fid)]))
  # Independently enumerate only recorded staff slots and apply the stated rules.
  training_rows<-list();included<-c("A","C","D","G","H");topics<-c(LETTERS[1:8],"Y","Z")
  for(slot in sub("^q406_","",grep("^q406_[0-9]+$",names(r),value=TRUE))){
   text<-as.character(r[[paste0("q406_",slot)]]);text[is.na(text)]<-""
   text<-toupper(trimws(gsub("[[:space:]]+"," ",text)))
   binary<-as.matrix(as.data.frame(lapply(r[paste0("q406_",tolower(topics),"_",slot)],as.numeric)))
   ii<-which(nzchar(text)|rowSums(!is.na(binary))>0)
   if(!length(ii))next
   statuses<-vapply(ii,function(i){
    v<-binary[i,];names(v)<-topics
    words<-if(nzchar(text[i]))strsplit(text[i]," ",fixed=TRUE)[[1]] else character()
    if(any(!words%in%topics)||any(!is.na(v)&!v%in%0:1))return(NA_real_)
    yes<-union(words,names(v)[which(v==1)])
    if("Y"%in%yes)return(NA_real_)
    if("Z"%in%yes&&any(yes%in%LETTERS[1:8]))return(NA_real_)
    if(length(words)&&any(v[!is.na(v)]!=as.numeric(names(v)[!is.na(v)]%in%words)))return(NA_real_)
    if(any(yes%in%included))return(1)
    if(length(words)||"Z"%in%yes||all(v[included]%in%0:1))return(0)
    NA_real_
   },numeric(1))
   training_rows[[length(training_rows)+1L]]<-data.frame(raw_row=ii,status=statuses)
  }
  staff<-do.call(rbind,training_rows)
  independent_training<-rep(NA_real_,nrow(r))
  means<-tapply(staff$status,staff$raw_row,valid_mean)
  independent_training[as.integer(names(means))]<-means
  ta<-read.csv(file.path(data_dir,"training_facility_audit.csv"))
  ti<-match(ta$fid,f$fid)
  verify("Independent raw staff training reconstruction",same(ta$training_proportion,independent_training[ri[ti]]))
  verify("Explicit Don't know excluded consistently",sum(ta$training_n_dont_know)==57&&sum(ta$training_n_known)==15116)
  verify("Conflicting or malformed records flagged",sum(ta$training_n_conflict)==6&&sum(ta$training_n_malformed)==2)
  verify("Training known-status facility coverage",sum(is.finite(ta$training_proportion))==2811&&all(ta$training_n_trained<=ta$training_n_known))
  verify("Text encoding agrees with recorded topic checkboxes",sum(ta$training_n_overlap_staff)==3696&&sum(ta$training_n_compared_topics)==18480&&sum(ta$training_n_topic_disagreements)==0)
  f$offering<-bool(f$one_larc_3_sa_offer);f$free<-as.numeric(f$p_free_prod);f$training<-independent_training[ri]
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
   verify(paste("district supply assigned to every resident",g,k),same(frame$supply[[k]][d$geo==g],rep(valid_mean(v),sum(d$geo==g))))
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
  for(g in areas){
   plan<-p$planning[p$planning$geo==g,]
   for(side in c("Demand","Supply")){
    fields<-if(side=="Demand")D else S
    t<-p$component_trace[p$component_trace$geo==g&p$component_trace$side==side,]
    ordered<-if(side=="Demand")order(-t$zscore,match(t$variable,fields))else order(-t$zscore,match(t$variable,fields))
    expected<-head(t$variable[ordered],3)
    observed<-if(side=="Demand")unlist(plan$top_demand)else unlist(plan$top_supply)
    verify(paste("Top three standardized components",g,side),identical(as.character(expected),as.character(observed)))
   }
  }
  previous<-NULL
  for(folder in "models_zscore"){
   models<-readRDS(file.path(data_dir,folder,"fitted_models.rds"));co<-read.csv(file.path(data_dir,folder,"model_coefficients.csv"))
   tt<-read.csv(file.path(data_dir,folder,"model_block_tests.csv"))
   for(id in names(models)){
    ob<-models[[id]];aa<-ob$data;fo<-ob$formula
    verify(paste(folder,id,"common women across all five fits"),is.null(previous)||identical(previous,aa$row));previous<-aa$row
    verify(paste(folder,id,"modern use and original need"),all(aa$need)&&all(aa$y_modern%in%0:1)&&same(aa$y_modern,d$y_modern[aa$row]))
    verify(paste(folder,id,"agreed covariates"),all(c("age10","age10_sq","education","fertility_goal")%in%all.vars(fo)))
    verify(paste(folder,id,"no random effects or interactions"),!grepl("[|:*]",paste(deparse(fo),collapse=" ")))
    fit<-glm(fo,data=aa,weights=aa$weight/mean(aa$weight),family=quasibinomial(),control=glm.control(maxit=100))
    verify(paste(folder,id,"weighted coefficients"),same(coef(fit),coef(ob$fit)))
    V<-clubSandwich::vcovCR(fit,cluster=aa$geo,type="CR2",inverse_var=FALSE)
    verify(paste(folder,id,"district CR2 covariance"),same(as.vector(V),as.vector(ob$V)))
    ct<-as.data.frame(clubSandwich::coef_test(fit,vcov=V,test="Satterthwaite"))
    saved_co<-co[co$model==id,];saved_co<-saved_co[match(rownames(ct),saved_co$term),]
    verify(paste(folder,id,"Satterthwaite odds-ratio intervals"),
     same(saved_co$or_lo,exp(ct$beta-qt(.975,ct$df_Satt)*ct$SE))&&
     same(saved_co$or_hi,exp(ct$beta+qt(.975,ct$df_Satt)*ct$SE)))
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
  context_score<-z$supply_score[match(aa$geo,z$geo)]
  verify("Supply risk is negative district readiness",same(aa$supply_risk,context_score))
  verify("30 distinct district supply profiles",length(unique(aa$supply_risk))==30)
  verify("Z-score primary with four alternative scoring methods",setequal(unique(p$scores$method),c("zscore","rank","raw","robust","median_count")))
  verify("No zero-coded missing supply",all(complete.cases(frame$supply)))
  expected_common<-which(d$need&complete.cases(d[c("y_modern","age10","age10_sq","education","fertility_goal",D)]))
  verify("Common model rows independently recovered",identical(aa$row,expected_common))
  verify("Constant partnership and absent rural/urban",all(aa$partnered[is.finite(aa$partnered)]==1)&&!"urban"%in%names(d))
  # Rebuild current-use classification from original responses independently.
  prompts<-as.matrix(raw[paste0("cu5",letters[1:4])])
  current<-ifelse(raw$cu1%in%c(1,3)|rowSums(prompts==1,na.rm=TRUE)>0,1,
   ifelse(raw$cu1%in%2&rowSums(prompts==2,na.rm=TRUE)==4,0,NA_real_))
  modern<-rowSums(as.matrix(raw[paste0("cu4_",1:8)])==1,na.rm=TRUE)>0|raw$cu5c%in%1|raw$cu5d%in%1|(raw$cu4_11%in%1&raw$cu4_lam%in%1)
  traditional<-rowSums(as.matrix(raw[paste0("cu4_",c(9,10,12:15))])==1,na.rm=TRUE)>0|raw$cu5a%in%1|raw$cu5b%in%1
  y<-ifelse(current%in%0,0,ifelse(current%in%1&modern,1,ifelse(current%in%1&traditional,0,NA_real_)))
  verify("Raw modern-use outcome independently reproduced",same(y,d$y_modern))
  for(id in names(models)){
   ob<-models[[id]];tt<-read.csv(file.path(data_dir,"models_zscore/model_block_tests.csv"));tt<-tt[tt$model==id,]
   verify(paste(id,"within-model Holm correction"),same(tt$holm_p,p.adjust(tt$p,"holm")))
   if(id%in%c("score_joint","components_joint"))verify(paste(id,"two block tests"),nrow(tt)==2)
  }
  gaps<-read.csv(file.path(data_dir,"models_zscore/individual_demand_probability_gaps.csv"))
  ob<-models$components_joint
  for(k in D){
   a0<-ob$data;a1<-ob$data;a0[[k]]<-0;a1[[k]]<-1
   p0<-weighted.mean(predict(ob$fit,newdata=a0,type="response"),a0$weight)
   p1<-weighted.mean(predict(ob$fit,newdata=a1,type="response"),a1$weight)
   saved<-gaps[gaps$term==k,]
   verify(paste("Independent weighted probability contrast",k),same(p0,saved$probability_absent)&&
    same(p1,saved$probability_present)&&same(p1-p0,saved$estimate))
  }
  save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
  jsonlite::write_json(list(all_passed=all(save$passed),check_count=nrow(save),source_rows=10364,need_n=3274,
   fp_facilities=2823,linked_fp_facilities=2823,model_n=nrow(aa),models=5,
   fp_providers_only=TRUE,health_PB2A_only=TRUE,primary_cutoffs_zero=TRUE,all_models_same_women=TRUE,
   no_random_effects=TRUE,limitations="Implementation verification; not causal validation, full survey-design reconstruction or a formal power analysis."),
   file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
  cat("Passed",nrow(save),"independent checks.\n")

}

# Save updated dataset -----------------------------------------------------
.report_steps[["save_updated_dataset.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Save a complete woman-level analytic join; restricted outputs are not packaged.
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  input<-arg("--input");data_dir<-arg("--data-dir");out<-arg("--output-dir")
  x<-readRDS(input);frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables
  ref<-read.csv(file.path(data_dir,"models_zscore/individual_demand_score_reference.csv"))
  models<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"))
  stopifnot(nrow(x)==nrow(d),nrow(x)==10364,identical(x$col_id,d$id))
  original<-x;original_fields<-names(x)
  if(!"fpn2_derived"%in%names(x))x$fpn2_derived<-d$need_code
  x$analysis_in_need_fp<-d$need;x$analysis_modern_use<-d$y_modern
  for(k in D)x[[paste0("demand_",k)]]<-d[[k]]
  Z<-sapply(D,function(k){r<-ref[ref$variable==k,];(d[[k]]-r$weighted_mean)/r$weighted_SD})
  x$demand_risk_score<-rowMeans(Z);x$demand_risk_score[!d$need]<-NA_real_
  z<-p$scores[p$scores$method=="zscore",]
  x$supply_risk_score<-z$supply_score[match(d$geo,z$geo)]
  x$supply_readiness_score<- -x$supply_risk_score
  x$model_complete_case<-d$row%in%models$score_joint$data$row
  stopifnot(all(vapply(original_fields,function(k)identical(x[[k]],original[[k]]),logical(1))))
  ii<-models$score_joint$data$row
  stopifnot(max(abs(x$demand_risk_score[ii]-models$score_joint$data$demand_score))<1e-10,
   max(abs(x$supply_risk_score[ii]-models$score_joint$data$supply_risk))<1e-10)
  attr(x$demand_risk_score,"label")<-"Personal mean standardized concern score; need group only"
  attr(x$supply_risk_score,"label")<-"Negative mean favorable district supply z-score; higher is worse"
  attr(x$supply_readiness_score,"label")<-"Mean favorable district supply z-score; higher is better"
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  saveRDS(x,file.path(out,"sindh_women_manitoba_updated.rds"),compress="gzip")
  haven::write_dta(x,file.path(out,"sindh_women_manitoba_updated.dta"),version=15)
  jsonlite::write_json(list(rows=nrow(x),original_join_columns_preserved=TRUE,need_n=sum(d$need),
   common_model_n=sum(x$model_complete_case),supply_contexts=length(unique(d$geo)),
   demand_score_scope="Each woman's own six responses; weighted need-group component references; missing outside the need group.",
   supply_score_scope="Assigned district FP-provider summaries; all residents share their district score; no Rural/Urban split.",
   restricted_output="Woman-level microdata; excluded from source-code upload packages."),
   file.path(out,"updated_dataset_verification.json"),auto_unbox=TRUE,pretty=TRUE)
  cat("Saved updated individual dataset: 10364 women,",sum(x$model_complete_case),"common model cases.\n")

}

# Regression sensitivity -----------------------------------------------------
.report_steps[["regression_sensitivity.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Complete-response sensitivity checks for administrative-area country analyses.
  # Input locations are supplied by the caller; only aggregate summaries enter HTML.
  entry<-grep("^--file=",commandArgs(),value=TRUE)
  project<-if(length(entry))dirname(normalizePath(gsub("~+~"," ",sub("^--file=","",entry[1]),fixed=TRUE)))else getwd()
  while(!file.exists(file.path(project,"support/survey_methods.R"))){
   parent<-dirname(project);if(identical(parent,project))stop("Run within the complete project");project<-parent
  }
  source(file.path(project,"support/survey_methods.R"))
  source(file.path(project,"support/risk_scores.R"))
  sensitivity_configuration<-function(country){
   configurations<-list(
    sindh=list(country="Sindh",area_singular="district",area_plural="districts",need=3274L,common=3115L,base=3263L,areas=30L,appendix_title="Appendix · Sensitivity checks"),
    senegal=list(country="Senegal",area_singular="département",area_plural="départements",need=1905L,common=1558L,base=1893L,areas=33L,appendix_title="Appendix C · Sensitivity checks"),
    cotedivoire=list(country="Côte d’Ivoire",area_singular="département",area_plural="départements",need=4292L,common=3951L,base=4185L,areas=77L,appendix_title="Appendix C · Sensitivity checks"))
   if(!country%in%names(configurations))stop("Unsupported administrative-area country")
   configurations[[country]]
  }

  response_probabilities<-function(base,controls){
   stopifnot(all(complete.cases(base[c("responder","y_modern",controls,"geo","weight")])))
   groups<-split(base$responder,base$geo)
   stopifnot(all(vapply(groups,function(x)any(x==1),logical(1))))
   complete_areas<-names(groups)[vapply(groups,function(x)all(x==1),logical(1))]
   partial<-!base$geo%in%complete_areas;probability<-rep(1,nrow(base));fit<-NULL
   if(any(partial)){
    a<-droplevels(base[partial,]);a$response_area<-factor(a$geo)
    formula<-reformulate(c("y_modern",controls,"response_area"),"responder")
    fit<-fit_survey_logistic(formula,a)
    probability[partial]<-as.numeric(predict(fit,newdata=a,type="response"))
   }
   stopifnot(all(is.finite(probability)),all(probability>0),all(probability<=1))
   list(probability=probability,fit=fit,complete_areas=complete_areas)
  }

  cluster_jackknife_covariance<-function(estimates){
   G<-nrow(estimates);centered<-sweep(estimates,2,colMeans(estimates),"-")
   crossprod(centered)*(G-1)/G
  }

  build_regression_sensitivity<-function(data_dir,out,country){
   config<-sensitivity_configuration(country)
   dir.create(out,recursive=TRUE,showWarnings=FALSE)
   frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
   p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables
   stopifnot(p$audit$need_n==config$need,p$audit$supply_score_orientation=="constraints")
   original<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"))
   refs<-read.csv(file.path(data_dir,"models_zscore/individual_demand_score_reference.csv"))
   d$demand_score<-rowMeans(sapply(D,function(k){r<-refs[refs$variable==k,];(d[[k]]-r$weighted_mean)/r$weighted_SD}))
   d$supply_risk<-supply_constraint_score(frame$supply,p$component_trace,TRUE)
   controls<-setdiff(all.vars(original$supply$formula),c("y_modern","supply_risk"))
   common_vars<-c("y_modern",controls,D,"demand_score","supply_risk")
   d$common<-d$need&d$positive&complete.cases(d[common_vars])
   d$base<-d$need&d$positive&complete.cases(d[c("y_modern",controls,"supply_risk")])
   stopifnot(identical(d$row[d$common],original$score_joint$data$row),sum(d$common)==config$common,sum(d$base)==config$base)
   stopifnot(all(vapply(split(d$supply_risk,d$geo),function(v)length(unique(v))==1,logical(1))))
   source_files<-c(file.path(data_dir,"analysis_frame.rds"),file.path(data_dir,"report_data.json"),
    file.path(data_dir,"models_zscore/fitted_models.rds"))
   source_hash<-unname(tools::md5sum(source_files))
   savecsv<-function(x,name)write.csv(x,file.path(out,name),row.names=FALSE,na="")
   wm<-function(x,w){ii<-is.finite(x);if(any(ii))survey_weighted_mean(x[ii],w[ii])else NA_real_}
   full_design<-surgo_design(d[d$positive,])
   cr2_table<-function(f,a){
    V<-geographic_cr2(f,a$geo);ct<-as.data.frame(clubSandwich::coef_test(f,vcov=V,test="Satterthwaite"))
    data.frame(term=rownames(ct),estimate=ct$beta,se=ct$SE,df=ct$df_Satt,p=ct$p_Satt,
     odds_ratio=exp(ct$beta),or_lo=exp(ct$beta-qt(.975,ct$df_Satt)*ct$SE),or_hi=exp(ct$beta+qt(.975,ct$df_Satt)*ct$SE))
   }
   # Describe exclusions using all women in the approved need group, not just known outcomes.
   groups<-list(Included=d$need&d$common,Excluded=d$need&!d$common)
   overview<-do.call(rbind,lapply(names(groups),function(k){a<-d[groups[[k]],]
    data.frame(group=k,n=nrow(a),survey_weight=sum(a$weight),known_outcomes=sum(is.finite(a$y_modern)),
     modern_use_pct=100*wm(a$y_modern,a$weight),mean_age=30+10*wm(a$age10,a$weight),
     urban_pct=if("urban"%in%names(a))100*wm(a$urban,a$weight)else NA_real_,partnered_pct=100*wm(a$partnered,a$weight))
   }))
   categories<-do.call(rbind,lapply(c("education","fertility_goal"),function(k)do.call(rbind,lapply(names(groups),function(g){
    a<-d[groups[[g]],];v<-as.character(a[[k]]);v[is.na(v)]<-"Unresolved"
    do.call(rbind,lapply(sort(unique(v)),function(level)data.frame(variable=k,group=g,level=level,
     n=sum(v==level),weighted_pct=100*survey_weighted_mean(as.numeric(v==level),a$weight))))
   }))))
   missingness<-do.call(rbind,lapply(c("y_modern",controls,D,"supply_risk"),function(k){
    a<-d[d$need,];data.frame(variable=k,n_missing=sum(is.na(a[[k]])),total=nrow(a),
     weighted_missing_pct=100*survey_weighted_mean(as.numeric(is.na(a[[k]])),a$weight))
   }))
   area_coverage<-do.call(rbind,lapply(sort(unique(d$geo[d$need])),function(g){a<-d[d$need&d$geo==g,]
    data.frame(geo=g,area=a$district[1],need_n=nrow(a),common_n=sum(a$common),excluded_n=sum(!a$common),
     excluded_weighted_pct=100*survey_weighted_mean(as.numeric(!a$common),a$weight),
     broader_base_n=sum(a$base))
   }))
   savecsv(overview,"included_excluded_overview.csv");savecsv(categories,"included_excluded_categories.csv")
   savecsv(missingness,"exclusion_drivers.csv");savecsv(area_coverage,"exclusions_by_area.csv")
   # Identical formulas in the common and model-specific available samples.
   formulas<-c(list(supply=original$supply$formula),setNames(lapply(D,function(k)
    reformulate(c(controls,k,"supply_risk"),"y_modern")),paste0("single_",D)))
   coefficients<-samples<-fits<-list()
   for(id in names(formulas))for(version in c("common","available")){
    formula<-formulas[[id]]
    eligible<-if(version=="common")d$common else d$need&d$positive&complete.cases(d[all.vars(formula)])
    fd<-full_design;fd$variables$.sensitivity_eligible<-eligible[d$positive]
    des<-subset(fd,.sensitivity_eligible);a<-droplevels(d[eligible,])
    fit<-fit_survey_logistic(formula,a,des);co<-cr2_table(fit,a)
    co$model<-id;co$sample<-version;coefficients[[paste(id,version)]]<-co
    samples[[paste(id,version)]]<-data.frame(model=id,sample=version,n=nrow(a),events=sum(a$y_modern),
     areas=length(unique(a$geo)),weighted_modern_use=wm(a$y_modern,a$weight))
    fits[[paste(id,version)]]<-list(fit=fit,data=a,formula=formula)
    if(id=="supply"&&version=="common")stopifnot(max(abs(coef(fit)-coef(original$supply$fit)))<1e-9,
     max(abs(geographic_cr2(fit,a$geo)-original$supply$V))<1e-9)
   }
   coefficients<-do.call(rbind,coefficients);samples<-do.call(rbind,samples)
   savecsv(coefficients,"available_case_coefficients.csv");savecsv(samples,"available_case_samples.csv")
   # Response weights target women with known outcome, controls and area-wide supply.
   base<-droplevels(d[d$base,]);base$responder<-as.numeric(base$common)
   response<-response_probabilities(base,controls);base$response_probability<-response$probability
   base$response_adjustment<-1/base$response_probability
   joint_specs<-lapply(original[c("score_joint","components_joint")],`[[`,"formula")
   fit_joint<-function(a,response_weighted,formula){
    cc<-droplevels(a[a$responder==1,]);if(response_weighted)cc$weight<-cc$weight*cc$response_adjustment
    fit_survey_logistic(formula,cc)
   }
   joint_fits<-list();joint_points<-list();replicates<-list()
   for(id in names(joint_specs))for(version in c("survey_only","response_weighted")){
    key<-paste(id,version);f<-fit_joint(base,version=="response_weighted",joint_specs[[id]])
    joint_fits[[key]]<-f;joint_points[[key]]<-coef(f)
    if(version=="survey_only")stopifnot(max(abs(coef(f)-coef(original[[id]]$fit)))<1e-9)
   }
   # Delete one administrative area, refitting both stages each time.
   # This is geographic error-cluster inference, not an asserted sampling-PSU design.
   areas<-sort(unique(base$geo));G<-length(areas);stopifnot(G==config$areas)
   for(g in areas){
    a<-droplevels(base[base$geo!=g,]);rr<-response_probabilities(a,controls)
    a$response_probability<-rr$probability;a$response_adjustment<-1/rr$probability
    for(id in names(joint_specs))for(version in c("survey_only","response_weighted")){
     key<-paste(id,version);f<-fit_joint(a,version=="response_weighted",joint_specs[[id]])
     stopifnot(identical(names(coef(f)),names(joint_points[[key]])))
     replicates[[paste(key,g)]]<-data.frame(model=id,weighting=version,omitted_area=g,
      term=names(coef(f)),estimate=unname(coef(f)))
    }
    cat("Completed sensitivity jackknife:",g,"\n")
   }
   replicates<-do.call(rbind,replicates);joint<-list()
   for(id in names(joint_specs))for(version in c("survey_only","response_weighted")){
    key<-paste(id,version);b<-joint_points[[key]];r<-replicates[replicates$model==id&replicates$weighting==version,]
    B<-sapply(names(b),function(term)r$estimate[match(paste(areas,term),paste(r$omitted_area,r$term))])
    stopifnot(all(is.finite(B)))
    V<-cluster_jackknife_covariance(B);se<-sqrt(diag(V));df<-G-1
    joint[[key]]<-data.frame(model=id,weighting=version,term=names(b),estimate=unname(b),se=se,df=df,
     p=2*pt(-abs(b/se),df),odds_ratio=exp(b),or_lo=exp(b-qt(.975,df)*se),or_hi=exp(b+qt(.975,df)*se),n=sum(base$responder==1))
   }
   joint<-do.call(rbind,joint);savecsv(joint,"response_weighted_joint_coefficients.csv")
   savecsv(replicates,"response_weighted_area_jackknife.csv")
   # Observed balance against the broader eligible target group.
   balance_matrix<-cbind(modern_use=base$y_modern,age_years=30+10*base$age10,partnered=base$partnered,
    supply_risk=base$supply_risk,model.matrix(~education+fertility_goal+factor(geo)-1,base))
   if("urban"%in%names(base))balance_matrix<-cbind(balance_matrix,urban=base$urban)
   complete<-base$responder==1
   balance<-do.call(rbind,lapply(colnames(balance_matrix),function(k){
    v<-balance_matrix[,k];target<-wm(v,base$weight);sd<-sqrt(wm((v-target)^2,base$weight))
    before<-wm(v[complete],base$weight[complete]);after<-wm(v[complete],base$weight[complete]*base$response_adjustment[complete])
    data.frame(variable=k,target_mean=target,common_mean=before,response_weighted_mean=after,
     standardized_difference_before=if(sd>0)(before-target)/sd else 0,
     standardized_difference_after=if(sd>0)(after-target)/sd else 0)
   }))
   savecsv(balance,"response_weighting_balance.csv")
   adjusted<-base$weight[complete]*base$response_adjustment[complete]
   kish<-function(w)sum(w)^2/sum(w^2)
   diagnostics<-list(target_n=nrow(base),complete_n=sum(complete),areas=G,fully_observed_areas=response$complete_areas,
    response_probability_min=min(base$response_probability),response_probability_max=max(base$response_probability),
    response_adjustment_min=min(base$response_adjustment[complete]),response_adjustment_median=median(base$response_adjustment[complete]),
    response_adjustment_p99=unname(quantile(base$response_adjustment[complete],.99)),response_adjustment_max=max(base$response_adjustment[complete]),
    kish_effective_n_original=kish(base$weight[complete]),kish_effective_n_adjusted=kish(adjusted),
    maximum_absolute_balance_difference_before=max(abs(balance$standardized_difference_before)),
    maximum_absolute_balance_difference_after=max(abs(balance$standardized_difference_after)),
    response_model_formula=paste("Complete demand information ~ modern use +",paste(controls,collapse=" + "),"+ administrative-area indicators; probability 1 in areas fully observed within the eligible target."),
    response_weight_definition="Original SURGO weight / estimated probability of complete demand information. No trimming or imputation.",
    inference=paste0("For response-weighted joint comparisons, delete-one-area jackknife refits the response and outcome models; variance=(G-1)/G times centered replicate crossproducts; t intervals use G-1=",G-1," df. Survey-only joint comparison uses the same jackknife; primary report inference remains CR2/Satterthwaite."))
   audit<-list(country=config$country,country_key=country,supply_resolution=paste(config$area_singular,"only"),area_singular=config$area_singular,area_plural=config$area_plural,appendix_title=config$appendix_title,geographic_clusters=G,need_n=sum(d$need),common_n=sum(d$common),
    excluded_n=sum(d$need&!d$common),base_n=sum(d$base),regression_function="survey::svyglm",
    design_ids="~1",design_strata="None",weights="Original setting-specific SURGO weight; response adjustments only in the named sensitivity fits",
    controls=controls,demand_references_fixed=TRUE,supply_references_fixed=TRUE,
    main_results_unchanged=TRUE,source_files_unchanged=identical(source_hash,unname(tools::md5sum(source_files))),
    uncertainty="Response weighting requires the completeness model to account adequately for selection using observed information. It cannot establish robustness to selection on unrecorded concerns. Jackknife inference uses the available administrative-area clusters and conditions on fixed score references.")
   stopifnot(audit$source_files_unchanged)
   payload<-list(audit=audit,overview=overview,categories=categories,missingness=missingness,area_coverage=area_coverage,
    available_coefficients=coefficients,available_samples=samples,joint_coefficients=joint,weight_diagnostics=diagnostics,balance=balance,
    demand_variables=D,demand_labels=p$demand_labels)
   jsonlite::write_json(payload,file.path(out,"sensitivity_results.json"),auto_unbox=TRUE,pretty=TRUE,dataframe="rows",na="null",digits=12)
   saveRDS(list(data=d,base=base,response=response,available_fits=fits,joint_fits=joint_fits,replicates=replicates),
    file.path(out,"sensitivity_fits.rds"))
   cat("Built",config$country,"administrative-area sensitivity checks with",sum(d$common),"common and",sum(d$base),"broader eligible women.\n")
   invisible(payload)
  }

  if(sys.nframe()==0L){
   args<-commandArgs(TRUE);arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
   build_regression_sensitivity(arg("--data-dir"),arg("--output-dir"),arg("--country"))
  }

}

# Verify regression sensitivity -----------------------------------------------------
.report_steps[["verify_regression_sensitivity.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Independent sample, weighting, coefficient and two-stage jackknife checks.
  args<-commandArgs(TRUE);arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-arg("--data-dir");out<-arg("--sensitivity-dir")
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  p<-jsonlite::fromJSON(file.path(out,"sensitivity_results.json"));ob<-readRDS(file.path(out,"sensitivity_fits.rds"))
  original<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"))
  D<-p$demand_variables;controls<-p$audit$controls
  checks<-list();check<-function(name,ok){if(!isTRUE(ok))stop(name);checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE)}
  same<-function(a,b,tol=1e-8)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=tol))
  check("Original survey variables preserved",all(vapply(names(d),function(k)same(d[[k]],ob$data[[k]],0),logical(1))))
  base<-d$need&d$positive&complete.cases(d[c("y_modern",controls)])&complete.cases(frame$supply)
  check("Broader target independently selected",identical(d$row[base],ob$base$row)&&sum(base)==p$audit$base_n)
  common<-base&complete.cases(d[D]);check("Common model sample independently selected",identical(d$row[common],original$score_joint$data$row)&&sum(common)==p$audit$common_n)
  for(model in unique(p$available_samples$model))for(version in c("common","available")){
   key<-paste(model,version);a<-ob$available_fits[[key]];needed<-all.vars(a$formula)
   ii<-if(version=="common")common else ob$data$need&ob$data$positive&complete.cases(ob$data[needed])
   check(paste(model,version,"exact rows"),identical(a$data$row,d$row[ii]))
   check(paste(model,version,"need and modern outcome"),all(a$data$need)&&all(a$data$y_modern%in%0:1))
   check(paste(model,version,"unchanged survey weights"),same(a$data$weight,d$weight[ii],0))
   check_data<-a$data;check_data$.verification_weight<-check_data$weight/mean(check_data$weight)
   f<-glm(a$formula,data=check_data,weights=.verification_weight,family=quasibinomial(),control=glm.control(maxit=100))
   check(paste(model,version,"independent weighted coefficients"),same(coef(f),coef(a$fit)))
   V<-clubSandwich::vcovCR(f,cluster=a$data$geo,type="CR2",inverse_var=FALSE)
   ct<-as.data.frame(clubSandwich::coef_test(f,vcov=V,test="Satterthwaite"))
   co<-p$available_coefficients[p$available_coefficients$model==model&p$available_coefficients$sample==version,]
   co<-co[match(rownames(ct),co$term),]
   check(paste(model,version,"Administrative-area CR2 Satterthwaite intervals"),same(co$or_lo,exp(ct$beta-qt(.975,ct$df_Satt)*ct$SE))&&same(co$or_hi,exp(ct$beta+qt(.975,ct$df_Satt)*ct$SE)))
  }
  for(group in c("Included","Excluded")){
   ii<-if(group=="Included")common else d$need&!common;r<-p$overview[p$overview$group==group,]
   known<-ii&is.finite(d$y_modern)
   check(paste(group,"weighted modern use and known denominator"),same(r$modern_use_pct,100*weighted.mean(d$y_modern[known],d$weight[known]))&&r$known_outcomes==sum(known))
   check(paste(group,"age units independently recovered"),same(r$mean_age,weighted.mean(30+10*d$age10[ii],d$weight[ii],na.rm=TRUE)))
  }
  # Re-estimate completeness probabilities with an independent ordinary weighted GLM.
  response_probability<-function(a){
   sums<-tapply(a$responder,a$geo,sum);counts<-table(a$geo)
   complete_areas<-names(sums)[sums==as.numeric(counts[names(sums)])]
   ii<-!a$geo%in%complete_areas;probability<-rep(1,nrow(a))
   if(any(ii)){
    b<-droplevels(a[ii,]);b$response_area<-factor(b$geo)
    b$.verification_weight<-b$weight/mean(b$weight)
    f<-glm(reformulate(c("y_modern",controls,"response_area"),"responder"),data=b,
     weights=.verification_weight,family=quasibinomial(),control=glm.control(maxit=100))
    probability[ii]<-predict(f,type="response")
   }
   probability
  }
  b<-ob$base;probability<-response_probability(b)
  check("Independent response probabilities",same(probability,b$response_probability))
  check("Positive response probabilities and inverse adjustments",all(probability>0&probability<=1)&&same(b$response_adjustment,1/probability))
  fully_observed<-names(which(tapply(b$responder,b$geo,function(x)all(x==1))))
  check("Fully observed areas require no response adjustment",setequal(p$weight_diagnostics$fully_observed_areas,fully_observed)&&all(b$response_adjustment[b$geo%in%fully_observed]==1))
  independent_joint<-function(a,weighted,formula){
   probability<-response_probability(a);ii<-a$responder==1;w<-a$weight[ii]
   if(weighted)w<-w/probability[ii]
   check_data<-a[ii,];check_data$.verification_weight<-w/mean(w)
   glm(formula,data=check_data,weights=.verification_weight,family=quasibinomial(),control=glm.control(maxit=100))
  }
  for(model in c("score_joint","components_joint"))for(version in c("survey_only","response_weighted")){
   fo<-original[[model]]$formula;key<-paste(model,version)
   f<-independent_joint(b,version=="response_weighted",fo)
   check(paste(key,"independent full-fit coefficients"),same(coef(f),coef(ob$joint_fits[[key]])))
   areas<-sort(unique(b$geo));B<-matrix(NA_real_,length(areas),length(coef(f)),dimnames=list(areas,names(coef(f))))
   for(g in areas){
    a<-droplevels(b[b$geo!=g,]);rep<-independent_joint(a,version=="response_weighted",fo)
    B[g,]<-coef(rep);saved<-ob$replicates[ob$replicates$model==model&ob$replicates$weighting==version&ob$replicates$omitted_area==g,]
    check(paste(key,g,"refit both stages"),same(coef(rep),saved$estimate[match(names(coef(rep)),saved$term)]))
   }
   V<-matrix(0,ncol(B),ncol(B));mu<-colMeans(B)
   for(j in seq_len(nrow(B)))V<-V+tcrossprod(B[j,]-mu)
   V<-V*(length(areas)-1)/length(areas);se<-sqrt(diag(V))
   co<-p$joint_coefficients[p$joint_coefficients$model==model&p$joint_coefficients$weighting==version,]
   co<-co[match(names(coef(f)),co$term),]
   check(paste(key,"independent jackknife variance"),same(co$se,se))
   df<-length(areas)-1
   check(paste(key,"area-jackknife t intervals"),all(co$df==df)&&same(co$or_lo,exp(coef(f)-qt(.975,df)*se))&&same(co$or_hi,exp(coef(f)+qt(.975,df)*se)))
  }
  check("Score units fixed to main models",same(ob$data$demand_score[common],original$score_joint$data$demand_score)&&same(ob$data$supply_risk[common],original$score_joint$data$supply_risk))
  check("Whole-area supply assignment retained",all(vapply(split(ob$data$supply_risk,ob$data$geo),function(x)length(unique(x))==1,logical(1))))
  result<-list(all_passed=TRUE,check_count=length(checks),country=p$audit$country,supply_resolution=p$audit$supply_resolution,
   need_n=sum(d$need),main_n=sum(common),supply_available_n=sum(base),primary_results_unchanged=TRUE,
   limitations="Implementation checks do not validate the untestable response-selection assumption or reconstruct the original sampling design.")
  write.csv(do.call(rbind,checks),file.path(out,"verification_checks.csv"),row.names=FALSE)
  jsonlite::write_json(result,file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
  cat("Passed",length(checks),"independent regression-sensitivity checks.\n")

}

# Plot regression sensitivity -----------------------------------------------------
.report_steps[["plot_regression_sensitivity.R"]] <- function(.step_args) {
  .step_environment<-environment()
  sys.nframe<-function()0L
  commandArgs<-function(trailingOnly=FALSE)if(trailingOnly).step_args else c("Rscript",paste0("--file=",.report_file))
  source<-function(file,local=FALSE,...) .report_source(file,if(is.environment(local))local else if(isTRUE(local))parent.frame()else .step_environment)
  # Plot aggregate coefficient comparisons using the existing report palette.
  args<-commandArgs(TRUE);arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  out<-arg("--data-dir");p<-jsonlite::fromJSON(file.path(out,"sensitivity_results.json"))
  D<-p$demand_variables;labels<-setNames(p$demand_labels,D)
  labels<-c(labels,supply_risk="Supply risk score",demand_score="Demand risk score")
  wrap<-function(x)vapply(x,function(v)paste(strwrap(v,31),collapse="\n"),character(1))
  library(ggplot2)
  theme_report<-theme_minimal(base_size=12,base_family="Arial")+
   theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
    axis.title.y=element_blank(),axis.text=element_text(color="#244657"),
    plot.title=element_text(face="bold",color="#17374a",size=16),
    legend.position="top",legend.title=element_blank(),
    strip.text.y=element_text(face="bold",color="#17374a"),
    plot.margin=margin(12,18,12,12))
  save_plot<-function(plot,name,height){
   file<-file.path(out,name);ggsave(file,plot,width=10.2,height=height,device=svglite::svglite,bg="white")
   ggsave(sub("[.]svg$",".png",file),plot,width=10.2,height=height,dpi=144,bg="white")
   text<-readLines(file,warn=FALSE)
   text<-text[!grepl("<!--.*Created with",text)]
   writeLines(text,file,useBytes=TRUE)
  }
  a<-p$available_coefficients
  a<-a[(a$model=="supply"&a$term=="supply_risk")|
   (startsWith(a$model,"single_")&a$term==sub("^single_","",a$model)),]
  a$component<-factor(a$term,levels=rev(c("supply_risk",D)))
  a$sample<-factor(a$sample,levels=c("common","available"),labels=c("Common sample","Available cases"))
  pos<-position_dodge(width=.58)
  g<-ggplot(a,aes(x=odds_ratio,y=component,color=sample))+
   geom_vline(xintercept=1,linetype="dashed",color="#aabcc6",linewidth=.5)+
   geom_errorbar(aes(xmin=or_lo,xmax=or_hi),orientation="y",width=.17,position=pos,linewidth=.6)+
   geom_point(position=pos,size=2.7)+scale_x_log10()+
   scale_y_discrete(labels=function(x)wrap(labels[x]))+
   scale_color_manual(values=c("Common sample"="#74838d","Available cases"="#047f8b"))+
   labs(title="Adjusted associations in common and broader eligible samples",x="Adjusted odds ratio · 95% confidence interval")+theme_report
  save_plot(g,"available_case_comparison.svg",6.3)
  b<-p$joint_coefficients
  b<-b[(b$model=="score_joint"&b$term%in%c("demand_score","supply_risk"))|
   (b$model=="components_joint"&b$term%in%c(D,"supply_risk")),]
  b$key<-paste(b$model,b$term)
  keys<-c(paste("score_joint",c("demand_score","supply_risk")),paste("components_joint",c(D,"supply_risk")))
  b$key<-factor(b$key,levels=rev(keys))
  key_labels<-setNames(labels[sub("^.* ","",keys)],keys)
  b$model_label<-factor(b$model,levels=c("score_joint","components_joint"),
   labels=c("Demand + supply\nscores","Six demand indicators\n+ supply score"))
  b$weighting<-factor(b$weighting,levels=c("survey_only","response_weighted"),
   labels=c("Survey weights","Survey + response weights"))
  g<-ggplot(b,aes(x=odds_ratio,y=key,color=weighting))+
   geom_vline(xintercept=1,linetype="dashed",color="#aabcc6",linewidth=.5)+
   geom_errorbar(aes(xmin=or_lo,xmax=or_hi),orientation="y",width=.17,position=pos,linewidth=.6)+
   geom_point(position=pos,size=2.7)+scale_x_log10()+
   scale_y_discrete(labels=function(x)wrap(key_labels[x]))+
   scale_color_manual(values=c("Survey weights"="#74838d","Survey + response weights"="#047f8b"))+
   facet_grid(model_label~.,scales="free_y",space="free_y")+
   labs(title="Joint associations with original and response-adjusted weights",x="Adjusted odds ratio · 95% area-jackknife confidence interval")+
   theme_report+theme(strip.text.y=element_text(angle=0,size=10),strip.placement="outside")
  save_plot(g,"response_weighting_comparison.svg",7.7)
  cat("Saved two aggregate sensitivity coefficient figures.\n")

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
  paths<-c("render_analysis.py"="support/renderers/aggregate/sindh_render.py",
           "verify_html.py"="support/renderers/aggregate/sindh_verify.py",
           "html_fragment.py"="support/renderers/html_fragment.py",
           "render_regression_sensitivity.py"="support/renderers/render_regression_sensitivity.py")
  if(!key %in% names(paths))stop("Unknown rendering dependency: ",key)
  run_python(.report_root,paths[[key]],args,python)
}

# End-to-end sequence -----------------------------------------------------------
build_sindh_aggregate <- function(root, join_dir, cache_dir, work_dir,
                                          python="python3", input_zip="") {
  root<-normalizePath(root,mustWork=TRUE)

  use_project_library(root)
  context<-prepare_aggregate_inputs(root,input_zip,work_dir,cache_dir,join_dir)
  module<-dirname(.report_file)
  work_dir<-project_path(work_dir,root)
  data<-file.path(work_dir,"Sindh_analysis_files")
  prep<-file.path(work_dir,"inputs");dir.create(data,recursive=TRUE,showWarnings=FALSE)
  stage<-function(name,args)run_stage(file.path(module,name),args,python)
  original<-file.path(context$join_dir,"checked_geography/sindh_women_manitoba.rds")
  fac<-file.path(context$join_dir,"audit/facility_linkage_inputs.rds")
  raw<-file.path(context$cache_dir,"inputs/surgo/sindh/20250124_forms_wt.dta")
  raw_fac<-file.path(context$cache_dir,"inputs/uom/sindh/uom_sindh_facility_pooled.dta")
  stage("prepare_fp_provider_data.R",c("--input",original,"--facility-input",fac,
    "--raw-facility",raw_fac,"--raw-women",raw,"--dictionary",
    file.path(context$cache_dir,"exports/merged_supply_demand_dictionary.csv"),"--output-dir",prep))
  need_file<-file.path(context$cache_dir,"surgo_pipeline/sindh/sindh_wra_design_ready.rds")
  inputs<-c("--input",file.path(prep,"sindh_women_manitoba_fp_providers.rds"),
    "--facility-input",fac,"--raw-facility",raw_fac,"--need-file",need_file)
  stage("build_analysis.R",c(inputs,"--output-dir",data))
  stage("fit_models.R",c("--data-dir",data,"--output-dir",file.path(data,"models_zscore")))
  stage("prepare_map.R",c("--boundary",file.path(root,"support/assets/boundaries/sindh_districts.geojson"),"--data-dir",data))
  sensitivity<-file.path(data,"regression_sensitivity")
  sensitivity_stage<-function(name,args)run_stage(file.path(root,"administrative_area/analysis",name),args,python)
  sensitivity_stage("regression_sensitivity.R",c("--country","sindh","--data-dir",data,"--output-dir",sensitivity))
  sensitivity_stage("verify_regression_sensitivity.R",c("--data-dir",data,"--sensitivity-dir",sensitivity))
  sensitivity_stage("plot_regression_sensitivity.R",c("--data-dir",sensitivity))
  stage("verify_analysis.R",c(inputs,"--raw-women",raw,"--district-aliases",file.path(context$cache_dir,"lookups/sindh_district_aliases.csv"),"--cleaned-facility",
    file.path(context$cache_dir,"uom_pipeline/uom_summary_facility.csv"),"--data-dir",data))
  stage("save_updated_dataset.R",c("--input",file.path(prep,"sindh_women_manitoba_fp_providers.rds"),"--data-dir",data,"--output-dir",prep))
  native<-file.path(work_dir,"Sindh_analysis.html")
  stage("render_analysis.py",c("--data-dir",data,"--output",native))
  # Sensitivity analyses: alternative scores and final-composite SD maps.
  render_score_threshold_appendix(root,"sindh",data,native,python,"administrative_area")
  sensitivity_stage("render_regression_sensitivity.py",c("--data-dir",sensitivity,"--html",native))
  stage("verify_html.py",c("--data-dir",data,"--html",native))
  fragment<-file.path(work_dir,"report_fragment.html")
  run_stage(file.path(root,"analysis/common/html_fragment.py"),c("--input",native,"--output",fragment),python)
  list(native=native,fragment=fragment,data_dir=data)
}


if(sys.nframe()==0L) {
  if("--help" %in% commandArgs(TRUE)) {
    cat("Run: Rscript aggregate/sindh/Sindh_analysis.R --input-zip FILE\n",
        "Optional: --work-dir DIR --cache-dir DIR --join-dir VERIFIED_JOIN --output-dir DIR --python EXE --pandoc-dir DIR\n",sep="")
    quit(status=0)
  }
  o<-report_arguments("output/aggregate/sindh","aggregate/sindh")
  if(nzchar(o$pandoc_dir))Sys.setenv(RSTUDIO_PANDOC=o$pandoc_dir)
  r<-build_sindh_aggregate(.report_root,o$join_dir,o$cache_dir,o$work_dir,o$python,o$input_zip)
  output<-project_path(o$output_dir,.report_root);dir.create(output,recursive=TRUE,showWarnings=FALSE)
  target<-rmarkdown::render(file.path(dirname(.report_file),"Sindh_analysis.Rmd"),output_file="Sindh_analysis.html",
    output_dir=output,params=list(prepared_fragment=r$fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
  run_stage("verify_html.py",c("--data-dir",r$data_dir,"--html",target),o$python)
  message("Completed Sindh_analysis.html")
}
