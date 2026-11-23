#!/usr/bin/env Rscript
# Rebuild supplementary coverage, component-profile and awareness checks after the seven reports.
# Run: Rscript data_checks/Build_data_checks.R --work-dir WORK --cache-dir CACHE --output-dir DIR
# WORK must contain the country calculations made by run_all.R with the same input ZIP.
# Packages: survey, clubSandwich, haven, dplyr, readr, readxl, ggplot2, svglite,
# rmarkdown, knitr, base64enc, jsonlite, digest. Exact inputs: docs/DATASET_FILENAMES.md.
.entry<-grep("^--file=",commandArgs(),value=TRUE)
.source_files<-vapply(sys.frames(),function(frame){file<-frame$ofile;if(is.null(file))""else as.character(file)[1]},character(1))
.source_files<-.source_files[nzchar(.source_files)]
.report_file<-if(length(.source_files))tail(.source_files,1)else gsub("~+~"," ",sub("^--file=","",.entry[1]),fixed=TRUE)
.report_file<-normalizePath(.report_file,mustWork=TRUE);.report_root<-dirname(dirname(.report_file))
source(file.path(.report_root,"support/runtime.R"));use_project_library(.report_root)
.diag_codes<-list()

# Surgo UoM checks
.diag_codes[["Surgo_UoM_checks.R"]] <- quote({
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Rebuild aggregate coverage checks from caller-supplied analysis joins and surveys.
  coverage_key <- function(v) {
    v <- iconv(as.character(v), to = "ASCII//TRANSLIT")
    v <- toupper(gsub("[^A-Za-z0-9]", "", v))
    v[is.na(v) | !nzchar(v)] <- "UNRECORDED"
    v[v == "IFAKOIJAIYE"] <- "IFAKOIJAYE"
    v
  }

  coverage_name <- function(g, label, country) {
    if (g == "UNRECORDED") return("Geography not recorded")
    if (country == "lagos") return(label)
    special <- c(KEDOUGOU="Kédougou", LINGUERE="Linguère", MBACKE="Mbacké",
      SEDHIOU="Sédhiou", THIES="Thiès", SAINTLOUIS="Saint-Louis", GUEDIAWAYE="Guédiawaye")
    if (country == "senegal" && g %in% names(special)) return(unname(special[g]))
    tools::toTitleCase(tolower(label))
  }

  prepare_country_coverage <- function(root, country, input, facility_input, raw_facility,
                                       work_dir, residence_lookup="", need_file="") {
    source(file.path(root,"support/runtime.R"),local=TRUE)
    nv <- function(v) as.numeric(haven::zap_labels(v))
    inputs <- c(input, facility_input, raw_facility,
                if(country=="senegal")residence_lookup,
                if(country=="sindh")need_file)
    if (!all(nzchar(inputs))) stop("Supply every required input for ",country)
    inputs <- vapply(inputs,project_path,character(1),root=root)
    stopifnot(all(file.exists(inputs))); hashes <- unname(tools::md5sum(inputs))
    x <- readRDS(inputs[1]); f <- readRDS(inputs[2]); f <- f[f$country==country,]
    has_residence <- country!="sindh"
    weight_var <- switch(country,lagos="weight",sindh="Normalizedweights","WEIGHT")
    consent_var <- if(country%in%c("lagos","sindh"))"irb1"else"IRB1"
    need_var <- if(country=="lagos")"fpn2"else"FPN2"
    stopifnot(all(c("link_geo_join",weight_var,consent_var,"link_has_supply")%in%names(x)),
              !anyDuplicated(f$fid))
    if(has_residence)stopifnot(all(c("link_rurb","link_has_exact_fp_supply","link_supply_fallback")%in%names(x)))
    if(country=="sindh") {
      ref <- readRDS(inputs[4]); stopifnot(!anyDuplicated(x$col_id),!anyDuplicated(ref$col_id),"fpn2_derived"%in%names(ref))
      ri <- match(x$col_id,ref$col_id)
      need_code <- nv(ref$fpn2_derived[ri]); need_var <- "fpn2_derived"
    } else {stopifnot(need_var%in%names(x));need_code<-nv(x[[need_var]])}
    raw_fields <- switch(country,lagos=c("key","q506","location"),
      sindh=c("key","facility_id","q506"),senegal=c("fid","s5_6","s1_3"),
      cotedivoire=c("fid","s2_5_sp","s1_5"))
    raw <- haven::read_dta(inputs[3],col_select=tidyselect::all_of(raw_fields))
    raw_id <- as.character(raw[[if(country%in%c("lagos","sindh"))"key"else"fid"]])
    if(country=="sindh") {
      blank<-is.na(raw_id)|trimws(raw_id)=="";raw_id[blank]<-as.character(raw$facility_id[blank])
    }
    stopifnot(!anyDuplicated(raw_id));fi<-match(as.character(f$fid),raw_id)
    stopifnot(nrow(f)==nrow(raw),!anyNA(fi),!anyDuplicated(fi))
    fp_var <- switch(country,senegal="s5_6",cotedivoire="s2_5_sp","q506")
    f$fp <- nv(raw[[fp_var]][fi])%in%1
    f$geo <- coverage_key(f$assigned_join_area)
    f$residence <- as.character(f$rurb)
    if(country=="lagos") raw_residence<-as.character(haven::as_factor(raw$location[fi]))
    if(country=="cotedivoire")raw_residence<-ifelse(nv(raw$s1_5[fi])%in%1,"Rural",ifelse(nv(raw$s1_5[fi])%in%2,"Urban",NA_character_))
    if(country=="senegal") {
      lut<-readxl::read_xlsx(inputs[4],range="A1:B79")
      stopifnot(!anyDuplicated(coverage_key(lut$s1_3)))
      li<-match(coverage_key(as.character(haven::as_factor(raw$s1_3[fi]))),coverage_key(lut$s1_3))
      rv<-nv(lut$urbanrural[li]);raw_residence<-ifelse(rv%in%1,"Rural",ifelse(rv%in%2,"Urban",NA_character_))
    }
    if(has_residence)stopifnot(identical(tolower(raw_residence),tolower(f$residence)))
    geo<-coverage_key(x$link_geo_join)
    residence<-if(has_residence)as.character(x$link_rurb)else rep(NA_character_,nrow(x))
    if(has_residence)stopifnot(all(residence[!is.na(residence)]%in%c("Rural","Urban")))
    w<-nv(x[[weight_var]]);eligible<-is.finite(w)&w>0&nv(x[[consent_var]])%in%1
    stopifnot(all(need_code[eligible]%in%c(0,1,95,97,99)|is.na(need_code[eligible])))
    if(country=="sindh")stopifnot(!anyNA(ri[eligible]),all(need_code[eligible]%in%0:1))
    all_geos<-sort(unique(c(geo,f$geo)))
    all_fp<-setNames(vapply(all_geos,function(g)sum(f$fp&f$geo==g),integer(1)),all_geos)
    local_fp<-function(g,r)sum(f$fp&f$geo==g&f$residence%in%r)
    exact<-if(has_residence)vapply(seq_len(nrow(x)),function(i) !is.na(residence[i])&&local_fp(geo[i],residence[i])>0,logical(1))else unname(all_fp[geo]>0)
    fallback<-if(has_residence)!exact&!is.na(residence)&geo!="UNRECORDED"&all_fp[geo]>0 else rep(FALSE,nrow(x))
    unmatched<-!exact&!fallback
    stopifnot(identical(unname(!unmatched),as.logical(x$link_has_supply)))
    if(has_residence)stopifnot(identical(unname(exact),as.logical(x$link_has_exact_fp_supply)),identical(unname(fallback),as.logical(x$link_supply_fallback)))
    share<-function(selected,denominator) {
      den<-denominator&eligible
      if(!any(den))return(NA_real_)
      100*survey_weighted_mean(as.numeric(selected[den]),w[den])
    }
    work<-project_path(work_dir,root);dir.create(work,recursive=TRUE,showWarnings=FALSE)
    population_frames<-list()
    for(population in c("all_women","women_in_need")) {
      pop<-if(population=="all_women")rep(TRUE,nrow(x))else need_code%in%1
      out<-file.path(work,population);dir.create(out,recursive=TRUE,showWarnings=FALSE)
      rows<-lapply(all_geos,function(g){
        selected<-pop&geo==g
        label<-if(g%in%f$geo)as.character(f$assigned_join_area[match(g,f$geo)])else as.character(x$link_geo_join[match(g,geo)])
        a<-data.frame(geo=g,area=coverage_name(g,label,country),total_n=sum(selected),total_pct=share(selected,pop),
          total_fp_n=unname(all_fp[g]),source_facilities=sum(f$geo==g),
          unknown_residence_n=if(has_residence)sum(selected&is.na(residence))else 0L,
          unknown_residence_fp_n=if(has_residence)sum(f$fp&f$geo==g&is.na(f$residence))else 0L,stringsAsFactors=FALSE)
        a$total_status<-if(!a$total_n)"no_women"else if(a$total_fp_n>0)"exact"else"unmatched"
        if(has_residence)for(r in c("Rural","Urban")) {
          p<-tolower(r);mask<-selected&residence%in%r;n<-sum(mask);nfp<-local_fp(g,r)
          a[[paste0(p,"_n")]]<-n;a[[paste0(p,"_pct")]]<-share(mask,pop&residence%in%r)
          a[[paste0(p,"_fp_n")]]<-nfp
          a[[paste0(p,"_status")]]<-if(!n)"no_women"else if(nfp>0)"exact"else if(all_fp[g]>0)"fallback"else"unmatched"
          a[[paste0(p,"_assigned_fp_n")]]<-if(!n)0L else if(nfp>0)nfp else unname(all_fp[g])
        }
        a
      })
      coverage<-do.call(rbind,rows);coverage<-coverage[order(coverage$area),]
      stopifnot(sum(coverage$total_n)==sum(pop),sum(coverage$total_fp_n)==sum(f$fp),abs(sum(coverage$total_pct,na.rm=TRUE)-100)<1e-8)
      if(has_residence) {
        stopifnot(all(coverage$rural_n+coverage$urban_n+coverage$unknown_residence_n==coverage$total_n),
          all(coverage$rural_fp_n+coverage$urban_fp_n+coverage$unknown_residence_fp_n==coverage$total_fp_n))
        for(p in c("rural","urban"))stopifnot(abs(sum(coverage[[paste0(p,"_pct")]],na.rm=TRUE)-100)<1e-8)
      }
      summary<-do.call(rbind,lapply(c("exact","fallback","unmatched"),function(s){
        mask<-pop&switch(s,exact=exact,fallback=fallback,unmatched=unmatched)
        data.frame(status=s,women_n=sum(mask),sample_pct=100*sum(mask)/sum(pop),weighted_pct=share(mask,pop))
      }))
      exceptions<-data.frame(area=character(),residence=character(),source_facilities=integer(),local_fp_n=integer(),assigned_fp_n=integer(),women_n=integer(),sample_pct=numeric(),weighted_pct=numeric())
      if(has_residence)for(g in all_geos)for(r in c("Rural","Urban")) {
        mask<-pop&geo==g&residence%in%r&fallback
        if(any(mask))exceptions<-rbind(exceptions,data.frame(area=coverage$area[match(g,coverage$geo)],residence=r,
          source_facilities=sum(f$geo==g&f$residence%in%r),local_fp_n=local_fp(g,r),assigned_fp_n=unname(all_fp[g]),
          women_n=sum(mask),sample_pct=100*sum(mask)/sum(pop),weighted_pct=share(mask,pop)))
      }
      meta<-list(country=country,population=population,need_variable=need_var,has_residence=has_residence,
        women_n=sum(pop),source_women_n=nrow(x),valid_weighted_n=sum(pop&eligible),excluded_weighted_n=sum(pop&!eligible),
        surveyed_areas=length(unique(geo[pop&geo!="UNRECORDED"])),all_areas=sum(all_geos!="UNRECORDED"),
        facility_only_areas=sum(coverage$total_n==0&coverage$source_facilities>0),
        source_facilities=nrow(f),fp_facilities=sum(f$fp),linked_fp_facilities=sum(f$fp&f$geo%in%geo[pop]),
        rural_women_n=if(has_residence)sum(pop&residence%in%"Rural")else NA_integer_,
        urban_women_n=if(has_residence)sum(pop&residence%in%"Urban")else NA_integer_,
        unknown_residence_women_n=if(has_residence)sum(pop&is.na(residence))else NA_integer_,
        rural_fp_facilities=sum(f$fp&f$residence%in%"Rural"),urban_fp_facilities=sum(f$fp&f$residence%in%"Urban"),
        unknown_residence_fp_facilities=sum(f$fp&is.na(f$residence)),
        rural_weighted_share=if(has_residence)share(pop&residence%in%"Rural",pop)else NA_real_,
        urban_weighted_share=if(has_residence)share(pop&residence%in%"Urban",pop)else NA_real_,
        weight_variable=weight_var,fp_variable=fp_var,source_files_unchanged=identical(hashes,unname(tools::md5sum(inputs))))
      named_area<-coverage$geo!="UNRECORDED"
      demand_present<-coverage$total_n>0
      facility_records_present<-coverage$source_facilities>0
      fp_providers_present<-coverage$total_fp_n>0
      meta$facility_record_areas<-sum(named_area&facility_records_present)
      meta$fp_provider_areas<-sum(named_area&fp_providers_present)
      meta$both_data_areas<-sum(named_area&demand_present&fp_providers_present)
      meta$demand_only_areas<-sum(named_area&demand_present&!fp_providers_present)
      meta$supply_only_areas<-sum(named_area&!demand_present&fp_providers_present)
      meta$unknown_geography_women_n<-sum(pop&geo=="UNRECORDED")
      stopifnot(meta$both_data_areas+meta$demand_only_areas==meta$surveyed_areas,
        meta$both_data_areas+meta$supply_only_areas==meta$fp_provider_areas)
      stopifnot(sum(summary$women_n)==sum(pop),abs(sum(summary$weighted_pct)-100)<1e-8,meta$source_files_unchanged)
      write.csv(coverage,file.path(out,"women_and_facility_coverage.csv"),row.names=FALSE,na="")
      write.csv(summary,file.path(out,"assignment_summary.csv"),row.names=FALSE,na="")
      write.csv(exceptions,file.path(out,"fallback_exceptions.csv"),row.names=FALSE,na="")
      jsonlite::write_json(meta,file.path(out,"coverage_metadata.json"),auto_unbox=TRUE,pretty=TRUE,na="null",digits=12)
      population_frames[[population]]<-coverage
    }
    a<-population_frames$all_women;b<-population_frames$women_in_need
    stopifnot(identical(a$geo,b$geo),identical(a$total_fp_n,b$total_fp_n),all(b$total_n<=a$total_n),identical(hashes,unname(tools::md5sum(inputs))))
    if(country%in%c("cotedivoire","senegal")) {
      stopifnot(all(c("geo_changed","geo_join_pipeline","geo_join_audited","corrected_source_geo_name")%in%names(f)))
      restored<-f$geo_changed%in%TRUE&is.na(f$geo_join_pipeline)&!is.na(f$geo_join_audited)
      restored_geos<-sort(unique(f$geo[restored]))
      restored_table<-do.call(rbind,lapply(restored_geos,function(g) {
        ai<-match(g,a$geo);bi<-match(g,b$geo);ff<-f[restored&f$geo==g,]
        stopifnot(!is.na(ai),!is.na(bi),a$total_n[ai]>0)
        district_keys<-sort(unique(as.character(ff$corrected_source_geo_name)))
        district_counts<-vapply(district_keys,function(v)sum(coverage_key(f$corrected_source_geo_name)==coverage_key(v)),integer(1))
        district_qualified<-vapply(district_keys,function(v)any(ff$geo_review_required[coverage_key(ff$corrected_source_geo_name)==coverage_key(v)]%in%TRUE),logical(1))
        district_names<-vapply(district_keys,function(v)if(coverage_key(v)=="TIASSALE")"Tiassalé"else tools::toTitleCase(tolower(v)),character(1))
        stopifnot(sum(district_counts)==nrow(ff))
        data.frame(geo=g,departement=if(g=="TIASSALE")"Tiassalé"else a$area[ai],
          health_district=paste(district_names,collapse="; "),health_district_count=length(district_names),
          health_district_with_counts=paste(paste0(district_names," (",district_counts,")",ifelse(district_qualified,"*","")),collapse="; "),
          qualified_districts=paste(district_names[district_qualified],collapse="; "),
          jc_departement_profile_present=any(coverage_key(f$geo_join_pipeline)==g),
          facility_records_recovered=nrow(ff),
          all_women_n=a$total_n[ai],all_women_weighted_pct=a$total_pct[ai],
          need_women_n=b$total_n[bi],need_women_weighted_pct=b$total_pct[bi],stringsAsFactors=FALSE)
      }))
      stopifnot(sum(restored_table$facility_records_recovered)==sum(restored))
      write.csv(restored_table,file.path(work,"restored_geographic_matches.csv"),row.names=FALSE,na="")
      if(country=="senegal") {
        moved<-!is.na(f$geo_join_pipeline)&coverage_key(f$geo_join_pipeline)!=f$geo
        revisions<-unique(f[moved,c("corrected_source_geo_name","geo_join_pipeline","assigned_join_area")])
        revisions$facility_records<-vapply(seq_len(nrow(revisions)),function(i)sum(moved&
          f$corrected_source_geo_name==revisions$corrected_source_geo_name[i]&
          f$geo_join_pipeline%in%revisions$geo_join_pipeline[i]&f$assigned_join_area==revisions$assigned_join_area[i]),integer(1))
        revisions$current_source_women<-vapply(revisions$assigned_join_area,function(g)sum(geo==coverage_key(g)),integer(1))
        stopifnot(sum(revisions$facility_records)==sum(moved))
        write.csv(revisions,file.path(work,"other_geographic_revisions.csv"),row.names=FALSE,na="")
      }
    }
    cat(country,":",nrow(x),"source women;",sum(need_code%in%1),"women in need;",sum(f$fp),"FP providers; assignment flags reconciled.\n")
    invisible(work)
  }

  prepare_surgo_uom_checks <- function(root, inputs, facility_input, work_dir) {
    for(p in c("haven","jsonlite","readxl"))if(!requireNamespace(p,quietly=TRUE))stop("Install ",p)
    source(file.path(root,"support/runtime.R"),local=TRUE)
    work<-project_path(work_dir,root)
    for(country in c("cotedivoire","lagos","senegal","sindh")) {
      z<-inputs[[country]]
      prepare_country_coverage(root,country,z$input,facility_input,z$raw_facility,file.path(work,country),
        if(country=="senegal")z$residence_lookup else "",if(country=="sindh")z$need_file else "")
    }
    normalizePath(work,mustWork=TRUE)
  }

  if(sys.nframe()==0L) {
    args<-commandArgs(trailingOnly=TRUE)
    arg<-function(k,default=""){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
    if("--help"%in%args) {
      cat("Rscript analysis/Surgo_UoM_checks.R --facility-input CROSSWALKED_FACILITIES --lagos-input JOIN --lagos-raw-facility SURVEY --cotedivoire-input JOIN --cotedivoire-raw-facility SURVEY --senegal-input JOIN --senegal-raw-facility SURVEY --senegal-residence-lookup LOOKUP --sindh-input JOIN --sindh-raw-facility SURVEY --sindh-need-file NEED [--work-dir output/surgo_uom_checks] [--output-dir data_checks] [--pandoc-dir DIRECTORY]\n")
      quit(status=0)
    }
    script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
    root<-normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
    source(file.path(root,"support/runtime.R"));use_project_library(root)
    inputs<-setNames(lapply(c("cotedivoire","lagos","senegal","sindh"),function(k)list(
      input=arg(paste0("--",k,"-input"),Sys.getenv(paste0("SURGO_MANITOBA_",toupper(k),"_WOMEN_JOIN"),"")),
      raw_facility=arg(paste0("--",k,"-raw-facility"),Sys.getenv(paste0("SURGO_MANITOBA_",toupper(k),"_RAW_FACILITY"),"")),
      residence_lookup=if(k=="senegal")arg("--senegal-residence-lookup",Sys.getenv("SURGO_MANITOBA_SENEGAL_RESIDENCE_LOOKUP",""))else"",
      need_file=if(k=="sindh")arg("--sindh-need-file",Sys.getenv("SURGO_MANITOBA_SINDH_NEED_FILE",""))else"")),c("cotedivoire","lagos","senegal","sindh"))
    work<-prepare_surgo_uom_checks(root,inputs,arg("--facility-input",arg("--lagos-facility-input",Sys.getenv("SURGO_MANITOBA_FACILITY_LINKAGE_INPUT",""))),arg("--work-dir","output/surgo_uom_checks"))
    pandoc<-arg("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
    output<-project_path(arg("--output-dir","data_checks"),root);dir.create(output,recursive=TRUE,showWarnings=FALSE)
    rmarkdown::render(file.path(root,"data_checks/Surgo_UoM_checks.Rmd"),output_file="Surgo_UoM_checks.html",output_dir=normalizePath(output),params=list(prepared_dir=work),quiet=TRUE,envir=new.env(parent=globalenv()))
    cat("Saved Surgo_UoM_checks.html\n")
  }

})

# build data checks
.diag_codes[["build_data_checks.R"]] <- quote({
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Recalculate the cumulative checks from the input bundle and current report frames.
  args <- commandArgs(trailingOnly=TRUE)
  arg <- function(k) {i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  if("--help"%in%args) {
   cat("Rscript analysis/build_data_checks.R --cache-dir CACHE --facility-input FACILITIES --lagos-dir ANALYSIS --senegal-dir ANALYSIS --cotedivoire-dir ANALYSIS --sindh-dir ANALYSIS --output-json JSON\n")
   quit(status=0)
  }
  nv <- function(x) as.numeric(haven::zap_labels(x))
  countries <- c("lagos","senegal","sindh","cotedivoire")
  cache <- arg("--cache-dir")
  fac <- readRDS(arg("--facility-input"))
  frames <- setNames(lapply(countries,function(k)readRDS(file.path(arg(paste0("--",k,"-dir")),"analysis_frame.rds"))),countries)
  num <- function(x) format(x,big.mark=",",scientific=FALSE,trim=TRUE)
  pct <- function(x) sprintf("%.2f%%",x)
  count_pct <- function(n,p) paste0(num(n),' <span class="pct">(',pct(p),')</span>')
  values <- list()
  put <- function(table,row,col,value) values[[paste(table,row,col,sep="_")]] <<- as.character(value)
  raw <- list()
  for(i in seq_along(countries)) {
   k<-countries[i];folder<-if(k=="cotedivoire")"cdi"else k
   v<-switch(k,senegal="s5_6",cotedivoire="s2_5_sp","q506")
   fields<-c(v,if(k=="cotedivoire")c("fid","s5_1","s5_6","s5_3a","s5_3c","s5_3f","s5_3l"))
   raw[[k]]<-haven::read_dta(file.path(cache,"inputs/uom",folder,paste0("uom_",folder,"_facility_pooled.dta")),
    col_select=tidyselect::all_of(fields))
   x<-nv(raw[[k]][[v]]);n<-length(x);counts<-c(sum(x%in%1),sum(x%in%c(0,2)),sum(!x%in%c(0,1,2)))
   put(1,i,3,num(n))
   for(j in 1:3)put(1,i,j+3,count_pct(counts[j],100*counts[j]/n))
   d<-frames[[k]]$women;use<-d$need
   stopifnot(sum(use)==c(lagos=2159,senegal=1905,sindh=3274,cotedivoire=4292)[[k]])
   for(j in 1:7) {
    fields<-c("agency","stigma","health","fertility","confidence","opposes")
    missing<-if(j==7)!complete.cases(d[fields])else !is.finite(d[[fields[j]]])
    put(3,j,i+1,count_pct(sum(use&missing),100*survey_weighted_mean(missing[use],d$weight[use])))
   }
  }
  d<-frames$cotedivoire$women;supply<-frames$cotedivoire$supply
  unknown<-!complete.cases(supply);positive<-d$positive&is.finite(d$weight)&d$weight>0
  keys<-c("BONON","FACOBLY","GOHITAFLA","TAABO","DAOUKRO")
  for(i in 1:7) {
   selected<-if(i<=5)d$geo%in%keys[i]else if(i==6)!d$geo%in%unique(fac$assigned_join_area[fac$country=="cotedivoire"])&unknown else unknown
   if(i==6)selected<-unknown&!d$geo%in%keys
   n<-sum(selected);wp<-100*survey_weighted_mean(as.numeric(selected[positive]),d$weight[positive])
   put(4,i,2,num(n));put(4,i,3,pct(100*n/nrow(d)));put(4,i,4,if(i==6)"—"else pct(wp))
  }
  r<-raw$cotedivoire;f<-fac[fac$country=="cotedivoire",]
  ix<-match(as.character(r$fid),as.character(f$fid));stopifnot(!anyNA(ix))
  geo<-toupper(gsub("[^A-Za-z0-9]","",as.character(f$assigned_join_area[ix])))
  selected<-geo%in%"DAOUKRO"&nv(r$s2_5_sp)%in%1
  stopifnot(sum(selected)==19)
  for(i in 1:3) {
   x<-nv(r[[c("s2_5_sp","s5_1","s5_6")[i]]][selected]);yes<-sum(x%in%1);no<-sum(x%in%c(0,2))
   text<-if(!no)paste(num(yes),"Yes")else if(!yes)paste(num(no),"No")else paste0(num(yes)," Yes; ",num(no)," No")
   put(5,i,2,paste0("<strong>",text,"</strong>"))
  }
  for(i in 1:4) {
   x<-nv(r[[c("s5_3a","s5_3c","s5_3f","s5_3l")[i]]][selected])
   # This questionnaire uses -9 for not applicable; this is distinct from blanks.
   counts<-c(sum(x%in%1),sum(x%in%2),sum(x%in%(-9)),sum(is.na(x)))
   stopifnot(sum(counts)==sum(selected))
   for(j in 1:4)put(6,i,j+1,num(counts[j]))
  }
  yes<-nv(r$s2_5_sp)%in%1;onsite<-nv(r$s5_1)
  counts<-c(sum(yes&onsite%in%1),sum(yes&onsite%in%2),sum(yes))
  for(i in 1:3){put(8,i,2,num(counts[i]));put(8,i,3,pct(100*counts[i]/sum(yes)))}
  out<-arg("--output-json");dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
  jsonlite::write_json(values,out,auto_unbox=TRUE,pretty=TRUE)
  cat("Recalculated facility eligibility, six-domain missingness, supply nonmatches and Daoukro routing.\n")

})

# check demand population
.diag_codes[["check_demand_population.R"]] <- quote({
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Compare geographic classification using all eligible women versus women in need.
  # Read-only with respect to report inputs, source data and main classifications.
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
  data_dir<-arg("--data-dir");out<-arg("--output-dir")
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables
  current<-p$scores[p$scores$method=="zscore",];areas<-current$geo
  stopifnot(!anyDuplicated(areas),length(D)==6,all(d$need<=d$positive))
  selected<-d$positive&is.finite(d$weight)&d$weight>0&d$geo%in%areas
  source_hash<-unname(tools::md5sum(file.path(data_dir,c("analysis_frame.rds","report_data.json"))))
  wm<-function(v,w){ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))survey_weighted_mean(v[ok],w[ok])else NA_real_}
  classify<-function(df,sf)ifelse(is.na(df)|is.na(sf),"unassessed",ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
  cells<-list();result<-list();refs<-list()
  for(group in c("all_eligible","in_need")){
   use<-selected&if(group=="in_need")d$need else TRUE
   m<-sapply(D,function(k)vapply(areas,function(g)wm(d[[k]][use&d$geo%in%g],d$weight[use&d$geo%in%g]),numeric(1)))
   rownames(m)<-areas;complete<-complete.cases(m)
   mu<-colMeans(m[complete,,drop=FALSE]);sd<-apply(m[complete,,drop=FALSE],2,stats::sd)
   stopifnot(all(is.finite(mu)),all(sd>0),all(complete))
   z<-sweep(sweep(m,2,mu,"-"),2,sd,"/");score<-rowMeans(z);flag<-score>1e-10
   category<-classify(flag,current$supply_flag)
   if(group=="in_need"){
    stopifnot(max(abs(score-current$demand_score))<1e-9,
     identical(as.logical(flag),current$demand_flag),identical(as.character(category),as.character(current$category)))
   }
   result[[group]]<-data.frame(geo=areas,demand_score=score,demand_flag=flag,
    supply_flag=current$supply_flag,category=category,n_women=vapply(areas,function(g)sum(use&d$geo%in%g),integer(1)))
   for(k in D)for(g in areas){
    ii<-use&d$geo%in%g;valid<-ii&is.finite(d[[k]])
    cells[[length(cells)+1L]]<-data.frame(population=group,geo=g,variable=k,weighted_pct=100*m[g,k],n_valid=sum(valid),n_total=sum(ii))
   }
   refs[[group]]<-data.frame(population=group,variable=D,mean_pct=100*mu,SD_pct=100*sd,reference_areas=sum(complete))
  }
  a<-result$all_eligible;b<-result$in_need
  labels<-if("district"%in%names(d))vapply(areas,function(g)as.character(d$district[match(g,d$geo)]),character(1))else areas
  comparison<-data.frame(geo=areas,area=labels,all_eligible_n=a$n_women,in_need_n=b$n_women,
   all_eligible_demand_score=a$demand_score,in_need_demand_score=b$demand_score,
   all_eligible_demand_flag=a$demand_flag,in_need_demand_flag=b$demand_flag,
   supply_flag=current$supply_flag,all_eligible_category=a$category,current_in_need_category=b$category,
   changed=a$category!=b$category)
  summary<-list(country=p$audit$country,areas=nrow(comparison),changed=sum(comparison$changed),unchanged=sum(!comparison$changed),
   assessed=sum(comparison$current_in_need_category!="unassessed"),demand_flags_changed=sum(a$demand_flag!=b$demand_flag),
   all_eligible_categories=as.list(table(a$category)),current_in_need_categories=as.list(table(b$category)),
   current_need_classification_reproduced=TRUE,
   method="Keep six domain definitions, valid-answer rules, fixed geographic reference set and supply flags. Re-estimate weighted concern percentages, component means/SDs and mean six z-scores for each population. Demand>0; supply-readiness flag unchanged.",
   all_eligible_definition="Consent/positive final survey weight where required; all current use and need categories. Original source records outside weighted estimation are excluded from both weighted comparisons.",
   no_main_report_changes=TRUE)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  write.csv(comparison,file.path(out,"classification_population_comparison.csv"),row.names=FALSE,na="")
  write.csv(do.call(rbind,cells),file.path(out,"demand_prevalence_by_population.csv"),row.names=FALSE,na="")
  write.csv(do.call(rbind,refs),file.path(out,"normalization_by_population.csv"),row.names=FALSE,na="")
  jsonlite::write_json(summary,file.path(out,"summary.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
  stopifnot(identical(source_hash,unname(tools::md5sum(file.path(data_dir,c("analysis_frame.rds","report_data.json"))))))
  cat(summary$country,":",summary$changed,"of",summary$areas,"classifications differ; current need-only results reproduced.\n")

})

# score profile examples
.diag_codes[["score_profile_examples.R"]] <- quote({
  # Compare similar administrative-area scores with different component profiles.
  # Reads saved aggregate report inputs only; never changes the working reports.
  build_score_profile_examples<-function(data_dirs,work_dir){
   for(pkg in c("jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
   dir.create(work_dir,recursive=TRUE,showWarnings=FALSE)
   country_names<-c(lagos="Lagos",senegal="Senegal",cotedivoire="Côte d’Ivoire",sindh="Sindh")
   aliases<-c(IFAKOIJAYE="Ifako-Ijaye",IBEJULEKKI="Ibeju-Lekki",AJEROMIIFELODUN="Ajeromi-Ifelodun",AMUWOODOFIN="Amuwo-Odofin",LAGOSISLAND="Lagos Island",LAGOSMAINLAND="Lagos Mainland",ETIOSA="Eti-Osa",MBACKE="Mbacké",KOUNFAO="Koun-Fao",YAMOUSSOUKRO="Yamoussoukro",DISTRICTKEMARI="Kemari",QAMBARSHAHDADKOT="Qambar Shahdadkot",MEAGUI="Méagui")
   pretty<-function(g,p){if(g%in%names(aliases))return(unname(aliases[g]));q<-p$coverage[p$coverage$geo==g,,drop=FALSE];tools::toTitleCase(tolower(if(nrow(q)&&"name"%in%names(q))q$name[1]else g))}
   component_labels<-c(agency="Healthcare permission",stigma="Anticipated social stigma",health="Health / side-effect beliefs",fertility="Future-fertility concerns",confidence="Low method knowledge*",opposes="Opposes family planning",offering="Short + long-acting offering",free="Free-product provision",training="Trained-provider share",daily_provision="Daily method provision",larc_stock="At least one LARC in stock",stockout_burden="Stockout burden")
   profiles<-components<-candidates<-list();hashes<-list();source_checks<-list()
   for(country in names(data_dirs)){
    input<-file.path(data_dirs[[country]],"report_data.json");p<-jsonlite::fromJSON(input)
    hashes[[country]]<-unname(tools::md5sum(input));sc<-p$scores[p$scores$method=="zscore",]
    stopifnot(length(p$demand_variables)==6,length(p$supply_variables)==6)
    for(side in c("Demand","Supply")){
     fields<-if(side=="Demand")p$demand_variables else p$supply_variables
     for(i in seq_len(nrow(sc))){
      g<-sc$geo[i];tr<-p$component_trace[p$component_trace$side==side&p$component_trace$geo==g,];tr<-tr[match(fields,tr$variable),]
      stopifnot(nrow(tr)==6,identical(as.character(tr$variable),fields))
      if(any(!is.finite(tr$zscore)))next
      direction<-if(side=="Demand" || identical(p$audit$supply_score_orientation,"constraints"))1 else -1
      z<-tr$zscore*direction
      expected<-if(side=="Demand")sc$demand_score[i]else direction*sc$supply_score[i]
      stopifnot(abs(mean(z)-expected)<1e-9)
      hm<-p$heatmap[p$heatmap$geo==g&p$heatmap$side==side,];hm<-hm[match(fields,hm$variable),]
      stopifnot(max(abs(hm$value-tr$original_pct))<1e-8)
      if(side=="Demand")stopifnot(length(unique(hm$n_total))==1)
      coverage<-p$coverage[p$coverage$geo==g,];nr<-p$rates[p$rates$geo==g,]
      n_need<-if(side=="Demand")unique(hm$n_total)else if(nrow(nr)&&"n"%in%names(nr))nr$n[1]else NA_real_
      n_fac<-if("fp_facilities"%in%names(coverage))coverage$fp_facilities[1]else max(hm$n_total)
      pos<-pmax(z,0);neg<-pmin(z,0);breadth<-sum(z>1e-10);top<-which.max(z)
      concentration<-if(sum(pos)>0)max(pos)/sum(pos)else 0
      key<-paste(country,side,g,sep="|")
      profiles[[key]]<-data.frame(country=country,country_label=unname(country_names[country]),side=side,geo=g,area=pretty(g,p),score=expected,unfavorable_components=breadth,components_z_at_least_half=sum(z>=.5),largest_positive_component=fields[top],largest_positive_share=concentration,positive_total=sum(pos)/6,negative_total=sum(neg)/6,n_need=n_need,fp_facilities=n_fac,min_valid=min(hm$n_valid),max_valid=max(hm$n_valid),reference_areas=if("reference_n"%in%names(tr))unique(tr$reference_n)else if(side=="Demand")sum(is.finite(sc$demand_score))else sum(is.finite(sc$supply_score)))
      components[[key]]<-data.frame(country=country,side=side,geo=g,area=pretty(g,p),variable=fields,label=unname(component_labels[fields]),raw_pct=tr$original_pct,reference_raw_mean=if(side=="Supply" && identical(p$audit$supply_score_orientation,"constraints"))ifelse(fields=="stockout_burden",tr$indicator_mean,100-tr$indicator_mean)else ifelse(fields=="stockout_burden",100-tr$indicator_mean,tr$indicator_mean),risk_z=z,contribution=z/6,n_valid=hm$n_valid,n_total=hm$n_total,higher_raw_is_worse=side=="Demand"|fields=="stockout_burden")
     }
    }
    source_checks[[country]]<-data.frame(country=country,source_rows=p$audit$source_rows,need_n=p$audit$need_n,source_report_md5=hashes[[country]])
   }
   profiles<-do.call(rbind,profiles);components<-do.call(rbind,components)
   # Search complete profiles with an above-average challenge score on the same side.
   for(country in names(data_dirs))for(side in c("Demand","Supply")){
    a<-profiles[profiles$country==country&profiles$side==side&profiles$score>1e-10,]
    if(nrow(a)<2)next
    for(pair in combn(seq_len(nrow(a)),2,simplify=FALSE)){
     aa<-a[pair[1],];bb<-a[pair[2],];gap<-abs(aa$score-bb$score)
     if(gap>.1+1e-10)next
     x<-components[components$country==country&components$side==side&components$geo==aa$geo,]
     y<-components[components$country==country&components$side==side&components$geo==bb$geo,];y<-y[match(x$variable,y$variable),]
     candidates[[length(candidates)+1]]<-data.frame(country=country,side=side,area_a=aa$area,geo_a=aa$geo,score_a=aa$score,area_b=bb$area,geo_b=bb$geo,score_b=bb$score,score_gap=gap,breadth_a=aa$unfavorable_components,breadth_b=bb$unfavorable_components,largest_positive_share_a=aa$largest_positive_share,largest_positive_share_b=bb$largest_positive_share,mean_absolute_component_difference=mean(abs(x$risk_z-y$risk_z)),adequate_for_illustration=if(side=="Demand")min(aa$min_valid,bb$min_valid)>=50 else min(aa$min_valid,bb$min_valid)>=20)
    }
   }
   candidates<-do.call(rbind,candidates)
   # Illustrative choices from the candidate search, not estimates of causal drivers.
   cases<-data.frame(country=c("lagos","lagos","senegal","senegal","cotedivoire","cotedivoire"),side=rep(c("Demand","Supply"),3),geo_a=c("IFAKOIJAYE","IKORODU","MBACKE","GOUDIRY","DIVO","KOUNFAO"),geo_b=c("APAPA","IBEJULEKKI","BIGNONA","MATAM","YAMOUSSOUKRO","GAGNOA"),stringsAsFactors=FALSE)
   cases$case_id<-paste(cases$country,tolower(cases$side),sep="_")
   selected<-list();plots<-list()
   for(i in seq_len(nrow(cases))){
    cc<-cases[i,];a<-profiles[profiles$country==cc$country&profiles$side==cc$side&profiles$geo==cc$geo_a,];b<-profiles[profiles$country==cc$country&profiles$side==cc$side&profiles$geo==cc$geo_b,]
    stopifnot(nrow(a)==1,nrow(b)==1,a$score>0,b$score>0,abs(a$score-b$score)<=.1+1e-10)
    stopifnot(if(cc$side=="Demand")min(a$min_valid,b$min_valid)>=50 else min(a$min_valid,b$min_valid)>=20)
    v<-components[components$country==cc$country&components$side==cc$side&components$geo%in%c(cc$geo_a,cc$geo_b),]
    fields<-v$variable[v$geo==cc$geo_a];v$row<-factor(v$variable,levels=rev(fields))
    panel_labels<-sprintf("%s  |  total %+.3f\n%d of 6 components above-average challenges",c(a$area,b$area),c(a$score,b$score),c(a$unfavorable_components,b$unfavorable_components))
    v$panel<-factor(v$geo,levels=c(cc$geo_a,cc$geo_b),labels=panel_labels)
    v$direction<-ifelse(v$risk_z>0,"More challenging than reference","More favorable than reference")
    hue<-if(cc$side=="Demand")"#087f8a"else"#c7772c"
    labels<-setNames(v$label[match(fields,v$variable)],fields)
    g<-ggplot2::ggplot(v,ggplot2::aes(contribution,row,fill=direction))+
     ggplot2::geom_vline(xintercept=0,color="#6c8490",linewidth=.55)+ggplot2::geom_col(width=.58)+
     ggplot2::geom_text(ggplot2::aes(label=sprintf("%.1f%%",raw_pct),hjust=ifelse(contribution>=0,-.12,1.12)),size=3.4,color="#254252")+
     ggplot2::facet_wrap(~panel,nrow=1)+ggplot2::scale_y_discrete(labels=labels)+
     ggplot2::scale_fill_manual(values=c("More challenging than reference"=hue,"More favorable than reference"="#b7c7ce"))+
     ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(.22,.24)))+
     ggplot2::labs(title=paste(unname(country_names[cc$country]),tolower(cc$side),"profiles: similar totals, different components"),
      subtitle="Bars are signed contributions to the total score (component z / 6). Labels show the underlying percentages.",
      x="Contribution to the overall challenge score",y=NULL,fill=NULL,
      caption=paste0(if(cc$side=="Demand")"Percentages: survey-weighted women in need, valid answers separately by domain."else"Percentages: unweighted FP-provider summaries, valid facilities separately by measure; higher stockout burden is worse.",
       "\nArea-level planning profiles from the Rural/Urban reports. Positive contributions add challenges; negative contributions offset them.",
       if(cc$side=="Demand")"\n*Retained knowledge label measures confidence choosing a method or managing side effects. These are score components, not proven causes of nonuse."else"\nThese are contributions to an index, not estimated causal effects on contraceptive use."))+
     ggplot2::theme_minimal(base_family="sans",base_size=11)+ggplot2::theme(panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor=ggplot2::element_blank(),strip.background=ggplot2::element_rect(fill="#edf4f6",color=NA),strip.text=ggplot2::element_text(face="bold",color="#17374a",lineheight=1.2),plot.title=ggplot2::element_text(face="bold",size=17,color="#17374a"),plot.subtitle=ggplot2::element_text(size=10,color="#617b88"),plot.caption=ggplot2::element_text(hjust=0,size=9,color="#617b88",lineheight=1.2),legend.position="bottom",legend.text=ggplot2::element_text(size=9),panel.spacing=grid::unit(1.5,"lines"),plot.margin=ggplot2::margin(15,20,12,12))
    svg<-paste0(cc$case_id,".svg");png<-paste0(cc$case_id,".png")
    ggplot2::ggsave(file.path(work_dir,svg),g,width=14,height=6.8,device=svglite::svglite,bg="white")
    ggplot2::ggsave(file.path(work_dir,png),g,width=14,height=6.8,dpi=170,bg="white")
    selected[[cc$case_id]]<-list(case=cc,a=a,b=b,components=v,svg=svg,png=png,score_gap=abs(a$score-b$score))
   }
   write.csv(profiles,file.path(work_dir,"all_area_profile_metrics.csv"),row.names=FALSE,na="")
   write.csv(components,file.path(work_dir,"all_component_contributions.csv"),row.names=FALSE,na="")
   write.csv(candidates,file.path(work_dir,"similar_score_candidate_pairs.csv"),row.names=FALSE,na="")
   write.csv(cases,file.path(work_dir,"selected_examples.csv"),row.names=FALSE)
   audit<-list(countries=names(data_dirs),selected_cases=nrow(cases),maximum_selected_score_gap=max(vapply(selected,function(v)v$score_gap,numeric(1))),source_checks=do.call(rbind,source_checks),source_report_md5=hashes,
    method="Compare above-average area planning challenge scores with absolute gap <=0.10. Demand uses original risk z; supply uses negative readiness z for a common higher=worse display. Existing scores and classifications are unchanged.",
    selection="Illustrative pairs chosen from the search for clear profile differences and valid denominators (at least 50 per demand domain or 20 per supply measure). These are exploratory rules, not clinical thresholds or tests of regional differences.",
    concentration="Largest positive z / sum of positive z, excluding favorable negative components. This is an arithmetic profile statistic, not a share of the net score or of nonuse.",
    precision="No inference about profile differences is made. Raw demand percentages use survey weights; area standardization references and facility means are unweighted across their respective records. Small area/item Ns and overlapping indicators limit interpretation.")
   result<-list(selected=selected,profiles=profiles,components=components,candidates=candidates,audit=audit,labels=component_labels)
   saveRDS(result,file.path(work_dir,"score_profile_examples.rds"))
   jsonlite::write_json(audit,file.path(work_dir,"verification.json"),auto_unbox=TRUE,pretty=TRUE,dataframe="rows",digits=12)
   for(c in names(data_dirs))stopifnot(identical(hashes[[c]],unname(tools::md5sum(file.path(data_dirs[[c]],"report_data.json")))))
   cat("Verified component sums for",nrow(profiles),"area-side profiles; selected six pairs without modifying source reports.\n")
   normalizePath(work_dir,mustWork=TRUE)
  }

  if(sys.nframe()==0L){
   args<-commandArgs(trailingOnly=TRUE)
   arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop("Supply ",k);return(default)};if(i==length(args))stop("Supply ",k);args[i+1L]}
   if("--help"%in%args){cat("Rscript analysis/score_profile_examples.R --lagos-dir ANALYSIS --senegal-dir ANALYSIS --cotedivoire-dir ANALYSIS [--work-dir output/score_profiles] [--output-dir reports] [--pandoc-dir DIRECTORY]\n");quit(status=0)}
   script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
   root<-normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
   source(file.path(root,"support/runtime.R"));use_project_library(root)
   resolve<-function(p)project_path(p,root)
   dirs<-lapply(c(lagos=arg("--lagos-dir"),senegal=arg("--senegal-dir"),cotedivoire=arg("--cotedivoire-dir")),resolve)
   work<-build_score_profile_examples(dirs,resolve(arg("--work-dir","output/score_profiles")))
   pandoc<-arg("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
   out<-resolve(arg("--output-dir","reports"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
   rmarkdown::render(file.path(root,"data_checks/score_profile_examples.Rmd"),output_file="Score_profile_examples.html",output_dir=normalizePath(out),params=list(prepared_dir=work),quiet=TRUE,envir=new.env(parent=globalenv()))
   cat("Saved Score_profile_examples.html\n")
  }

})

# score profile contrasts
.diag_codes[["score_profile_contrasts.R"]] <- quote({
  # Extend the saved-profile comparison to all four geographies.
  # Only aggregate report inputs are read. No scores, models or source reports change.
  build_score_profile_contrasts <- function(data_dirs, work_dir, project_root) {
    source(file.path(project_root, 'analysis/score_profile_examples.R'))
    source(file.path(project_root, 'analysis/plot_score_profiles_combined.R'))
    build_score_profile_examples(data_dirs, file.path(work_dir, 'profile_audit'))
    x <- readRDS(file.path(work_dir, 'profile_audit/score_profile_examples.rds'))
    pairs <- list()
    for (country in names(data_dirs)) for (side in c('Demand','Supply')) {
      p <- x$profiles[x$profiles$country==country & x$profiles$side==side & x$profiles$score>1e-10,]
      if(nrow(p)<2) next
      for(ii in combn(seq_len(nrow(p)),2,simplify=FALSE)) {
        a<-p[ii[1],];b<-p[ii[2],]
        ca<-x$components[x$components$country==country & x$components$side==side & x$components$geo==a$geo,]
        cb<-x$components[x$components$country==country & x$components$side==side & x$components$geo==b$geo,]
        cb<-cb[match(ca$variable,cb$variable),]
        difference<-abs(ca$raw_pct-cb$raw_pct);j<-which.max(difference)
        pairs[[length(pairs)+1]]<-data.frame(country=country,country_label=a$country_label,side=side,
          geo_a=a$geo,area_a=a$area,score_a=a$score,geo_b=b$geo,area_b=b$area,score_b=b$score,
          score_gap=abs(a$score-b$score),breadth_a=a$unfavorable_components,breadth_b=b$unfavorable_components,
          concentration_a=a$largest_positive_share,concentration_b=b$largest_positive_share,
          minimum_valid=min(a$min_valid,b$min_valid),mean_absolute_pp=mean(difference),maximum_pp=max(difference),
          maximum_gap_component=ca$variable[j],maximum_gap_label=ca$label[j],
          maximum_gap_pct_a=ca$raw_pct[j],maximum_gap_pct_b=cb$raw_pct[j],
          near_equal=abs(a$score-b$score)<=.1+1e-10,
          illustration_n=min(a$min_valid,b$min_valid)>=(if(side=='Demand')50 else 20),
          broad_vs_concentrated=(a$unfavorable_components>=5 & b$largest_positive_share>=.7) |
            (b$unfavorable_components>=5 & a$largest_positive_share>=.7))
      }
    }
    pairs<-do.call(rbind,pairs)
    pairs<-pairs[order(pairs$country,pairs$side,-pairs$mean_absolute_pp,pairs$score_gap),]
    overview<-do.call(rbind,lapply(names(data_dirs),function(country) {
      p<-pairs[pairs$country==country & pairs$side=='Demand' & pairs$near_equal & pairs$illustration_n,]
      stopifnot(nrow(p)>0);p[which.max(p$mean_absolute_pp),]
    }))
    stopifnot(sum(pairs$side=='Demand' & pairs$near_equal & pairs$broad_vs_concentrated)==0)
    cases<-data.frame(case_id=c('sindh_sharp_demand','cdi_broader_demand','senegal_broad_demand','cdi_concentrated_supply'),
      country=c('sindh','cotedivoire','senegal','cotedivoire'),side=c('Demand','Demand','Demand','Supply'),
      geo_a=c('DISTRICTKEMARI','DIVO','DAGANA','ABIDJAN'),geo_b=c('KHAIRPUR','DABOU','BIGNONA','GAGNOA'))
    selected<-list()
    for(i in seq_len(nrow(cases))) {
      cc<-cases[i,]
      a<-x$profiles[x$profiles$country==cc$country & x$profiles$side==cc$side & x$profiles$geo==cc$geo_a,]
      b<-x$profiles[x$profiles$country==cc$country & x$profiles$side==cc$side & x$profiles$geo==cc$geo_b,]
      stopifnot(nrow(a)==1,nrow(b)==1,a$score>0,b$score>0)
      v<-x$components[x$components$country==cc$country & x$components$side==cc$side &
        x$components$geo%in%c(cc$geo_a,cc$geo_b),]
      for(gg in c(cc$geo_a,cc$geo_b)) stopifnot(abs(sum(v$contribution[v$geo==gg])-
        x$profiles$score[x$profiles$country==cc$country & x$profiles$side==cc$side & x$profiles$geo==gg])<1e-9)
      selected[[cc$case_id]]<-list(case=cc,a=a,b=b,components=v,score_gap=abs(a$score-b$score))
    }
    selected$senegal_broad_demand$plot_title<-'Broader concerns versus a concentrated confidence concern'
    x$selected<-selected;x$pairs<-pairs;x$overview<-overview
    x$labels['confidence']<-'Low confidence choosing a method or managing side effects'
    x$audit$selection<-paste('Four-geography search within country; complete profiles and positive totals.',
      'Near-equal means absolute score gap <= 0.10 in the original average-z units.',
      'Demand overview maximizes mean absolute percentage-point contrast among pairs with at least 50 valid women per domain.',
      'Broad means at least five positive component z-values; concentrated means one supplies at least 70% of positive component z-points.',
      'The separate Dagana/Bignona example relaxes score similarity and the 50-response screening rule and is labelled accordingly.')
    x$audit$demand_profiles<-sum(x$profiles$side=='Demand')
    x$audit$supply_profiles<-sum(x$profiles$side=='Supply')
    x$audit$strict_near_equal_demand_pairs<-sum(pairs$side=='Demand' & pairs$near_equal & pairs$broad_vs_concentrated)
    x$audit$strict_near_equal_supply_pairs<-sum(pairs$side=='Supply' & pairs$near_equal & pairs$broad_vs_concentrated)
    x$audit$selected_cases<-nrow(cases)
    saveRDS(x,file.path(work_dir,'score_profile_contrasts.rds'))
    write.csv(pairs,file.path(work_dir,'all_positive_profile_pairs.csv'),row.names=FALSE,na='')
    write.csv(overview,file.path(work_dir,'four_geography_demand_overview.csv'),row.names=FALSE,na='')
    for(id in names(selected))plot_score_profile_percentages(file.path(work_dir,'score_profile_contrasts.rds'),id,work_dir,
      supply_score='challenge',
      supply_percentages=if(selected[[id]]$case$side=='Supply')'original'else'gaps')
    jsonlite::write_json(x$audit,file.path(work_dir,'verification.json'),pretty=TRUE,auto_unbox=TRUE,dataframe='rows',digits=12)
    cat('Verified four-geography contrasts without refitting or changing source reports.\n')
    invisible(x)
  }

  if(sys.nframe()==0L) {
    args<-commandArgs(trailingOnly=TRUE)
    arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
    if('--help'%in%args){cat('Rscript analysis/score_profile_contrasts.R --lagos-dir ANALYSIS --senegal-dir ANALYSIS --cotedivoire-dir ANALYSIS --sindh-dir ANALYSIS [--work-dir output/score_profile_contrasts] [--output-dir reports] [--pandoc-dir DIRECTORY]\n');quit(status=0)}
    script<-gsub('~+~',' ',sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]),fixed=TRUE)
    root<-normalizePath(file.path(dirname(script),'..'),mustWork=TRUE)
    source(file.path(root,'support/runtime.R'));use_project_library(root)
    resolve<-function(p)project_path(p,root)
    dirs<-lapply(c(lagos=arg('--lagos-dir'),senegal=arg('--senegal-dir'),cotedivoire=arg('--cotedivoire-dir'),sindh=arg('--sindh-dir')),resolve)
    work<-resolve(arg('--work-dir','output/score_profile_contrasts'));dir.create(work,recursive=TRUE,showWarnings=FALSE)
    build_score_profile_contrasts(dirs,work,root)
    pandoc<-arg('--pandoc-dir',Sys.getenv('RSTUDIO_PANDOC',''));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
    out<-resolve(arg('--output-dir','reports'));dir.create(out,recursive=TRUE,showWarnings=FALSE)
    rmarkdown::render(file.path(root,'data_checks/score_profile_contrasts.Rmd'),output_file='Score_profile_contrasts.html',
      output_dir=normalizePath(out),params=list(prepared_dir=work),quiet=TRUE,envir=new.env(parent=globalenv()))
  }

})

# plot demand pair table
.diag_codes[["plot_demand_pair_table.R"]] <- quote({
  # Presentation table from saved aggregate profiles; no model refitting.
  plot_demand_pair_table <- function(input,country,area_a,area_b,output_dir,html_output='') {
    for(p in c('svglite','jsonlite'))if(!requireNamespace(p,quietly=TRUE))stop('Install ',p)
    x<-readRDS(input)
    pick<-function(g)x$profiles[x$profiles$country==country&x$profiles$side=='Demand'&x$profiles$geo==g,]
    a<-pick(area_a);b<-pick(area_b);stopifnot(nrow(a)==1,nrow(b)==1)
    v<-x$components[x$components$country==country&x$components$side=='Demand',]
    fields<-c('agency','stigma','health','fertility','confidence','opposes')
    aa<-v[v$geo==area_a,];aa<-aa[match(fields,aa$variable),]
    bb<-v[v$geo==area_b,];bb<-bb[match(fields,bb$variable),]
    stopifnot(nrow(aa)==6,nrow(bb)==6,all(is.finite(aa$risk_z)),all(is.finite(bb$risk_z)),
      max(abs(aa$reference_raw_mean-bb$reference_raw_mean))<1e-9,
      abs(mean(aa$risk_z)-a$score)<1e-9,abs(mean(bb$risk_z)-b$score)<1e-9)
    labels<-c('Healthcare permission barriers','Anticipated social stigma','Health / side-effect beliefs',
      'Future-fertility concerns','Low method knowledge','Opposition to family planning')
    difference<-abs(aa$raw_pct-bb$raw_pct)
    higher<-ifelse(aa$raw_pct>bb$raw_pct,a$area,ifelse(bb$raw_pct>aa$raw_pct,b$area,'Equal'))
    shade<-function(z)if(z<=1e-10)'#F5F7F9'else grDevices::colorRampPalette(c('#FFF0D9','#C46024'))(301)[1+round(100*min(z,3))]
    text<-function(label,x,y,size=12,bold=FALSE,col='#244652',just='left')grid::grid.text(label,x=x,y=y,just=just,
      gp=grid::gpar(fontfamily='sans',fontsize=size,fontface=if(bold)'bold'else'plain',col=col))
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    stem<-paste(country,tolower(area_a),tolower(area_b),'table',sep='_');svg<-file.path(output_dir,paste0(stem,'.svg'))
    svglite::svglite(svg,width=15,height=8.5,bg='white');grid::grid.newpage()
    text('Similar demand scores, different reported concerns',.025,.955,24,TRUE,col='#173E50')
    for(i in 1:2){q<-list(a,b)[[i]];left<-c(.025,.515)[i]
      grid::grid.roundrect(x=left+.23,y=.815,width=.46,height=.105,r=grid::unit(.012,'npc'),gp=grid::gpar(fill='#EEF4F6',col='#D8E3E8'))
      text(sprintf('%s  |  demand score %+.3f',q$area,q$score),left+.016,.839,17,TRUE)
      text(sprintf('%d of 6 components above average · %s women in need',q$unfavorable_components,format(q$n_need,big.mark=',')),left+.016,.795,11,col='#55707D')
    }
    text('Bold = above reference average',.025,.723,11,TRUE)
    text('Shading: farther above average',.315,.723,11)
    for(j in 1:40)grid::grid.rect(x=.570+j*.0021,y=.724,width=.0022,height=.021,gp=grid::gpar(fill=shade(j*3/40),col=NA))
    text('Blue bars: difference between areas',.705,.723,11,col='#246D95')
    grid::grid.rect(x=.5,y=.664,width=.95,height=.058,gp=grid::gpar(fill='#173E50',col=NA))
    text('Demand component',.038,.664,12,TRUE,col='white')
    text('Reference\naverage',.397,.664,11,TRUE,col='white',just='centre')
    text(a$area,.528,.664,12,TRUE,col='white',just='centre');text(b$area,.655,.664,12,TRUE,col='white',just='centre')
    text('Difference · percentage points',.735,.664,11,TRUE,col='white')
    y<-.594-(0:5)*.082
    for(i in seq_along(fields)) {
      grid::grid.rect(x=.5,y=y[i],width=.95,height=.079,gp=grid::gpar(fill=if(i%%2)'#FFFFFF'else'#FAFCFD',col=NA))
      text(labels[i],.038,y[i],12);text(sprintf('%.1f%%',aa$reference_raw_mean[i]),.397,y[i],13,col='#6A818C',just='centre')
      for(j in 1:2){z<-list(aa,bb)[[j]]$risk_z[i];pct<-list(aa,bb)[[j]]$raw_pct[i];cx<-c(.528,.655)[j]
        grid::grid.roundrect(x=cx,y=y[i],width=.112,height=.062,r=grid::unit(.007,'npc'),gp=grid::gpar(fill=shade(z),col=NA))
        text(sprintf('%.1f%%',pct),cx,y[i],15,bold=z>1e-10,col=if(z>=2.4)'white'else'#244652',just='centre')
      }
      start<-.735;width<-.221
      grid::grid.roundrect(x=start+width/2,y=y[i]-.019,width=width,height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#E8F0F5',col=NA))
      if(difference[i]>0)grid::grid.roundrect(x=start+width*difference[i]/200,y=y[i]-.019,width=width*difference[i]/100,
        height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#3F86AC',col=NA))
      text(sprintf('%.1f pp · higher in %s',difference[i],higher[i]),start,y[i]+.009,11,col='#276D92')
    }
    grDevices::dev.off()
    vals<-data.frame(component=fields,label=labels,reference_pct=aa$reference_raw_mean,
      area_a=a$area,pct_a=aa$raw_pct,z_a=aa$risk_z,above_a=aa$risk_z>1e-10,n_a=aa$n_valid,
      area_b=b$area,pct_b=bb$raw_pct,z_b=bb$risk_z,above_b=bb$risk_z>1e-10,n_b=bb$n_valid,
      difference_pp=difference,higher_area=higher)
    write.csv(vals,file.path(output_dir,paste0(stem,'_values.csv')),row.names=FALSE)
    if(nzchar(html_output)){
      esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
      rows<-vapply(seq_len(nrow(vals)),function(i)paste0('<tr><td>',esc(vals$label[i]),'</td><td>',sprintf('%.1f%%',vals$reference_pct[i]),
        '</td><td>',if(vals$above_a[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_a[i]),if(vals$above_a[i])'</strong>'else'',
        '</td><td>',if(vals$above_b[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_b[i]),if(vals$above_b[i])'</strong>'else'',
        '</td><td>',sprintf('%.1f pp',vals$difference_pp[i]),' · higher in ',esc(vals$higher_area[i]),'</td></tr>'),character(1))
      html<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Demand comparison table example</title><style>body{margin:0;background:#F2F6F8;color:#244652;font:16px/1.6 Arial,sans-serif}main{max-width:1500px;margin:24px auto;padding:0 20px}figure{margin:0;background:white;border:1px solid #DBE5EA;border-radius:8px}img{display:block;width:100%;height:auto}.note{background:white;padding:18px 24px;margin:16px 0;border-left:4px solid #3F86AC}.table-wrap{overflow-x:auto;background:white}table{width:100%;border-collapse:collapse;font-size:14px}th,td{padding:12px;text-align:left;border-bottom:1px solid #DFE8ED}th{background:#EEF4F6}summary{cursor:pointer;font-weight:bold;padding:12px}@media print{body{background:white}}</style></head><body><main><figure><img alt="Raw demand percentages with above-reference bolding, relative-elevation shading and percentage-point differences for ',esc(a$area),' and ',esc(b$area),'" src="data:image/svg+xml;base64,',jsonlite::base64_enc(readBin(svg,'raw',n=file.info(svg)$size)),
        '"></figure><details><summary>Definitions and accessible values</summary><p>Bold means above the component reference mean (z &gt; 0). Orange becomes darker as the positive component z-score increases; shade saturates at z = 3. Values at or below average are neutral. Blue bars show absolute percentage-point differences on a fixed 0–100 scale, with text naming the higher area. No new threshold changes scores or classifications.</p><p>Reference averages give each area equal weight within the geography. Individual area percentages are survey-weighted among women in need, using valid responses per component. These are descriptive point estimates, not tests of differences. The retained label “Low method knowledge” refers to MC5 confidence/self-efficacy, not an objective knowledge test.</p><div class="table-wrap"><table><thead><tr><th>Component</th><th>Reference average</th><th>',esc(a$area),'</th><th>',esc(b$area),'</th><th>Difference</th></tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div></details></main></body></html>')
      dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(html,html_output,useBytes=TRUE)
    }
    invisible(svg)
  }

  plot_all_demand_pair_tables <- function(input,output_dir,html_output='') {
    cases<-data.frame(country=c('cotedivoire','senegal','lagos','sindh'),
      label=c('Côte d’Ivoire','Senegal','Lagos','Sindh'),
      a=c('MEAGUI','MBACKE','AJEROMIIFELODUN','DISTRICTKEMARI'),
      b=c('YAMOUSSOUKRO','BIGNONA','IFAKOIJAYE','KHAIRPUR'))
    sections<-character(nrow(cases));style<-''
    for(i in seq_len(nrow(cases))){
      cc<-cases[i,];child<-file.path(output_dir,paste0(cc$country,'_demand_comparison.html'))
      plot_demand_pair_table(input,cc$country,cc$a,cc$b,output_dir,child)
      h<-paste(readLines(child,warn=FALSE),collapse='\n')
      fragment<-regmatches(h,regexpr('(?s)<main>.*?</main>',h,perl=TRUE))
      fragment<-sub('^<main>','',sub('</main>$','',fragment))
      sections[i]<-paste0('<section id="',cc$country,'" aria-label="',cc$label,'">',fragment,'</section>')
      if(i==1)style<-regmatches(h,regexpr('(?s)<style>.*?</style>',h,perl=TRUE))
    }
    if(nzchar(html_output)){
      nav<-paste0('<a href="#',cases$country,'">',cases$label,'</a>',collapse='')
      h<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Demand pair comparisons</title>',style,
        '<style>header{max-width:1500px;margin:24px auto;padding:0 20px}h1{font-size:28px;color:#173E50}nav{display:flex;gap:12px;flex-wrap:wrap}nav a{color:#246D95;text-decoration:none;padding:6px 14px;background:white;border:1px solid #DBE5EA;border-radius:18px}section{margin:0 0 32px}details{background:white;padding:10px 20px;margin-top:10px;border-radius:6px}section:target{scroll-margin-top:18px}details p{max-width:1100px}@media print{header,details{display:none}section{break-inside:avoid;break-after:page}}</style></head><body><header><h1>Demand pair comparisons</h1><nav>',nav,'</nav></header><main>',paste(sections,collapse=''),'</main></body></html>')
      dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(h,html_output,useBytes=TRUE)
    }
    invisible(cases)
  }

  plot_regional_comparisons <- function(input,output_dir,html_output) {
    cases<-data.frame(
      id=c('cotedivoire','senegal','lagos','sindh-kemari-khairpur','sindh-hyderabad-sanghar'),
      country=c('cotedivoire','senegal','lagos','sindh','sindh'),
      label=c('Côte d’Ivoire','Senegal','Lagos','Sindh','Sindh'),
      a=c('MEAGUI','MBACKE','AJEROMIIFELODUN','DISTRICTKEMARI','HYDERABAD'),
      b=c('YAMOUSSOUKRO','BIGNONA','IFAKOIJAYE','KHAIRPUR','SANGHAR'),
      comparison_type=c('Both — different types of challenge and different breadth and severity',
        'Different breadth and severity','Different types of challenge',
        'Different types of challenge','Both — different types of challenge and different breadth and severity'),
      takeaway=c(
        'Méagui exceeds the reference average on five components, compared with three in Yamoussoukro. Health/side-effect concerns are higher in Méagui (70.6% versus 57.5%), while low method knowledge is much higher in Yamoussoukro (76.5% versus 17.5%). The similar scores combine a broader set of above-average concerns in Méagui with a particularly large confidence-related concern in Yamoussoukro.',
        'Mbacké has four components above average, including one marginal elevation, compared with two in Bignona. Bignona’s standout concern is low method knowledge (86.5% versus 33.3% in Mbacké). Mbacké has higher health/side-effect concerns (68.1% versus 31.7%) and opposition to family planning (34.7% versus 18.2%).',
        'Both LGAs have five components above average, but the mix differs. Ajeromi-Ifelodun has more social stigma (23.4% versus 7.8%) and opposition to family planning (36.0% versus 27.2%). Ifako-Ijaye has more healthcare permission barriers (24.2% versus 11.9%) and health/side-effect concerns (57.3% versus 49.0%).',
        'Kemari and Khairpur have almost identical scores, but low method knowledge is 85.2% in Kemari and 10.0% in Khairpur. Khairpur has more future-fertility concerns (89.5% versus 63.1%) and social stigma (50.3% versus 23.8%). Three components are above average in Kemari, compared with four in Khairpur.',
        'Hyderabad’s profile features high health/side-effect and future-fertility concerns (92.0% and 89.0%). Sanghar has more social stigma (65.5% versus 30.4%), low method knowledge (40.9% versus 11.0%) and opposition to family planning (25.2% versus 11.1%). Sanghar has five components above average, compared with three in Hyderabad, although its permission barriers and health concerns are only slightly above their respective averages.'),
      stringsAsFactors=FALSE)
    x<-readRDS(input)
    sections<-nav<-character(nrow(cases));style<-''
    esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
    for(i in seq_len(nrow(cases))){
      cc<-cases[i,]
      q<-x$profiles[x$profiles$country==cc$country&x$profiles$side=='Demand'&x$profiles$geo%in%c(cc$a,cc$b),]
      stopifnot(nrow(q)==2,all(is.finite(q$score)),all(q$score>0))
      a<-q[q$geo==cc$a,];b<-q[q$geo==cc$b,]
      child<-file.path(output_dir,paste0(cc$id,'_demand_comparison.html'))
      plot_demand_pair_table(input,cc$country,cc$a,cc$b,output_dir,child)
      h<-paste(readLines(child,warn=FALSE),collapse='\n')
      fragment<-regmatches(h,regexpr('(?s)<main>.*?</main>',h,perl=TRUE))
      fragment<-sub('^<main>','',sub('</main>$','',fragment))
      finding<-paste0('<div class="finding"><p class="comparison-label"><strong>What this comparison shows:</strong> ',esc(cc$comparison_type),'</p><p><strong>Key finding.</strong> ',esc(cc$takeaway),'</p></div>')
      fragment<-sub('</figure>',paste0('</figure>',finding),fragment,fixed=TRUE)
      title<-paste0(cc$label,' · ',a$area,' and ',b$area)
      sections[i]<-paste0('<section id="',cc$id,'" aria-labelledby="',cc$id,'-title"><h2 id="',cc$id,'-title">',i,'. ',esc(title),'</h2>',fragment,'</section>')
      nav[i]<-paste0('<a href="#',cc$id,'">',i,'. ',esc(a$area),' / ',esc(b$area),'</a>')
      if(i==1)style<-regmatches(h,regexpr('(?s)<style>.*?</style>',h,perl=TRUE))
    }
    h<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Composite score comparisons</title>',style,
      '<style>header{max-width:1500px;margin:32px auto 20px;padding:0 20px}h1{margin:0 0 16px;font-size:32px;line-height:1.2;color:#173E50}header p{max-width:1120px;margin:10px 0}.intro{font-size:18px}.context{font-size:14px;color:#55707D}nav{display:flex;gap:8px;flex-wrap:wrap;margin-top:22px}nav a{color:#246D95;text-decoration:none;padding:6px 12px;background:white;border:1px solid #DBE5EA;border-radius:18px;font-size:14px}nav a:hover,nav a:focus{background:#E5EFF4}h2{font-size:22px;line-height:1.35;color:#173E50;margin:0 0 14px}section{margin:0 0 48px;scroll-margin-top:20px}.finding{background:#EAF2F6;border-left:4px solid #3F86AC;border-radius:0 6px 6px 0;padding:16px 22px;margin:14px 0}.finding p{margin:0;max-width:1260px}details{background:white;padding:8px 18px;margin-top:10px;border:1px solid #DBE5EA;border-radius:6px}details p{max-width:1100px}summary{font-size:14px}main{margin-top:28px}@media(max-width:640px){header{margin-top:22px}h1{font-size:27px}h2{font-size:20px}.intro{font-size:17px}.finding{padding:14px 16px}section{margin-bottom:36px}}@media print{body{background:white}header{margin:0;padding:0}header nav,details{display:none}main{padding:0}section{break-before:page;break-inside:avoid}.finding{background:#F2F6F8}h1{font-size:26px}}</style></head><body><header><h1>Composite score comparisons</h1>',
      '<p class="intro">These five comparisons show areas flagged for <strong>“Demand” concerns</strong> that have similar overall demand risk scores but face different challenges. <strong>Similar overall scores can reflect different barriers; the component profile shows what the score alone conceals.</strong></p>',
      '<p class="context">All percentages describe women in need of family planning. “Demand” means an above-average overall demand score within the geography; some areas are also flagged for supply concerns. Reference averages give each surveyed area equal weight. An above-average component is not necessarily far above average.</p><nav aria-label="Comparisons">',paste(nav,collapse=''),'</nav></header><main>',paste(sections,collapse=''),'</main></body></html>')
    h<-sub('</style></head>','</style><style>.finding p.comparison-label{margin:0 0 8px;font-size:14px;font-weight:600;color:#246D95}</style></head>',h,fixed=TRUE)
    dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(h,html_output,useBytes=TRUE)
    invisible(cases)
  }

  if(sys.nframe()==0L){
    args<-commandArgs(trailingOnly=TRUE)
    arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
    if('--help'%in%args){cat('Rscript analysis/plot_demand_pair_table.R --input AGGREGATE_PROFILES [--regional-comparisons | --all-pairs | --country COUNTRY --area-a KEY --area-b KEY] [--output-dir output/demand_pair_tables] [--html-output REPORT]\n');quit(status=0)}
    if('--regional-comparisons'%in%args)plot_regional_comparisons(arg('--input'),arg('--output-dir','output/demand_pair_tables'),arg('--html-output'))else
    if('--all-pairs'%in%args)plot_all_demand_pair_tables(arg('--input'),arg('--output-dir','output/demand_pair_tables'),arg('--html-output',''))else
      plot_demand_pair_table(arg('--input'),arg('--country','senegal'),arg('--area-a','MBACKE'),arg('--area-b','BIGNONA'),arg('--output-dir','output/demand_pair_tables'),arg('--html-output',''))
  }

})

# plot supply pair table
.diag_codes[["plot_supply_pair_table.R"]] <- quote({
  # Supply component comparisons from saved aggregate profiles; no model refitting.
  plot_supply_pair_table <- function(input,country,area_a,area_b,output_dir,html_output='') {
    for(p in c('svglite','jsonlite'))if(!requireNamespace(p,quietly=TRUE))stop('Install ',p)
    x<-readRDS(input)
    pick<-function(g)x$profiles[x$profiles$country==country&x$profiles$side=='Supply'&x$profiles$geo==g,]
    a<-pick(area_a);b<-pick(area_b);stopifnot(nrow(a)==1,nrow(b)==1,a$score>0,b$score>0)
    if(area_a=='MBAHIAKRO')a$area<-'M’Bahiakro'
    if(area_b=='MBAHIAKRO')b$area<-'M’Bahiakro'
    v<-x$components[x$components$country==country&x$components$side=='Supply',]
    fields<-c('offering','free','training','daily_provision','larc_stock','stockout_burden')
    aa<-v[v$geo==area_a,];aa<-aa[match(fields,aa$variable),]
    bb<-v[v$geo==area_b,];bb<-bb[match(fields,bb$variable),]
    stopifnot(nrow(aa)==6,nrow(bb)==6,all(is.finite(aa$risk_z)),all(is.finite(bb$risk_z)),
      abs(mean(aa$risk_z)-a$score)<1e-9,abs(mean(bb$risk_z)-b$score)<1e-9)
    # Composite scores are constraint-aligned. Display component percentages on
    # the favorable scale, with stock continuity, and label the distinct directions.
    for(n in c('aa','bb')){
      q<-get(n);q$readiness_z<--q$risk_z
      q$display_pct<-ifelse(q$variable=='stockout_burden',100-q$raw_pct,q$raw_pct)
      q$display_reference<-ifelse(q$variable=='stockout_burden',100-q$reference_raw_mean,q$reference_raw_mean)
      assign(n,q)
    }
    stopifnot(max(abs(aa$display_reference-bb$display_reference))<1e-9,
      min(aa$n_valid,bb$n_valid)>=20,
      all(aa$display_pct>0&aa$display_pct<100),all(bb$display_pct>0&bb$display_pct<100))
    labels<-c('Short + long-acting offering','Free-product provision','Trained-provider share',
      'Daily provision of offered modern methods','At least one LARC in stock','Stock continuity')
    difference<-abs(aa$display_pct-bb$display_pct)
    lower<-ifelse(aa$display_pct<bb$display_pct,a$area,ifelse(bb$display_pct<aa$display_pct,b$area,'Equal'))
    shade<-function(z)if(z>=-1e-10)'#F5F7F9'else grDevices::colorRampPalette(c('#FFF0D9','#C46024'))(301)[1+round(100*min(-z,3))]
    text<-function(label,x,y,size=12,bold=FALSE,col='#244652',just='left')grid::grid.text(label,x=x,y=y,just=just,
      gp=grid::gpar(fontfamily='sans',fontsize=size,fontface=if(bold)'bold'else'plain',col=col))
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    stem<-paste(country,tolower(area_a),tolower(area_b),'supply_table',sep='_');svg<-file.path(output_dir,paste0(stem,'.svg'))
    svglite::svglite(svg,width=15,height=8.5,bg='white');grid::grid.newpage()
    text('Similar supply constraint scores, different service weaknesses',.025,.955,24,TRUE,col='#173E50')
    for(i in 1:2){q<-list(a,b)[[i]];left<-c(.025,.515)[i]
      grid::grid.roundrect(x=left+.23,y=.815,width=.46,height=.105,r=grid::unit(.012,'npc'),gp=grid::gpar(fill='#EEF4F6',col='#D8E3E8'))
      text(sprintf('%s  |  supply constraint score %+.3f',q$area,q$score),left+.016,.839,17,TRUE)
      text(sprintf('%d of 6 components below average · %s FP facilities',q$unfavorable_components,format(q$fp_facilities,big.mark=',')),left+.016,.795,11,col='#55707D')
    }
    text('Bold = below reference average',.025,.723,11,TRUE)
    text('Shading: farther below average',.315,.723,11)
    for(j in 1:40)grid::grid.rect(x=.570+j*.0021,y=.724,width=.0022,height=.021,gp=grid::gpar(fill=shade(-j*3/40),col=NA))
    text('Blue bars: difference between areas',.705,.723,11,col='#246D95')
    grid::grid.rect(x=.5,y=.664,width=.95,height=.058,gp=grid::gpar(fill='#173E50',col=NA))
    text('Supply component · higher is better',.038,.664,12,TRUE,col='white')
    text('Reference\naverage',.397,.664,11,TRUE,col='white',just='centre')
    text(a$area,.528,.664,12,TRUE,col='white',just='centre');text(b$area,.655,.664,12,TRUE,col='white',just='centre')
    text('Difference · percentage points',.735,.664,11,TRUE,col='white')
    y<-.594-(0:5)*.082
    for(i in seq_along(fields)) {
      grid::grid.rect(x=.5,y=y[i],width=.95,height=.079,gp=grid::gpar(fill=if(i%%2)'#FFFFFF'else'#FAFCFD',col=NA))
      text(labels[i],.038,y[i],12);text(sprintf('%.1f%%',aa$display_reference[i]),.397,y[i],13,col='#6A818C',just='centre')
      for(j in 1:2){q<-list(aa,bb)[[j]];z<-q$readiness_z[i];cx<-c(.528,.655)[j]
        grid::grid.roundrect(x=cx,y=y[i],width=.112,height=.062,r=grid::unit(.007,'npc'),gp=grid::gpar(fill=shade(z),col=NA))
        text(sprintf('%.1f%%',q$display_pct[i]),cx,y[i],15,bold=z< -1e-10,col=if(z<=-2.4)'white'else'#244652',just='centre')
      }
      start<-.735;width<-.221
      grid::grid.roundrect(x=start+width/2,y=y[i]-.019,width=width,height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#E8F0F5',col=NA))
      if(difference[i]>0)grid::grid.roundrect(x=start+width*difference[i]/200,y=y[i]-.019,width=width*difference[i]/100,
        height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#3F86AC',col=NA))
      text(sprintf('%.1f pp · lower in %s',difference[i],lower[i]),start,y[i]+.009,11,col='#276D92')
    }
    grDevices::dev.off()
    vals<-data.frame(component=fields,label=labels,reference_pct=aa$display_reference,
      area_a=a$area,pct_a=aa$display_pct,z_a=aa$readiness_z,below_a=aa$readiness_z< -1e-10,n_a=aa$n_valid,
      area_b=b$area,pct_b=bb$display_pct,z_b=bb$readiness_z,below_b=bb$readiness_z< -1e-10,n_b=bb$n_valid,
      difference_pp=difference,lower_area=lower)
    write.csv(vals,file.path(output_dir,paste0(stem,'_values.csv')),row.names=FALSE)
    if(nzchar(html_output)){
      esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
      rows<-vapply(seq_len(nrow(vals)),function(i)paste0('<tr><td>',esc(vals$label[i]),'</td><td>',sprintf('%.1f%%',vals$reference_pct[i]),
        '</td><td>',if(vals$below_a[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_a[i]),if(vals$below_a[i])'</strong>'else'',
        '</td><td>',if(vals$below_b[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_b[i]),if(vals$below_b[i])'</strong>'else'',
        '</td><td>',sprintf('%.1f pp',vals$difference_pp[i]),' · lower in ',esc(vals$lower_area[i]),'</td><td>',vals$n_a[i],' / ',vals$n_b[i],'</td></tr>'),character(1))
      h<-paste0('<figure><img alt="Supply-readiness percentages, below-reference bolding and percentage-point differences for ',esc(a$area),' and ',esc(b$area),'" src="data:image/svg+xml;base64,',jsonlite::base64_enc(readBin(svg,'raw',n=file.info(svg)$size)),
        '"></figure><details><summary>Definitions and accessible values</summary><p>Higher values mean better readiness in all six rows. Stock continuity is 100 minus the original stockout burden; it retains the underlying UoM product-group denominator and is not the percentage of facilities without any stockout. Bold means below the component reference mean (z &lt; 0). Orange becomes darker as the shortfall increases; shading saturates at z = −3. Blue bars show absolute percentage-point differences on a fixed 0–100 scale, with text naming the lower area.</p><p>Supply summaries include confirmed FP-providing facilities with valid responses separately for each measure. Each facility contributes equally; valid zero/FALSE values are retained, and missing/undefined values are excluded. Method offering and LARC stock are facility percentages; free provision, training, daily provision and stock continuity summarize within-facility proportions. Reference averages give each assessed area equal weight within its geography. Composite scores are the mean of six constraint-aligned z-scores; positive means greater overall supply constraints. The displayed component percentages and their component z-scores run in the opposite, favorable direction. All figures use whole-area profiles, as in the area heatmaps, rather than residence-specific profiles assigned in woman-level models. These descriptive comparisons do not establish causal effects or statistically significant differences.</p><div class="table-wrap"><table><thead><tr><th>Component</th><th>Reference average</th><th>',esc(a$area),'</th><th>',esc(b$area),'</th><th>Difference</th><th>Valid facilities: first / second area</th></tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div></details>')
      writeLines(h,html_output,useBytes=TRUE)
    }
    invisible(svg)
  }

  build_supply_regional_comparisons <- function(input,output_dir,html_output,project_root) {
    source(file.path(project_root,'analysis/plot_demand_pair_table.R'),local=TRUE)
    cases<-data.frame(
      id=c('supply-lagos-types','supply-senegal-concentrated','supply-cdi-concentrated','supply-cdi-types','supply-lagos-breadth','supply-cdi-service-contrast','supply-cdi-offsetting-strengths','supply-cdi-broad-versus-deep'),
      country=c('lagos','senegal','cotedivoire','cotedivoire','lagos','cotedivoire','cotedivoire','cotedivoire'),
      label=c('Lagos','Senegal','Côte d’Ivoire','Côte d’Ivoire','Lagos','Côte d’Ivoire','Côte d’Ivoire','Côte d’Ivoire'),
      a=c('IBEJULEKKI','MATAM','ABIDJAN','BONGOUANOU','AJEROMIIFELODUN','BONGOUANOU','BIANKOUMA','DIVO'),
      b=c('IKORODU','GOUDIRY','GAGNOA','MBAHIAKRO','OJO','TANDA','MEAGUI','BONGOUANOU'),
      comparison_type=c('Different types of challenge','Different breadth and severity',
        'Both — different types of challenge and different breadth and severity',
        'Different types of challenge','Different breadth and severity',
        'Different types of challenge','Different breadth and severity','Different breadth and severity'),
      takeaway=c(
        'Ibeju-Lekki and Ikorodu have nearly identical supply-constraint scores but different weaknesses. Ibeju-Lekki has lower short- and long-acting offering and LARC stock (both 51.4%, versus 74.1% in Ikorodu). Ikorodu has lower free provision (43.9% versus 70.6%) and trained-provider share (53.9% versus 72.4%). Similar totals conceal different service profiles.',
        'Matam’s main shortfall is daily provision: 29.8%, compared with the 76.2% reference average. Its free provision and training are below average by less than one percentage point each. Goudiry has four components below average, including method offering (67.6%) and daily provision (48.6%). This pair shows one pronounced shortfall in Matam alongside broader weaknesses in Goudiry.',
        'Abidjan has five components below average; Gagnoa has three. Gagnoa’s largest shortfall is free provision (62.8%, versus 78.3% in Abidjan), while its other two below-average components are closer to their references. Abidjan has a much lower share of facilities with a LARC in stock (49.5% versus 69.7%). Abidjan’s daily provision is only marginally below average, so five below-average components does not mean five large shortfalls.',
        'Bongouanou and M’Bahiakro have almost identical scores, but their service profiles differ sharply. Bongouanou has lower LARC stock (37.0% versus 85.0%) and stock continuity (16.4% versus 42.1%). M’Bahiakro has lower free provision (62.8% versus 90.9%) and trained-provider share (16.9% versus 45.9%). The same overall readiness level combines different weaknesses.',
        'Ajeromi-Ifelodun is below average across all six components; Ojo is below average on four, with method offering and LARC stock above average. Ojo’s daily provision is lower (17.2% versus 25.1%), while Ajeromi-Ifelodun has lower free provision (34.3% versus 42.9%) and LARC stock (68.9% versus 76.8%). Similar positive constraint scores can reflect weaknesses across all components or a mix of strengths and shortfalls.',
        'Bongouanou’s largest shortfalls are LARC stock (37.0%) and stock continuity (16.4%). Tanda has much lower free provision (62.7% versus 90.9%) and daily provision (36.0% versus 73.0%). Their similar supply-constraint scores conceal different types of service weakness, with three components below average in Bongouanou and five in Tanda.',
        'Biankouma is below average on four components, with modest-to-moderate shortfalls. Méagui has two pronounced shortfalls: method offering is 51.3% against an 80.3% reference average, and daily provision is 35.6% against 66.2%. Its other four components are above average and offset these weaknesses, bringing the overall score close to zero. The relatively favorable total therefore conceals substantial weaknesses in two services.',
        'Divo has five components below average, with modest-to-moderate shortfalls. Bongouanou has two pronounced weaknesses: LARC stock is 37.0% against a 72.4% reference average, and stock continuity is 16.4% against 35.1%. Method offering is also below average, while its other three components are above average and partly offset these weaknesses. Similar overall readiness scores can therefore represent broader moderate weaknesses or fewer, deeper weaknesses balanced by strengths elsewhere.'),stringsAsFactors=FALSE)
    # Explicit display names retain accents and punctuation without altering keys.
    aliases<-c(MBAHIAKRO='M’Bahiakro')
    x<-readRDS(input)
    negative<-x$profiles[x$profiles$side=='Supply'&is.finite(x$profiles$score)&x$profiles$score>1e-10,]
    stopifnot(!any(negative$unfavorable_components==1))
    candidates<-list()
    for(country in unique(negative$country)){
      pp<-negative[negative$country==country,]
      if(nrow(pp)<2)next
      for(pair in combn(seq_len(nrow(pp)),2,simplify=FALSE)){
        a<-pp[pair[1],];b<-pp[pair[2],]
        if(abs(a$score-b$score)>.1+1e-10)next
        aa<-x$components[x$components$side=='Supply'&x$components$country==country&x$components$geo==a$geo,]
        bb<-x$components[x$components$side=='Supply'&x$components$country==country&x$components$geo==b$geo,]
        bb<-bb[match(aa$variable,bb$variable),]
        pa<-ifelse(aa$variable=='stockout_burden',100-aa$raw_pct,aa$raw_pct)
        pb<-ifelse(bb$variable=='stockout_burden',100-bb$raw_pct,bb$raw_pct)
        eligible<-min(a$min_valid,b$min_valid)>=20&all(pa>0&pa<100)&all(pb>0&pb<100)
        candidates[[length(candidates)+1]]<-data.frame(country=country,geo_a=a$geo,geo_b=b$geo,
          supply_constraints_a=a$score,supply_constraints_b=b$score,score_gap=abs(a$score-b$score),
          below_average_a=a$unfavorable_components,below_average_b=b$unfavorable_components,
          minimum_valid=min(a$min_valid,b$min_valid),mean_absolute_pp=mean(abs(pa-pb)),
          maximum_pp=max(abs(pa-pb)),illustration_screen=eligible)
      }
    }
    candidates<-do.call(rbind,candidates)
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    write.csv(candidates,file.path(output_dir,'supply_candidate_pairs.csv'),row.names=FALSE)
    sections<-nav<-character(nrow(cases))
    esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
    for(i in seq_len(nrow(cases))){
      cc<-cases[i,];q<-x$profiles[x$profiles$country==cc$country&x$profiles$side=='Supply'&x$profiles$geo%in%c(cc$a,cc$b),]
      stopifnot(nrow(q)==2,all(q$score>0),abs(diff(q$score))<=.1+1e-10)
      a<-q[q$geo==cc$a,];b<-q[q$geo==cc$b,]
      # Display-name corrections leave geographic keys unchanged.
      if(cc$a%in%names(aliases))a$area<-unname(aliases[cc$a])
      if(cc$b%in%names(aliases))b$area<-unname(aliases[cc$b])
      child<-file.path(output_dir,paste0(cc$id,'.html'))
      plot_supply_pair_table(input,cc$country,cc$a,cc$b,output_dir,child)
      h<-paste(readLines(child,warn=FALSE),collapse='\n')
      finding<-paste0('<div class="finding"><p class="comparison-label"><strong>What this comparison shows:</strong> ',esc(cc$comparison_type),'</p><p><strong>Key finding.</strong> ',esc(cc$takeaway),'</p></div>')
      h<-sub('</figure>',paste0('</figure>',finding),h,fixed=TRUE)
      sections[i]<-paste0('<section id="',cc$id,'" aria-labelledby="',cc$id,'-title"><h2 id="',cc$id,'-title">',i,'. ',esc(cc$label),' · ',esc(a$area),' and ',esc(b$area),'</h2>',h,'</section>')
      nav[i]<-paste0('<a href="#',cc$id,'">Supply ',i,': ',esc(a$area),' / ',esc(b$area),'</a>')
    }
    demand_html<-file.path(output_dir,'demand_comparisons.html')
    plot_regional_comparisons(input,output_dir,demand_html)
    h<-paste(readLines(demand_html,warn=FALSE),collapse='\n')
    h<-sub('<p class="intro">.*?</p>',paste0('<p class="intro">These comparisons show areas with similar overall scores but different challenges. Five demand comparisons pair areas with above-average demand concerns; eight supply comparisons pair areas with below-average supply readiness. <strong>Similar overall scores can reflect different barriers; the component profile shows what the score alone conceals.</strong></p>'),h,perl=TRUE)
    h<-sub('<p class="context">.*?</p>','<p class="context">Demand percentages describe women in need of family planning and use survey weights. Supply percentages summarize confirmed FP-providing facilities, with valid responses separately for each measure and no facility weights. Reference averages give each assessed area equal weight within its geography. Areas may be flagged for both demand and supply concerns.</p>',h,perl=TRUE)
    h<-sub('</nav>',paste0(paste(nav,collapse=''),'</nav>'),h,fixed=TRUE)
    h<-sub('<main>','<main><h2 class="group-title">Demand concerns</h2>',h,fixed=TRUE)
    introduction<-paste0('<div class="group-intro"><h2 class="group-title">Supply constraints</h2><p>Every pair below has positive supply-constraint scores. Higher percentages mean better readiness in all six rows; the stockout row is shown as <strong>stock continuity = 100% − stockout burden</strong>. Bold and orange shading identify values below the component reference average.</p><p>These examples distinguish different types of weakness, broader shortfalls and profiles dominated by one large shortfall.</p></div>')
    h<-sub('</main>',paste0(introduction,paste(sections,collapse=''),'<details><summary>How the supply examples were selected</summary><p>All four geographies were searched using the saved whole-area scores and component values. The eight illustrated pairs have negative readiness scores, an absolute score gap no larger than 0.10 average-z points, at least 20 valid FP facilities per component in each area, and no displayed 0% or 100% component. These screening rules select readable descriptive examples; they are not statistical adequacy thresholds. The pairs illustrate different contrasts rather than a representative sample of areas. No negative-readiness area had exactly one component below average. The illustrated supply pairs are fixed examples from Lagos, Senegal and Côte d’Ivoire; they are not an exhaustive list of qualifying pairs. The original scores, thresholds, classifications and models are unchanged.</p></details></main>'),h,fixed=TRUE)
    h<-sub('</style></head>','</style><style>.group-title{font-size:27px;margin:34px 0 20px;border-bottom:2px solid #D8E3E8;padding-bottom:10px}.group-intro{margin:45px 0 30px;max-width:1150px}.group-intro p{margin:10px 0}@media print{.group-intro{break-before:page}}</style></head>',h,fixed=TRUE)
    # The presentation report keeps its opening paragraph and comparison findings.
    # Detailed methods and value tables remain in standalone previews and source docs.
    h<-sub('(?s)<p class="context">.*?</header>','</header>',h,perl=TRUE)
    h<-gsub('(?s)<details><summary>Definitions and accessible values</summary>.*?</details>','',h,perl=TRUE)
    dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(h,html_output,useBytes=TRUE)
    write.csv(cases[c('id','country','a','b','comparison_type')],file.path(output_dir,'selected_supply_pairs.csv'),row.names=FALSE)
    invisible(cases)
  }

  if(sys.nframe()==0L){
    args<-commandArgs(trailingOnly=TRUE)
    arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
    if('--help'%in%args){cat('Rscript analysis/plot_supply_pair_table.R --input AGGREGATE_PROFILES [--output-dir output/supply_pair_tables] --html-output REPORT\n');quit(status=0)}
    script<-gsub('~+~',' ',sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]),fixed=TRUE)
    root<-normalizePath(file.path(dirname(script),'..'),mustWork=TRUE)
    build_supply_regional_comparisons(arg('--input'),arg('--output-dir','output/supply_pair_tables'),arg('--html-output'),root)
  }

})

# check method awareness
.diag_codes[["check_method_awareness.R"]] <- quote({
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Separate awareness diagnostic; source joins and working reports remain unchanged.
  args <- commandArgs(trailingOnly = TRUE)
  arg <- function(k, default = NULL) {
    i <- match(k, args)
    if (is.na(i)) { if (is.null(default)) stop("Supply ", k); return(default) }
    if (i == length(args)) stop("Supply ", k)
    args[i + 1L]
  }
  if ("--help" %in% args) {
    cat("Rscript analysis/check_method_awareness.R --lagos-dir ANALYSIS --lagos-women-file JOIN --senegal-dir ANALYSIS --senegal-women-file JOIN [--output-dir output/method_awareness_check] [--report-dir reports] [--pandoc-dir DIRECTORY]\n")
    quit(status = 0)
  }
  for (pkg in c("haven", "survey", "clubSandwich", "jsonlite", "ggplot2", "rmarkdown", "base64enc"))
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
  script <- gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE)
  root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
  resolve <- function(p) if (grepl("^(/|[A-Za-z]:)", p)) p else file.path(root, p)
  out <- resolve(arg("--output-dir", "output/method_awareness_check"))
  report_dir <- resolve(arg("--report-dir", "reports"))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  controls <- c("age10", "age10_sq", "education", "partnered", "fertility_goal", "urban")
  other_demand <- c("agency", "stigma", "health", "fertility", "opposes")
  method_names <- c("Condoms", "Injectables", "Daily pill", "Emergency pill", "IUD", "Implant", "Male sterilization", "Female sterilization")
  ka1_order <- c(1, 2, 3, 8, 4, 5, 7, 6)
  wm <- function(v, w) { ok <- is.finite(v) & is.finite(w) & w > 0; survey_weighted_mean(v[ok], w[ok]) }
  wcor <- function(x, y, w) {
    ok <- complete.cases(x, y, w); x <- x[ok]; y <- y[ok]; w <- w[ok]
    mx <- wm(x, w); my <- wm(y, w)
    wm((x - mx) * (y - my), w) / sqrt(wm((x - mx)^2, w) * wm((y - my)^2, w))
  }
  fit_model <- function(fo, d) fit_survey_logistic(fo, d)
  coef_table <- function(f, V) {
    z <- as.data.frame(clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite"))
    data.frame(term = rownames(z), estimate = z$beta, se = z$SE, df = z$df_Satt, p = z$p_Satt,
      odds_ratio = exp(z$beta), lo = exp(z$beta - qt(.975, z$df_Satt) * z$SE), hi = exp(z$beta + qt(.975, z$df_Satt) * z$SE), row.names = NULL)
  }
  vifs <- function(f, d) {
    X <- model.matrix(f)[, -1, drop = FALSE]
    data.frame(term = colnames(X), vif = vapply(seq_len(ncol(X)), function(j) {
      a <- lm.wfit(cbind(1, X[, -j, drop = FALSE]), X[, j], w = d$weight)
      total <- sum(d$weight * (X[, j] - wm(X[, j], d$weight))^2)
      total / sum(d$weight * a$residuals^2)
    }, numeric(1)))
  }
  margin <- function(f, V, d, changes) {
    a <- d
    for (k in names(changes)) a[[k]] <- changes[[k]]
    X <- model.matrix(delete.response(terms(f)), a, contrasts.arg = f$contrasts, xlev = f$xlevels)[, names(coef(f)), drop = FALSE]
    pp <- plogis(drop(X %*% coef(f))); w <- a$weight / sum(a$weight)
    estimate <- survey_weighted_mean(pp, a$weight); gradient <- colSums(X * (w * pp * (1 - pp)))
    direct <- survey_weighted_mean(predict(f, a, type = "response"), w)
    stopifnot(abs(estimate - direct) < 1e-10)
    list(estimate = estimate, gradient = gradient)
  }
  contrast_ci <- function(f, V, estimate, gradient) {
    G <- matrix(gradient, nrow = 1)
    z <- as.data.frame(clubSandwich::linear_contrast(f, vcov = V, contrasts = G, test = "Satterthwaite"))
    df <- as.numeric(z[[4]][1]); se <- sqrt(max(0, drop(G %*% V %*% t(G))))
    data.frame(estimate = estimate, se = se, df = df, lo = estimate - qt(.975, df) * se, hi = estimate + qt(.975, df) * se)
  }
  results <- list(); all_hashes <- list()
  for (country in c("lagos", "senegal")) {
    data_dir <- resolve(arg(paste0("--", country, "-dir")))
    women_file <- resolve(arg(paste0("--", country, "-women-file")))
    source_files <- c(file.path(data_dir, c("analysis_frame.rds", "report_data.json")), women_file)
    hashes <- unname(tools::md5sum(source_files))
    frame <- readRDS(source_files[1]); d <- frame$women
    p <- jsonlite::fromJSON(source_files[2]); x <- readRDS(women_file); names(x) <- tolower(names(x))
    nv <- function(v) as.numeric(haven::zap_labels(v))
    stopifnot(nrow(d) == nrow(x), identical(d$row, seq_len(nrow(x))), all(d$need <= d$positive),
      all(d$need == (d$positive & nv(x$fpn2) %in% 1)))
    raw_weight <- nv(x$weight); stopifnot(max(abs(raw_weight - d$weight), na.rm = TRUE) < 1e-12)
    a <- sapply(ka1_order, function(k) nv(x[[paste0("ka1_", k)]]))
    b <- sapply(letters[1:8], function(k) nv(x[[paste0("ka2", k)]]))
    all_named <- sapply(1:15, function(k) nv(x[[paste0("ka1_", k)]]))
    contradiction <- nv(x$ka1_16) %in% 1 & rowSums(all_named == 1, na.rm = TRUE) > 0
    recognized <- matrix(NA_real_, nrow(d), 8)
    for (j in 1:8) recognized[, j] <- ifelse(a[, j] %in% 1 | b[, j] %in% 1:2, 1,
      ifelse(a[, j] %in% 0 & b[, j] %in% 3, 0, NA_real_))
    raw_count <- rowSums(recognized)
    raw_recall <- ifelse(apply(a, 1, function(v) all(v %in% 0:1)), rowSums(a), NA_real_)
    d$awareness_count <- raw_count; d$recall_count <- raw_recall
    d$awareness_count[contradiction] <- NA_real_; d$recall_count[contradiction] <- NA_real_
    d$conflict <- contradiction
    # Existing MC5 coding is checked independently rather than relabeled as knowledge.
    mc <- sapply(c("mc5a", "mc5b"), function(k) { v <- nv(x[[k]]); ifelse(v %in% 1:4, as.numeric(v %in% 3:4), NA_real_) })
    mc_flag <- ifelse(rowSums(mc == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(mc)) > 0, NA_real_, 0))
    stopifnot(identical(as.numeric(mc_flag), as.numeric(d$confidence)))
    # Retain the exact assigned residence/fallback supply score on the risk scale.
    source(file.path(root, "support/risk_scores.R"))
    d$supply_risk <- supply_constraint_score(frame$supply, p$component_trace)
    d$eligible_description <- d$need
    full <- surgo_design(d[d$positive, ])
    need <- subset(full, eligible_description)
    prevalence <- function(v) {
      aa <- need; aa$variables$.v <- v[d$positive][d$eligible_description[d$positive]]
      aa <- subset(aa, is.finite(.v))
      if (!nrow(aa$variables)) return(c(estimate = NA, lo = NA, hi = NA))
      m <- survey::svymean(~.v, aa); se <- sqrt(vcov(m)[1, 1]); q <- qt(.975, survey::degf(aa))
      c(estimate = unname(coef(m)[1]), lo = unname(coef(m)[1]) - q * se, hi = unname(coef(m)[1]) + q * se)
    }
    summary <- list(country = if (country == "lagos") "Lagos" else "Senegal", need_n = sum(d$need),
      awareness_valid = sum(d$need & is.finite(d$awareness_count)), confidence_valid = sum(d$need & is.finite(d$confidence)),
      contradictions = sum(d$need & contradiction), raw_count_unresolved = sum(d$need & is.na(raw_count)),
      skipped_unnamed_method_cells = sum(a == 0 & b == 95 & d$need, na.rm = TRUE),
      named_but_prompted_cells = sum(a == 1 & b != 95 & d$need, na.rm = TRUE),
      weighted_awareness_mean = prevalence(d$awareness_count), weighted_recall_mean = prevalence(d$recall_count),
      weighted_confidence_pct = 100 * wm(d$confidence[d$need], d$weight[d$need]),
      awareness_confidence_correlation = wcor(d$awareness_count[d$need], d$confidence[d$need], d$weight[d$need]),
      descriptive_psus = length(unique(d$psu[d$need])), geographic_clusters = length(unique(d$geo[d$need])))
    dist <- do.call(rbind, lapply(c("awareness_count", "recall_count"), function(k) do.call(rbind, lapply(0:8, function(v) {
      valid <- d$need & is.finite(d[[k]]); vv <- ifelse(is.finite(d[[k]]), as.numeric(d[[k]] == v), NA_real_)
      e <- prevalence(vv); data.frame(country = summary$country, measure = k, count = v, n = sum(d[[k]][valid] == v), denominator = sum(valid), pct = 100 * e[1], lo = max(0, 100 * e[2]), hi = min(100, 100 * e[3]))
    }))))
    method_distribution <- do.call(rbind, lapply(1:8, function(j) {
      v <- recognized[, j]; v[contradiction] <- NA_real_; e <- prevalence(v)
      data.frame(country = summary$country, method = method_names[j], n_valid = sum(d$need & is.finite(v)), pct = 100 * e[1], lo = max(0, 100 * e[2]), hi = min(100, 100 * e[3]))
    }))
    overlap <- do.call(rbind, lapply(0:8, function(v) {
      ii <- d$need & d$awareness_count %in% v & is.finite(d$confidence)
      data.frame(country = summary$country, count = v, n = sum(ii), low_confidence_n = sum(d$confidence[ii] == 1),
        low_confidence_pct = 100 * wm(d$confidence[ii], d$weight[ii]), weighted_sample_pct = 100 * wm(as.numeric(ii[d$need & complete.cases(d[c("awareness_count", "confidence")])]), d$weight[d$need & complete.cases(d[c("awareness_count", "confidence")])]))
    }))
    mean_by_confidence <- do.call(rbind, lapply(0:1, function(v) {
      vv <- d$awareness_count; vv[!d$confidence %in% v] <- NA_real_; e <- prevalence(vv)
      data.frame(country = summary$country, confidence = v, n = sum(d$need & d$confidence %in% v & is.finite(vv)), mean = e[1], lo = e[2], hi = e[3])
    }))
    base <- c(controls, other_demand, "supply_risk")
    specs <- list(base = base, confidence = c(base, "confidence"), awareness = c(base, "awareness_count"), both = c(base, "confidence", "awareness_count"))
    all_vars <- unique(c("y_modern", unlist(specs)))
    d$eligible <- d$need & complete.cases(d[all_vars]); cc <- droplevels(d[d$eligible, ])
    des <- subset(full, eligible_description)
    # Re-create after adding the common-sample flag to preserve need-domain and common-sample selection.
    full_model <- surgo_design(d[d$positive, ])
    des <- subset(full_model, eligible)
    coefficients <- diagnostics <- cv <- samples <- tests <- fits <- list()
    for (id in names(specs)) {
      fo <- reformulate(specs[[id]], "y_modern"); f <- fit_survey_logistic(fo, cc, design=des)
      V <- geographic_cr2(f, cc$geo)
      ct <- coef_table(f, V); ct$model <- id; ct$country <- summary$country; coefficients[[id]] <- ct
      vi <- vifs(f, cc); vi$model <- id; vi$country <- summary$country; diagnostics[[id]] <- vi
      samples[[id]] <- data.frame(country = summary$country, model = id, n = nrow(cc), events = sum(cc$y_modern), clusters = length(unique(cc$geo)), parameters = length(coef(f)), max_vif = max(vi$vif), warnings = paste(attr(f, "fit_warnings"), collapse = " | "))
      for (k in intersect(c("confidence", "awareness_count"), names(coef(f)))) {
        K <- diag(length(coef(f)))[match(k, names(coef(f))), , drop = FALSE]
        tt <- clubSandwich::Wald_test(f, constraints = K, vcov = V, test = "HTZ")
        tests[[paste(id, k)]] <- data.frame(country = summary$country, model = id, term = k, F = tt$Fstat, df_num = tt$df_num, df_denom = tt$df_denom, p = tt$p_val)
      }
      pred <- rep(NA_real_, nrow(cc))
      for (geo in unique(cc$geo)) {
        train <- cc[cc$geo != geo, ]; test <- cc[cc$geo == geo, ]; ff <- fit_model(fo, train)
        pred[cc$geo == geo] <- predict(ff, newdata = test, type = "response")
      }
      stopifnot(all(is.finite(pred)), all(pred >= 0 & pred <= 1)); clipped <- pmax(1e-8, pmin(1 - 1e-8, pred))
      cv[[id]] <- data.frame(country = summary$country, model = id, brier = wm((cc$y_modern - pred)^2, cc$weight), log_loss = wm(-cc$y_modern * log(clipped) - (1 - cc$y_modern) * log(1 - clipped), cc$weight))
      fits[[id]] <- list(fit = f, V = V)
    }
    ob <- fits$both; gaps <- curves <- list()
    for (k in c("confidence", "awareness_count")) {
      if (k == "confidence") { lo <- margin(ob$fit, ob$V, cc, list(confidence = 0)); hi <- margin(ob$fit, ob$V, cc, list(confidence = 1)) } else {
        lo <- margin(ob$fit, ob$V, cc, list(awareness_count = cc$awareness_count))
        # One extra recognized method among women not already at the ceiling.
        aa <- cc[cc$awareness_count < 8, ]; lo <- margin(ob$fit, ob$V, aa, list(awareness_count = aa$awareness_count))
        hi <- margin(ob$fit, ob$V, aa, list(awareness_count = aa$awareness_count + 1))
      }
      tt <- contrast_ci(ob$fit, ob$V, hi$estimate - lo$estimate, hi$gradient - lo$gradient)
      tt$term <- k; tt$country <- summary$country; tt$probability_from <- lo$estimate; tt$probability_to <- hi$estimate; tt$n_standardized <- if(k == "confidence") nrow(cc) else nrow(aa)
      gaps[[k]] <- tt
    }
    for (v in sort(unique(cc$awareness_count))) for (c in 0:1) {
      a1 <- margin(ob$fit, ob$V, cc, list(awareness_count = v, confidence = c))
      tt <- contrast_ci(ob$fit, ob$V, a1$estimate, a1$gradient); tt$lo <- max(0, tt$lo); tt$hi <- min(1, tt$hi)
      tt$count <- v; tt$confidence <- c; tt$country <- summary$country
      tt$observed_cell_n <- sum(cc$awareness_count == v & cc$confidence == c); curves[[length(curves) + 1L]] <- tt
    }
    sensitivity <- list()
    # Check the spontaneous count and whether the five contradictory Senegal rows matter.
    for (check in c("recall_count", "retain_named_conflicts", "quadratic_awareness")) {
      dd <- d; fo <- reformulate(c(base, "confidence", "awareness_count"), "y_modern")
      if (check == "recall_count") dd$awareness_count <- dd$recall_count
      if (check == "retain_named_conflicts") dd$awareness_count <- raw_count
      if (check == "quadratic_awareness") { dd$awareness_sq <- dd$awareness_count^2; fo <- reformulate(c(base, "confidence", "awareness_count", "awareness_sq"), "y_modern") }
      ii <- dd$need & complete.cases(dd[unique(c("y_modern", all.vars(fo)))]); aa <- droplevels(dd[ii, ])
      ff <- fit_model(fo, aa); VV <- geographic_cr2(ff, aa$geo)
      ct <- coef_table(ff, VV); ct <- ct[ct$term %in% c("confidence", "awareness_count", "awareness_sq"), ]
      ct$check <- check; ct$n <- nrow(aa); ct$country <- summary$country; sensitivity[[check]] <- ct
    }
    summary$model_n <- nrow(cc); summary$model_events <- sum(cc$y_modern); summary$model_excluded <- sum(d$need) - nrow(cc)
    summary$model_awareness_confidence_correlation <- wcor(cc$awareness_count, cc$confidence, cc$weight)
    bind <- function(z) do.call(rbind, z)
    result <- list(summary = summary, distribution = dist, methods = method_distribution, overlap = overlap,
      means = mean_by_confidence, coefficients = bind(coefficients), vifs = bind(diagnostics), tests = bind(tests),
      samples = bind(samples), cv = bind(cv), gaps = bind(gaps), curves = bind(curves), sensitivity = bind(sensitivity))
    for (k in names(result)[-1]) write.csv(result[[k]], file.path(out, paste(country, k, "csv", sep = ".")), row.names = FALSE, na = "")
    stopifnot(identical(hashes, unname(tools::md5sum(source_files))))
    result$summary$input_hashes <- as.list(setNames(hashes, c("analysis_frame", "report_data", "woman_join")))
    result$summary$main_sources_unchanged <- TRUE
    results[[country]] <- result
    cat(summary$country, ": need N=", summary$need_n, "; awareness valid=", summary$awareness_valid,
      "; common model N=", summary$model_n, "; correlation=", round(summary$awareness_confidence_correlation, 3), "\n", sep = "")
  }
  joint_tests <- do.call(rbind, lapply(results, function(r) r$tests[r$tests$model == "both", ]))
  joint_tests$holm_p_four_tests <- p.adjust(joint_tests$p, "holm")
  write.csv(joint_tests, file.path(out, "joint_model_tests.csv"), row.names = FALSE)
  summary <- lapply(results, `[[`, "summary")
  jsonlite::write_json(summary, file.path(out, "verification.json"), auto_unbox = TRUE, pretty = TRUE, digits = 12)
  saveRDS(results, file.path(out, "aggregate_results.rds"))
  plot_theme <- ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#173d4d"),
    panel.grid.minor = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold"), legend.position = "bottom")
  cols <- c(Lagos = "#087d8a", Senegal = "#ba6634")
  save_plot <- function(plot, name, height = 5.1) ggplot2::ggsave(file.path(out, name), plot, width = 11, height = height, dpi = 180, bg = "white")
  dist <- do.call(rbind, lapply(results, `[[`, "distribution")); dist <- dist[dist$measure == "awareness_count", ]
  g <- ggplot2::ggplot(dist, ggplot2::aes(count, pct, fill = country)) + ggplot2::geom_col(width = .72, show.legend = FALSE) +
    ggplot2::facet_wrap(~country) + ggplot2::scale_fill_manual(values = cols) + ggplot2::scale_x_continuous(breaks = 0:8) +
    ggplot2::labs(title = "Women recognize different numbers of modern methods", subtitle = "Spontaneous naming plus picture recognition; before an explanation", x = "Modern methods recognized (0–8)", y = "Weighted share of women in need (%)") + plot_theme
  save_plot(g, "awareness_distribution.png")
  means <- do.call(rbind, lapply(results, `[[`, "means")); means$group <- ifelse(means$confidence == 1, "Low confidence", "Somewhat / very confident on both items")
  g <- ggplot2::ggplot(means, ggplot2::aes(mean, group, color = country)) + ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), orientation = "y", width = .16, linewidth = .7) +
    ggplot2::geom_point(size = 3.5) + ggplot2::facet_wrap(~country) + ggplot2::scale_color_manual(values = cols) +
    ggplot2::labs(title = "Awareness and confidence are related, but distinct", subtitle = "Survey-weighted mean recognized methods and 95% confidence intervals", x = "Modern methods recognized (0–8)", y = NULL, color = NULL) + plot_theme + ggplot2::theme(legend.position = "none")
  save_plot(g, "awareness_confidence_overlap.png", 3.8)
  ct <- do.call(rbind, lapply(results, `[[`, "coefficients")); ct <- ct[ct$model %in% c("confidence", "awareness", "both") & ct$term %in% c("confidence", "awareness_count"), ]
  ct$label <- ifelse(ct$term == "confidence", "Low confidence (yes vs no)", "One additional method recognized")
  ct$spec <- factor(ct$model, levels = c("confidence", "awareness", "both"), labels = c("Confidence only", "Awareness only", "Both included"))
  pd <- ggplot2::position_dodge(width = .55)
  g <- ggplot2::ggplot(ct, ggplot2::aes(odds_ratio, label, color = spec)) + ggplot2::geom_vline(xintercept = 1, color = "#879aa4", linetype = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), orientation = "y", width = .14, position = pd) + ggplot2::geom_point(position = pd, size = 2.8) +
    ggplot2::facet_wrap(~country) + ggplot2::scale_x_log10(breaks = c(.5, .75, 1, 1.25, 1.5), labels = c("0.50", "0.75", "1.00", "1.25", "1.50")) + ggplot2::scale_color_manual(values = c("#8b939c", "#ba6634", "#087d8a")) +
    ggplot2::labs(title = "Adjusted associations with modern contraceptive use", subtitle = "All comparisons use the same women within each setting; geographic CR2 95% intervals", x = "Adjusted odds ratio (log scale)", y = NULL, color = NULL) + plot_theme
  save_plot(g, "awareness_adjusted_associations.png", 4.4)
  prepared <- normalizePath(out, mustWork = TRUE); report_dir <- normalizePath(report_dir, mustWork = TRUE)
  pandoc <- arg("--pandoc-dir", Sys.getenv("RSTUDIO_PANDOC", "")); if (nzchar(pandoc)) Sys.setenv(RSTUDIO_PANDOC = pandoc)
  rmarkdown::render(file.path(root, "data_checks/check_method_awareness.Rmd"), output_file = "Method_awareness_check_Senegal_Lagos.html", output_dir = report_dir,
    params = list(prepared_dir = prepared), quiet = TRUE, envir = new.env(parent = globalenv()))
  cat("Saved separate Method_awareness_check_Senegal_Lagos.html; main reports were not modified.\n")

})

# plot score profiles combined
.diag_codes[["plot_score_profiles_combined.R"]] <- quote({
  # Plot aligned component percentages beside their contributions to an existing score.
  # Input is the aggregate-only diagnostic produced by score_profile_examples.R.
  plot_score_profiles_combined <- function(input, case_id, output_dir) {
    for(pkg in c('ggplot2','svglite'))if(!requireNamespace(pkg,quietly=TRUE))stop('Install ',pkg)
    x<-readRDS(input);q<-x$selected[[case_id]]
    if(is.null(q))stop('Unknown case: ',case_id)
    v<-q$components;areas<-c(q$a$geo,q$b$geo);area_names<-c(q$a$area,q$b$area)
    fields<-v$variable[v$geo==areas[1]];side<-as.character(q$case$side)
    stopifnot(length(fields)==6,nrow(v)==12,all(is.finite(v$raw_pct)),all(is.finite(v$contribution)))
    for(i in seq_along(areas))stopifnot(abs(sum(v$contribution[v$geo==areas[i]])-c(q$a$score,q$b$score)[i])<1e-10)
    v$percentage<-ifelse(side=='Supply'&!v$higher_raw_is_worse,100-v$raw_pct,v$raw_pct)
    v$reference<-ifelse(side=='Supply'&!v$higher_raw_is_worse,100-v$reference_raw_mean,v$reference_raw_mean)
    v$row<-7-match(v$variable,fields);v$area<-factor(v$geo,levels=areas,labels=area_names)
    v$y<-v$row+ifelse(v$geo==areas[1],.15,-.15)
    v$percentage_label<-sprintf('%.1f%%',v$percentage)
    v$contribution_label<-sprintf('%+.3f',v$contribution)
    reference<-v[v$geo==areas[1],]
    labels<-if(side=='Supply')c(offering='Short + long-acting offering\nGap: facilities not meeting standard (%)',
      free='Free-product provision\nGap: facility-average non-free share (%)',
      training='Trained-provider share\nGap: facility-average untrained share (%)',
      daily_provision='Daily provision of offered modern methods\nGap: facility-average non-daily share (%)',
      larc_stock='At least one LARC in stock\nGap: facilities without a stocked LARC (%)',
      stockout_burden='Stockout burden\nOriginal facility-average burden (%)')else setNames(v$label[match(fields,v$variable)],fields)
    if(side=='Demand')labels<-c(agency='Healthcare permission barriers',stigma='Anticipated social stigma',
      health='Health / side-effect beliefs',fertility='Future-fertility concerns',
      confidence='Low confidence choosing a method\nor managing side effects',opposes='Opposes family planning')
    colors<-setNames(c('#16768A','#C2782B'),area_names)
    theme<-ggplot2::theme_minimal(base_family='sans',base_size=11)+ggplot2::theme(
      panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor=ggplot2::element_blank(),
      panel.grid.major.x=ggplot2::element_line(color='#E5ECEF',linewidth=.4),
      plot.title=ggplot2::element_text(face='bold',size=14,color='#173E50'),
      plot.subtitle=ggplot2::element_text(size=10.5,color='#526D79'),
      axis.title=ggplot2::element_text(size=11,color='#294D5D'),
      axis.text=ggplot2::element_text(color='#375866',size=10.5),
      axis.text.y=ggplot2::element_text(lineheight=1.1),legend.position='none',
      plot.margin=ggplot2::margin(8,18,8,8))
    left<-ggplot2::ggplot(v,ggplot2::aes(percentage,y,color=area,shape=area))+
      ggplot2::geom_segment(data=reference,ggplot2::aes(x=reference,xend=reference,y=row-.34,yend=row+.34),
        inherit.aes=FALSE,color='#687E88',linewidth=.75)+
      ggplot2::geom_point(size=3.1)+
      ggplot2::geom_text(ggplot2::aes(label=percentage_label),hjust=0,nudge_x=2.2,size=3.2,show.legend=FALSE)+
      ggplot2::scale_color_manual(values=colors)+ggplot2::scale_shape_manual(values=c(16,18))+
      ggplot2::scale_y_continuous(limits=c(.4,6.6),breaks=6:1,labels=unname(labels[fields]),expand=c(0,0))+
      ggplot2::scale_x_continuous(limits=c(0,100),breaks=seq(0,100,25),expand=c(0,0))+
      ggplot2::labs(title=if(side=='Demand')'A. What women report'else'A. The underlying percentages',
        subtitle=if(side=='Demand')'Weighted % of women in need; higher = more concern'else'Higher = more challenging; grey ticks show the reference',
        x=if(side=='Demand')'Women reporting the concern (%)'else'Challenge measure (%)',y=NULL)+theme
    extent<-range(c(0,v$contribution));padding<-diff(extent)*.20
    right<-ggplot2::ggplot(v)+
      ggplot2::geom_vline(xintercept=0,color='#728A94',linewidth=.6)+
      ggplot2::geom_rect(ggplot2::aes(xmin=pmin(0,contribution),xmax=pmax(0,contribution),ymin=y-.075,ymax=y+.075,fill=area),color=NA)+
      ggplot2::geom_text(ggplot2::aes(x=contribution,y=y,label=contribution_label,
        hjust=ifelse(contribution>=0,-.13,1.13),color=area),size=3.1,show.legend=FALSE)+
      ggplot2::scale_fill_manual(values=colors)+ggplot2::scale_color_manual(values=colors)+
      ggplot2::scale_y_continuous(limits=c(.4,6.6),breaks=6:1,labels=NULL,expand=c(0,0))+
      ggplot2::scale_x_continuous(limits=extent+c(-padding,padding),breaks=c(-.2,0,.2,.4),expand=c(0,0))+
      ggplot2::labs(title=if(side=='Demand')'B. Why the overall scores look similar'else'B. How each measure enters the score',
        subtitle='Left of zero reduces the total; right of zero increases it',
        x=if(side=='Demand')'Contribution to the overall concern score'else'Contribution to challenge score (z / 6)',y=NULL)+theme+
      ggplot2::theme(axis.ticks.y=ggplot2::element_blank(),plot.margin=ggplot2::margin(8,20,8,12))
    gl<-ggplot2::ggplotGrob(left);gr<-ggplot2::ggplotGrob(right)
    stopifnot(length(gl$heights)==length(gr$heights));height<-grid::unit.pmax(gl$heights,gr$heights)
    gl$heights<-height;gr$heights<-height
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    svg<-file.path(output_dir,paste0(case_id,'_percentages_and_contributions.svg'))
    svglite::svglite(svg,width=16,height=7.6,bg='white')
    grid::grid.newpage();grid::pushViewport(grid::viewport(layout=grid::grid.layout(3,2,
      widths=grid::unit(c(.60,.40),'null'),heights=grid::unit(c(.14,.74,.12),'null'))))
    grid::pushViewport(grid::viewport(layout.pos.row=1,layout.pos.col=1:2))
    grid::grid.text(if(side=='Demand')paste0(q$a$country_label,': similar overall scores conceal different barriers')else paste0(q$a$country_label,': similar ',tolower(side),' scores, different component patterns'),
      x=.015,y=.79,just='left',gp=grid::gpar(fontsize=22,fontface='bold',col='#173E50'))
    for(i in 1:2) {
      xx<-c(.02,.255)[i]
      grid::grid.points(x=xx,y=.30,pch=c(16,18)[i],size=grid::unit(3,'mm'),gp=grid::gpar(col=colors[i],fill=colors[i]))
      grid::grid.text(sprintf(if(side=='Demand')'%s  |  overall concern score %+.3f'else'%s  |  total challenge score %+.3f',area_names[i],c(q$a$score,q$b$score)[i]),
        x=xx+.012,y=.30,just='left',gp=grid::gpar(fontsize=11.5,col='#294D5D'))
    }
    grid::grid.lines(x=c(.52,.52),y=c(.22,.38),gp=grid::gpar(col='#687E88',lwd=1.1))
    grid::grid.text(paste0('Reference: mean across ',q$a$reference_areas,' surveyed areas'),x=.535,y=.30,
      just='left',gp=grid::gpar(fontsize=11,col='#526D79'))
    grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row=2,layout.pos.col=1));grid::grid.draw(gl);grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row=2,layout.pos.col=2));grid::grid.draw(gr);grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row=3,layout.pos.col=1:2))
    caption<-if(side=='Supply')c(
      'First five rows show gaps: 100 minus the heatmap percentage. Stockout burden keeps its original percentage; higher = worse throughout.',
      'Offering/LARC gaps are percentages of FP facilities; other gaps/burden are averages of within-facility shares, using valid responses.',
      'Reference averages give each surveyed area equal weight. Challenge = negative readiness; the original scores and classifications are unchanged.')else c(
      sprintf('Women in need: %s n=%s; %s n=%s. Percentages use survey weights and valid responses separately for each domain.',area_names[1],q$a$n_need,area_names[2],q$b$n_need),
      'Grey ticks show equal-area averages across the surveyed départements. Each bar is the standardized component divided by six.',
      'The confidence measure is unchanged. These figures describe reported concerns and index composition, not causal effects on contraceptive use.')
    for(i in seq_along(caption))grid::grid.text(caption[i],x=.015,y=.78-(i-1)*.27,just='left',
      gp=grid::gpar(fontsize=10,col='#526D79'))
    grid::popViewport(2);grDevices::dev.off()
    stopifnot(all(v$percentage>=0&v$percentage<=100),all(v$reference>=0&v$reference<=100))
    write.csv(v[c('geo','area','variable','raw_pct','percentage','reference','risk_z','contribution','n_valid')],
      file.path(output_dir,paste0(case_id,'_combined_plot_values.csv')),row.names=FALSE)
    cat('Saved combined percentage and contribution figure for ',case_id,'\n',sep='')
    invisible(svg)
  }

  plot_score_profile_percentages <- function(input,case_id,output_dir,supply_score='challenge',supply_percentages='gaps') {
    supply_score<-match.arg(supply_score,c('challenge','readiness'))
    supply_percentages<-match.arg(supply_percentages,c('gaps','original'))
    for(pkg in c('ggplot2','svglite'))if(!requireNamespace(pkg,quietly=TRUE))stop('Install ',pkg)
    x<-readRDS(input);q<-x$selected[[case_id]]
    if(is.null(q)||!q$case$side%in%c('Demand','Supply'))stop('Select a saved demand or supply example')
    side<-as.character(q$case$side)
    display_scores<-c(q$a$score,q$b$score)*if(side=='Supply'&&supply_score=='readiness')-1 else 1
    v<-q$components;areas<-c(q$a$geo,q$b$geo);area_names<-c(q$a$area,q$b$area)
    fields<-v$variable[v$geo==areas[1]]
    stopifnot(length(fields)==6,nrow(v)==12,all(is.finite(v$raw_pct)),all(v$raw_pct>=0&v$raw_pct<=100))
    v$percentage<-ifelse(side=='Supply'&supply_percentages=='gaps'&!v$higher_raw_is_worse,100-v$raw_pct,v$raw_pct)
    v$row<-7-match(v$variable,fields);v$area<-factor(v$geo,levels=areas,labels=area_names)
    v$y<-v$row+ifelse(v$geo==areas[1],.16,-.16)
    labels<-c(agency='Healthcare permission barriers',stigma='Anticipated social stigma',
      health='Health / side-effect beliefs',fertility='Future-fertility concerns',
      confidence='Low confidence choosing a method\nor managing side effects',opposes='Opposes family planning')
    if(side=='Supply')labels<-c(offering='Short + long-acting offering\nGap: facilities not meeting standard',
      free='Free-product provision\nGap: average non-free product share',training='Trained-provider share\nGap: average untrained share',
      daily_provision='Daily provision of offered modern methods\nGap: average non-daily share',
      larc_stock='At least one LARC in stock\nGap: facilities without a stocked LARC',stockout_burden='Stockout burden')
    if(side=='Supply'&&supply_percentages=='original')labels<-c(
      offering='Short + long-acting offering\nHigher is better',free='Free-product provision\nHigher is better',
      training='Trained-provider share\nHigher is better',daily_provision='Daily provision of offered modern methods\nHigher is better',
      larc_stock='At least one LARC in stock\nHigher is better',stockout_burden='Stockout burden\nLower is better')
    colors<-setNames(c('#16768A','#C2782B'),area_names)
    v$text_color<-ifelse(v$percentage>92,'#FFFFFF',unname(colors[as.character(v$area)]))
    g<-ggplot2::ggplot(v)+
      ggplot2::geom_rect(ggplot2::aes(xmin=0,xmax=percentage,ymin=y-.115,ymax=y+.115,fill=area),color=NA)+
      ggplot2::geom_text(ggplot2::aes(x=pmin(percentage+1.3,98.5),y=y,label=sprintf('%.1f%%',percentage),color=text_color,
        hjust=ifelse(percentage>92,1,0)),
        size=4.1,show.legend=FALSE)+
      ggplot2::scale_fill_manual(values=colors)+ggplot2::scale_color_identity()+
      ggplot2::scale_y_continuous(limits=c(.45,6.55),breaks=6:1,labels=unname(labels[fields]),expand=c(0,0))+
      ggplot2::scale_x_continuous(limits=c(0,100),breaks=seq(0,100,25),labels=function(v)paste0(v,'%'),expand=c(0,0))+
      ggplot2::labs(x=if(side=='Demand')'Women reporting the concern · weighted %'else if(supply_percentages=='original')'Original heatmap percentages (%)'else'Service gaps or stockout burden · higher = worse (%)',y=NULL)+
      ggplot2::theme_minimal(base_family='sans',base_size=13)+ggplot2::theme(
        panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor=ggplot2::element_blank(),
        panel.grid.major.x=ggplot2::element_line(color='#E3EAED',linewidth=.4),
        axis.text=ggplot2::element_text(color='#31515F',size=12.5),
        axis.text.y=ggplot2::element_text(lineheight=1.1),axis.title.x=ggplot2::element_text(color='#31515F',size=12.5),
        legend.position='none',plot.margin=ggplot2::margin(5,35,8,15))
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    svg<-file.path(output_dir,paste0(case_id,'_presentation.svg'))
    svglite::svglite(svg,width=14,height=8,bg='white');grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout=grid::grid.layout(2,1,heights=grid::unit(c(.20,.80),'null'))))
    grid::pushViewport(grid::viewport(layout.pos.row=1))
    title<-if(!is.null(q$plot_title))q$plot_title else if(side=='Demand')'Similar overall scores, different reported concerns'else if(supply_score=='readiness'&&supply_percentages=='original')'Similar readiness scores, different supply profiles'else if(supply_score=='readiness')'Similar readiness scores, different service gaps'else'Similar supply scores, different service weaknesses'
    grid::grid.text(title,x=.025,y=.82,just='left',
      gp=grid::gpar(fontsize=23,fontface='bold',col='#173E50'))
    grid::grid.text(paste0(q$a$country_label,if(side=='Demand')' · women in need of family planning'else' · confirmed FP-provider facility summaries'),x=.025,y=.52,just='left',
      gp=grid::gpar(fontsize=13,col='#526D79'))
    for(i in 1:2) {
      xx<-c(.03,.49)[i]
      grid::grid.rect(x=xx,y=.17,width=.012,height=.11,gp=grid::gpar(fill=colors[i],col=NA))
      grid::grid.text(sprintf(if(side=='Demand')'%s  |  overall concern score %+.3f'else if(supply_score=='readiness')'%s  |  supply readiness score %+.3f'else'%s  |  supply constraint score %+.3f',area_names[i],display_scores[i]),
        x=xx+.014,y=.17,just='left',gp=grid::gpar(fontsize=13,col='#294D5D'))
    }
    grid::popViewport();grid::pushViewport(grid::viewport(layout.pos.row=2));grid::grid.draw(ggplot2::ggplotGrob(g));grid::popViewport()
    grid::popViewport();grDevices::dev.off()
    write.csv(v[c('geo','area','variable','raw_pct','percentage','n_valid')],file.path(output_dir,paste0(case_id,'_presentation_values.csv')),row.names=FALSE)
    cat('Saved percentage-only presentation figure for ',case_id,'\n',sep='')
    invisible(svg)
  }

  if(sys.nframe()==0L) {
    args<-commandArgs(trailingOnly=TRUE)
    arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
    if('--help'%in%args){cat('Rscript analysis/plot_score_profiles_combined.R --input AGGREGATE_EXAMPLES --case CASE [--view combined|percentages] [--supply-score challenge|readiness] [--supply-percentages gaps|original] [--output-dir output/combined_score_profiles]\n');quit(status=0)}
    view<-arg('--view','combined')
    if(!view%in%c('combined','percentages'))stop('Use --view combined or percentages')
    if(view=='percentages')plot_score_profile_percentages(arg('--input'),arg('--case','senegal_supply'),
      arg('--output-dir','output/combined_score_profiles'),arg('--supply-score','challenge'),arg('--supply-percentages','gaps'))else {
      if(arg('--supply-score','challenge')!='challenge'||arg('--supply-percentages','gaps')!='gaps')stop('Supply display options are available in the percentages view')
      plot_score_profiles_combined(arg('--input'),arg('--case','senegal_supply'),arg('--output-dir','output/combined_score_profiles'))
    }
  }

})

# senegal update data checks
.diag_codes[["senegal_update_data_checks.R"]] <- quote({
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

})

# cotedivoire update data checks
.diag_codes[["cotedivoire_update_data_checks.R"]] <- quote({
  # Shared survey-weighted estimation and geographic regression inference.
  .survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
  .survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
  while (!file.exists(file.path(.survey_root, "support/survey_methods.R"))) {
    .survey_parent <- dirname(.survey_root)
    if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
    .survey_root <- .survey_parent
  }
  source(file.path(.survey_root, "support/survey_methods.R"), local = TRUE)

  # Append aggregate-only Côte d’Ivoire residence fallback checks; preserve prior sections.
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop("Supply ",k);return(default)};if(i==length(args))stop("Supply ",k);args[i+1L]}
  base_html<-arg("--base-html");data_dir<-arg("--data-dir");output<-arg("--output");audit_json<-arg("--audit-json","")
  frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
  coverage<-read.csv(file.path(data_dir,"residence_supply_profiles_and_coverage.csv"),stringsAsFactors=FALSE)
  need<-d$need;total<-sum(need)
  stopifnot(nrow(d)==10859,total==4292,all(d$residence[need]%in%c("Rural","Urban")),
   all(is.finite(d$weight[need])&d$weight[need]>0),!anyNA(d$fallback),
   sum(need&!d$has_fp_supply)==49,sum(need&!complete.cases(frame$supply))==92)
  for(i in seq_len(nrow(coverage))){
   selected<-d$geo%in%coverage$geo[i]&d$residence%in%coverage$residence[i]
   expected_fallback<-coverage$fp_facilities[i]==0&&coverage$assigned_fp_facilities[i]>0
   stopifnot(sum(selected)==coverage$women[i],sum(selected&need)==coverage$women_in_need[i],
    all(d$fallback[selected]==coverage$fallback[i]),coverage$fallback[i]==expected_fallback)
  }
  # Counts are unweighted; percentages use the specified need-group denominator.
  percent<-function(selected,pop)if(sum(pop))100*sum(selected&pop)/sum(pop)else NA_real_
  wpercent<-function(selected,pop)if(sum(pop))100*survey_weighted_mean(as.numeric(selected[pop]),d$weight[pop])else NA_real_
  stat<-function(selected,pop)list(n=sum(selected&pop),denominator=sum(pop),
   sample_pct=percent(selected,pop),weighted_pct=wpercent(selected,pop),
   sample_pct_all=percent(selected,need),weighted_pct_all=wpercent(selected,need))
  name<-function(g)tools::toTitleCase(tolower(unique(d$district[d$geo%in%g])))
  esc<-function(x){x<-gsub("&","&amp;",as.character(x),fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
  num<-function(v)format(v,big.mark=",",scientific=FALSE,trim=TRUE)
  pct<-function(v)if(is.na(v))"—"else sprintf("%.2f%%",v)
  tab<-function(id,headers,rows){
   paste0('<div class="table-wrap"><table class="supply-coverage" id="',id,'"><thead><tr><th>',
   paste(esc(headers),collapse="</th><th>"),"</th></tr></thead><tbody>",
   paste(vapply(rows,function(x)paste0("<tr><td>",paste(esc(x),collapse="</td><td>"),"</td></tr>"),character(1)),collapse="\n"),
   "</tbody></table></div>")
  }
  section<-paste0('\n<!-- BEGIN CDI RESIDENCE CHECKS -->\n<section id="cdi-residence-fallbacks">',
   '<h2>Côte d’Ivoire: women in need and residence-specific supply fallback</h2>',
   '<p>Population: <strong>4,292 women in need of FP</strong>, using original SURGO <code>FPN2 = 1</code>, consent and positive <code>WEIGHT</code>. Facility summaries include general FP providers (<code>s2_5_sp = 1</code>), with valid answers separately for each measure. Each woman receives her own département–Rural/Urban profile. Only a cell with zero eligible FP providers can trigger a flagged whole-département fallback when that broader profile exists.</p>',
   '<div class="decision"><strong>No fallback was required</strong><p>All observed residence groups in linked départements have eligible FP facilities. No women in need received a département-wide default. The unmatched cases below lack a département profile too, so no fallback was available.</p></div>')
  summary_rows<-lapply(c("Rural","Urban"),function(res){
   pop<-need&d$residence%in%res;v<-stat(pop&d$fallback,pop)
   c(res,num(sum(pop)),num(v$n),pct(v$weighted_pct))
  })
  summary_total<-stat(need&d$fallback,need)
  summary_rows<-c(summary_rows,list(c("Total",num(total),num(summary_total$n),pct(summary_total$weighted_pct))))
  section<-paste0(section,'<h3 style="font-size:17px;color:#185664">Rural/Urban fallback summary</h3>',
   tab("cdi-fallback-need-summary",c("Residence","Women in need, n","Received département-wide fallback, n","Weighted % receiving fallback"),summary_rows),
   '<p class="note">Counts are unweighted. Each residence percentage uses the corresponding Rural or Urban need-group denominator; the total uses all 4,292 women in need.</p>')
  results<-list()
  for(res in c("Rural","Urban")){
   pop<-need&d$residence%in%res;selected<-pop&d$fallback
   geos<-sort(unique(d$geo[selected]));s<-stat(selected,pop)
   rr<-lapply(geos,function(g){v<-stat(selected&d$geo%in%g,pop);c(name(g),num(v$n),pct(v$sample_pct),pct(v$weighted_pct),pct(v$sample_pct_all),pct(v$weighted_pct_all))})
   if(!length(rr))rr<-list(c("None — no fallback required","0","0.00%","0.00%","0.00%","0.00%"))
   rr<-c(rr,list(c("Total",num(s$n),pct(s$sample_pct),pct(s$weighted_pct),pct(s$sample_pct_all),pct(s$weighted_pct_all))))
   section<-paste0(section,'<h3 style="font-size:17px;color:#185664">',res,' women in need receiving département-wide averages</h3>',
   '<p class="note">Residence denominator: ',num(sum(pop)),' ',tolower(res),' women in need; overall denominator: ',num(total),' women in need.</p>',
   tab(paste0("cdi-",tolower(res),"-fallback-need"),c("Département","Women receiving fallback, n",paste0("Sample % of ",res," women in need"),paste0("Weighted % of ",res," women in need"),"Sample % of all women in need","Weighted % of all women in need"),rr))
   results[[res]]<-s
  }
  unmatched<-need&!d$has_fp_supply;geos<-sort(unique(d$geo[unmatched]))
  unmatched_rows<-lapply(geos,function(g){ii<-unmatched&d$geo%in%g;s<-stat(ii,need)
   data.frame(departement=name(g),rural_n=sum(ii&d$residence%in%"Rural"),urban_n=sum(ii&d$residence%in%"Urban"),n=s$n,sample_pct=s$sample_pct,weighted_pct=s$weighted_pct)})
  u<-do.call(rbind,unmatched_rows);stopifnot(sum(u$n)==49,sum(u$rural_n)==34,sum(u$urban_n)==15)
  ut<-stat(unmatched,need)
  rows<-lapply(seq_len(nrow(u)),function(i)c(u$departement[i],num(u$rural_n[i]),num(u$urban_n[i]),num(u$n[i]),pct(u$sample_pct[i]),pct(u$weighted_pct[i])))
  rows<-c(rows,list(c("Total",num(sum(u$rural_n)),num(sum(u$urban_n)),num(ut$n),pct(ut$sample_pct),pct(ut$weighted_pct))))
  section<-paste0(section,'<h3 style="font-size:17px;color:#185664">Separate issue: unmatched women, not fallback recipients</h3>',
   '<p>These départements have no linked facility profile at either resolution. Supply remains unknown; zero or another département’s averages are not substituted.</p>',
   tab("cdi-unmatched-need-residence",c("Département","Rural women in need, n","Urban women in need, n","Total women in need, n","Sample % of all women in need","Weighted % of all women in need"),rows),
   '<p class="note">An additional 43 women in need in Daoukro (22 Rural, 21 Urban) receive their exact residence profile, but free-product and daily-provision values are undefined. Existing FP facilities with an undefined component do not trigger an empty-cell fallback. Thus 49 unmatched plus 43 partially defined assignments give 92 women without all six supply measures. All remain in the joined dataset and need-group overview.</p>',
   '<p class="note"><strong>Denominators.</strong> Need-group counts are 1,715 Rural and 2,577 Urban. Sample percentages divide respondent counts; weighted percentages use original SURGO WEIGHT. These are full need-group denominators, not fitted-model complete cases. The zero fallback percentages are identical under both conventions. This addition does not change supply assignments, scores, classifications or fitted models.</p>',
   '</section>\n<!-- END CDI RESIDENCE CHECKS -->\n')
  base<-paste(readLines(base_html,warn=FALSE,encoding="UTF-8"),collapse="\n")
  base<-gsub('(?s)\n<!-- BEGIN CDI RESIDENCE CHECKS -->.*?<!-- END CDI RESIDENCE CHECKS -->\n?',"\n",base,perl=TRUE)
  stopifnot(length(gregexpr("<footer",base,fixed=TRUE)[[1]])==1,grepl("<footer",base,fixed=TRUE))
  base<-sub("\\s*<footer","<footer",base,perl=TRUE)
  updated<-sub("<footer",paste0(section,"\n<footer"),base,fixed=TRUE)
  dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE);writeLines(updated,output,useBytes=TRUE)
  if(nzchar(audit_json)){
   dir.create(dirname(audit_json),recursive=TRUE,showWarnings=FALSE)
   jsonlite::write_json(list(need_n=total,residence=results,unmatched=u,unmatched_total=ut,
    exact_profile_but_incomplete_need=43,all_incomplete_supply_need=92,
    policy="Flagged whole-département fallback only for zero exact-cell FP providers when a whole-département profile exists; none needed here."),
    audit_json,auto_unbox=TRUE,pretty=TRUE,dataframe="rows",na="null",digits=12)
  }
  cat("Added Côte d’Ivoire need-group Rural/Urban fallback tables (zero cases) and separate unmatched-area coverage.\n")

})

run_diagnostic <- function(key,args) {
  e<-new.env(parent=globalenv())
  e$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)args else c("Rscript",paste0("--file=",.report_file))
  e$sys.nframe<-function()0L
  e$source<-function(file,local=FALSE,...) {
    target<-if(is.environment(local))local else if(isTRUE(local))parent.frame()else e
    name<-basename(file)
    if(name %in% names(.diag_codes)) {
      previous<-get0("sys.nframe",envir=target,inherits=FALSE)
      assign("sys.nframe",function()1L,envir=target)
      on.exit(if(is.null(previous))rm("sys.nframe",envir=target)else assign("sys.nframe",previous,envir=target),add=TRUE)
      return(eval(.diag_codes[[name]],target))
    }
    common<-c("survey_methods.R","risk_scores.R","runtime.R")
    if(name %in% common)return(base::source(file.path(.report_root,"support",name),local=target))
    stop("Undocumented diagnostic dependency: ",name)
  }
  eval(.diag_codes[[key]],e)
}
build_all_checks <- function(work,cache,output,python="python3") {
  work<-project_path(work,.report_root);cache<-project_path(cache,.report_root);output<-project_path(output,.report_root)
  report_dir<-file.path(output,"generated_checks");checks_dir<-file.path(output,"data_checks")
  dir.create(report_dir,recursive=TRUE,showWarnings=FALSE);dir.create(checks_dir,recursive=TRUE,showWarnings=FALSE)
  admin<-file.path(work,"joins/urban_rural/admin_reference")
  dirs<-list(lagos=file.path(work,"rural_urban/lagos/Lagos_rural_urban_updated_files"),
    senegal=file.path(work,"rural_urban/senegal/Senegal_rural_urban_updated_files"),
    cotedivoire=file.path(work,"rural_urban/cotedivoire/CotedIvoire_rural_urban_updated_files"),
    sindh=file.path(work,"aggregate/sindh/Sindh_analysis_files"))
  joins<-lapply(names(dirs),function(k)file.path(work,if(k=="sindh")"aggregate"else"rural_urban",k,"inputs",
    paste0(k,if(k=="sindh")"_women_manitoba_fp_providers.rds"else"_women_manitoba_rural_urban_fp.rds")))
  names(joins)<-names(dirs)
  dir_args<-unlist(lapply(names(dirs),function(k)c(paste0("--",k,"-dir"),dirs[[k]])),use.names=FALSE)
  run<-function(path,a) {
    key<-basename(path)
    if(grepl("[.]R$",key))run_diagnostic(key,a) else run_python(.report_root,paste0("support/renderers/",key),a,python)
  }
  o<-list(work_dir=work,cache_dir=cache)
  population_work<-file.path(o$work_dir,"demand_population")
  for(k in names(dirs))run("analysis/check_demand_population.R",c("--data-dir",dirs[[k]],"--output-dir",file.path(population_work,k)))
  run("analysis/render_demand_population_check.py",c("--results-dir",population_work,
   "--output",file.path(report_dir,"Demand_population_classification_check.html")))
  run("analysis/score_profile_examples.R",c(dir_args[seq_len(6)],"--work-dir",file.path(o$work_dir,"score_examples"),"--output-dir",report_dir))
  profile_work<-file.path(o$work_dir,"score_contrasts")
  run("analysis/score_profile_contrasts.R",c(dir_args,"--work-dir",profile_work,"--output-dir",report_dir))
  profiles<-file.path(profile_work,"score_profile_contrasts.rds")
  run("analysis/plot_demand_pair_table.R",c("--input",profiles,"--all-pairs","--output-dir",file.path(o$work_dir,"demand_pairs"),
   "--html-output",file.path(report_dir,"Demand_pair_comparisons.html")))
  run("analysis/plot_supply_pair_table.R",c("--input",profiles,"--output-dir",file.path(o$work_dir,"supply_pairs"),
   "--html-output",file.path(checks_dir,"Composite score_comparisons.html")))
  run("analysis/check_method_awareness.R",c("--lagos-dir",dirs$lagos,"--lagos-women-file",joins$lagos,
   "--senegal-dir",dirs$senegal,"--senegal-women-file",joins$senegal,
   "--output-dir",file.path(o$work_dir,"awareness"),"--report-dir",report_dir))
  raw_fac<-function(k) {f<-if(k=="cotedivoire")"cdi"else k;file.path(o$cache_dir,"inputs/uom",f,paste0("uom_",f,"_facility_pooled.dta"))}
  coverage_args<-c("--facility-input",file.path(admin,"audit/facility_linkage_inputs.rds"))
  for(k in names(dirs))coverage_args<-c(coverage_args,paste0("--",k,"-input"),joins[[k]],paste0("--",k,"-raw-facility"),raw_fac(k))
  need_file<-file.path(o$cache_dir,"surgo_pipeline/sindh/sindh_wra_design_ready.rds")
  run("analysis/Surgo_UoM_checks.R",c(coverage_args,"--senegal-residence-lookup",file.path(o$cache_dir,"inputs/SenegalURlookup.xlsx"),
   "--sindh-need-file",need_file,"--work-dir",file.path(o$work_dir,"coverage"),"--output-dir",checks_dir))
  values<-file.path(o$work_dir,"data_checks/calculated_values.json")
  run("analysis/build_data_checks.R",c(dir_args,"--cache-dir",o$cache_dir,"--facility-input",file.path(admin,"audit/facility_linkage_inputs.rds"),"--output-json",values))
  data_checks<-file.path(report_dir,"Data_checks.html")
  run("analysis/render_data_checks.py",c("--values",values,"--output",data_checks))
  for(k in c("senegal","cotedivoire"))run(paste0(k,"_update_data_checks.R"),
   c("--base-html",data_checks,"--data-dir",dirs[[k]],"--output",data_checks))

  invisible(output)
}

if(sys.nframe()==0L) {
  a<-commandArgs(TRUE);value<-function(k,d=""){i<-match(k,a);if(is.na(i))d else a[i+1L]}
  if("--help" %in% a){cat("Build_data_checks.R --work-dir DIR --cache-dir DIR --output-dir DIR [--python EXE]\n");quit(status=0)}
  pandoc<-value("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
  build_all_checks(value("--work-dir","output/build"),value("--cache-dir","data/cache"),value("--output-dir","output/rebuilt"),value("--python","python3"))
}
