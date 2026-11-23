# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: haven, jsonlite, readxl, rmarkdown, tidyselect, tools.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Coverage diagnostic entry.
# Inputs: Four FP-provider woman joins, four raw facility files, facility audit, D13 and D14.
# Outputs: Coverage tables/working checks and Surgo_UoM_checks.html through the matching Rmd.
# Source context via country pipeline: D01,D02,D03,D04,D05,D06,D07,D08,D09,D10,D11,D12,D13,D14;
#   exact dataset names in docs/COLLEAGUE_RUN_GUIDE.md.
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Shared survey-weighted estimation and geographic regression inference.
.survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
.survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
while (!file.exists(file.path(.survey_root, "analysis/common/survey_methods.R"))) {
  .survey_parent <- dirname(.survey_root)
  if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
  .survey_root <- .survey_parent
}
source(file.path(.survey_root, "analysis/common/survey_methods.R"), local = TRUE)

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
  source(file.path(root,"analysis/common/pipeline.R"),local=TRUE)
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
  source(file.path(root,"analysis/common/pipeline.R"),local=TRUE)
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
  source(file.path(root,"analysis/common/pipeline.R"));use_project_library(root)
  inputs<-setNames(lapply(c("cotedivoire","lagos","senegal","sindh"),function(k)list(
    input=arg(paste0("--",k,"-input"),Sys.getenv(paste0("SURGO_MANITOBA_",toupper(k),"_WOMEN_JOIN"),"")),
    raw_facility=arg(paste0("--",k,"-raw-facility"),Sys.getenv(paste0("SURGO_MANITOBA_",toupper(k),"_RAW_FACILITY"),"")),
    residence_lookup=if(k=="senegal")arg("--senegal-residence-lookup",Sys.getenv("SURGO_MANITOBA_SENEGAL_RESIDENCE_LOOKUP",""))else"",
    need_file=if(k=="sindh")arg("--sindh-need-file",Sys.getenv("SURGO_MANITOBA_SINDH_NEED_FILE",""))else"")),c("cotedivoire","lagos","senegal","sindh"))
  work<-prepare_surgo_uom_checks(root,inputs,arg("--facility-input",arg("--lagos-facility-input",Sys.getenv("SURGO_MANITOBA_FACILITY_LINKAGE_INPUT",""))),arg("--work-dir","output/surgo_uom_checks"))
  pandoc<-arg("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
  output<-project_path(arg("--output-dir","data_checks"),root);dir.create(output,recursive=TRUE,showWarnings=FALSE)
  rmarkdown::render(file.path(root,"analysis/Surgo_UoM_checks.Rmd"),output_file="Surgo_UoM_checks.html",output_dir=normalizePath(output),params=list(prepared_dir=work),quiet=TRUE,envir=new.env(parent=globalenv()))
  cat("Saved Surgo_UoM_checks.html\n")
}
