#!/usr/bin/env Rscript
# Build the three administrative-area reports independently of residence matching.
script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
root<-normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
args<-commandArgs(trailingOnly=TRUE)
if("--help"%in%args){
 cat(paste0("Usage: Rscript R/run_aggregate_reports.R [--input-zip FILE]\n",
  " [--countries all|lagos,senegal,cotedivoire] [--output-dir reports]\n",
  " [--work-dir output/aggregate_reports] [--cache-dir data/cache]\n",
  " [--join-dir VERIFIED_ADMINISTRATIVE_OUTPUT] [--python python3]\n",
  " [--pandoc-dir DIRECTORY]\n",
  "Input defaults to SURGO_MANITOBA_INPUT_ZIP. Paths resolve from the repository root.\n",
  "With no join-dir, prepare checked administrative joins directly from the input ZIP.\n"));quit(status=0)
}
opts<-list(input_zip=Sys.getenv("SURGO_MANITOBA_INPUT_ZIP",""),countries="all",output_dir="reports",
 work_dir="output/aggregate_reports",cache_dir="data/cache",join_dir="",
 python=Sys.getenv("SURGO_MANITOBA_PYTHON","python3"),pandoc_dir=Sys.getenv("RSTUDIO_PANDOC",""))
if(length(args)%%2!=0)stop("Arguments must be --name value pairs")
if(length(args))for(i in seq(1,length(args),by=2)){k<-gsub("-","_",sub("^--","",args[i]));if(!k%in%names(opts))stop("Unknown argument: ",args[i]);opts[[k]]<-args[i+1L]}
source(file.path(root,"administrative_area/analysis/report_pipeline.R"));use_project_library(root)
if(nzchar(opts$pandoc_dir))Sys.setenv(RSTUDIO_PANDOC=opts$pandoc_dir)
if(!rmarkdown::pandoc_available())stop("Supply Pandoc on the search path or with --pandoc-dir")
countries<-if(opts$countries=="all")c("lagos","senegal","cotedivoire")else strsplit(opts$countries,",",fixed=TRUE)[[1]]
if(anyDuplicated(countries)||!all(countries%in%c("lagos","senegal","cotedivoire")))stop("Invalid aggregate-country selection")
context<-prepare_aggregate_inputs(root,opts$input_zip,opts$work_dir,opts$cache_dir,opts$join_dir)
output<-project_path(opts$output_dir,root);work<-project_path(opts$work_dir,root)
dir.create(output,recursive=TRUE,showWarnings=FALSE)
report_names<-c(lagos="Lagos_analysis_aggregate",senegal="Senegal_analysis_aggregate",cotedivoire="Cote_dIvoire_analysis_aggregate")
functions<-c(lagos="build_lagos_aggregate",senegal="build_senegal_aggregate",cotedivoire="build_cotedivoire_aggregate")
records<-list()
for(country in countries){
 name<-report_names[[country]];module<-file.path(root,"administrative_area/analysis",country)
 env<-new.env(parent=globalenv());source(file.path(module,paste0(name,".R")),local=env)
 message("Building aggregate report: ",country)
 result<-env[[functions[[country]]]](root,context$join_dir,context$cache_dir,file.path(work,country),opts$python,"")
 fragment<-result$fragment
 target<-rmarkdown::render(file.path(module,paste0(name,".Rmd")),output_file=paste0(name,".html"),
  output_dir=output,params=list(prepared_fragment=fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
 run_stage(file.path(module,"verify_html.py"),c("--data-dir",result$data_dir,"--html",target),opts$python)
 audit<-jsonlite::read_json(file.path(result$data_dir,"analysis_audit.json"),simplifyVector=TRUE)
 model<-jsonlite::read_json(file.path(result$data_dir,"models_zscore/model_audit.json"),simplifyVector=TRUE)
 check<-jsonlite::read_json(file.path(result$data_dir,"checks/verification.json"),simplifyVector=TRUE)
 records[[country]]<-list(file=basename(target),sha256=digest::digest(file=target,algo="sha256"),
  source_women=audit$source_rows,women_in_need=audit$need_n,model_women=model$common_n,
  supply_resolution="Administrative area only",counseling_in_main=TRUE,
  complete_case_sensitivity_sections_in_report=FALSE,numerical_checks_passed=check$all_passed)
}
packages<-c("rmarkdown","knitr","haven","dplyr","readr","readxl","jsonlite","digest","sf","survey","clubSandwich","ggplot2","svglite")
versions<-setNames(lapply(packages,function(p)as.character(utils::packageVersion(p))),packages)
jsonlite::write_json(list(schema_version=1,workflow="administrative_area_aggregate",
 input_sha256=context$bundle_sha256,source_bundle_verified=nzchar(opts$input_zip),
 r_version=as.character(getRversion()),packages=versions,pandoc_version=as.character(rmarkdown::pandoc_version()),reports=records),
 file.path(output,"aggregate_build_manifest.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
message("Completed ",length(countries)," aggregate reports.")
