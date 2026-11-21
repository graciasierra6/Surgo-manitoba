#!/usr/bin/env Rscript
# Rebuild the current reports, coverage tables and supplementary results from one bundle.
args<-commandArgs(trailingOnly=TRUE)
script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
root<-normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
if("--help"%in%args) {
 cat(paste0("Usage: Rscript R/rebuild_all.R --input-zip FILE\n",
  " [--output-dir output/reproduction/rebuilt] [--work-dir output/reproduction/work]\n",
  " [--cache-dir data/reproduction_cache] [--python python3] [--pandoc-dir DIRECTORY]\n",
  " [--include-historical true|false] [--verify true|false]\n",
  " [--resume-after none|main]\n",
  "Input also accepts SURGO_MANITOBA_INPUT_ZIP. Defaults preserve published reports.\n",
  "Historical reports use a checksummed public source revision to retain their earlier definitions.\n"));quit(status=0)
}
o<-list(input_zip=Sys.getenv("SURGO_MANITOBA_INPUT_ZIP",""),output_dir="output/reproduction/rebuilt",
 work_dir="output/reproduction/work",cache_dir="data/reproduction_cache",python="python3",
 pandoc_dir=Sys.getenv("RSTUDIO_PANDOC",""),include_historical="true",verify="true",resume_after="none")
if(length(args)%%2)stop("Arguments must be --name value pairs")
if(length(args))for(i in seq(1,length(args),2)) {
 key<-gsub("-","_",sub("^--","",args[i]));if(!key%in%names(o))stop("Unknown argument: ",args[i]);o[[key]]<-args[i+1L]
}
if(!all(c(o$include_historical,o$verify)%in%c("true","false")))stop("Boolean options must be true or false")
if(!o$resume_after%in%c("none","main"))stop("Resume selection must be none or main")
source(file.path(root,"analysis/common/pipeline.R"));use_project_library(root)
required<-c("haven","dplyr","readr","readxl","jsonlite","digest","sf","survey",
 "clubSandwich","ggplot2","svglite","rmarkdown","knitr","base64enc")
missing<-required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop("Restore project dependencies first; missing: ",paste(missing,collapse=", "))
if(o$verify=="true") {
 lock<-jsonlite::read_json(file.path(root,"renv.lock"),simplifyVector=FALSE)
 different<-names(lock$Packages)[!vapply(names(lock$Packages),function(k)
  tryCatch(utils::packageVersion(k)==package_version(lock$Packages[[k]]$Version),error=function(e)FALSE),logical(1))]
 if(length(different))stop("Restore the recorded package versions before a reference rebuild: ",paste(different,collapse=", "))
 if(getRversion()!=package_version(lock$R$Version))stop("The reference rebuild requires R ",lock$R$Version)
}
if(nzchar(o$pandoc_dir))Sys.setenv(RSTUDIO_PANDOC=o$pandoc_dir)
if(!rmarkdown::pandoc_available())stop("Pandoc is required; supply --pandoc-dir or RSTUDIO_PANDOC")
if(!nzchar(o$input_zip))stop("Supply --input-zip or SURGO_MANITOBA_INPUT_ZIP")
for(k in c("input_zip","output_dir","work_dir","cache_dir"))o[[k]]<-project_path(o[[k]],root)
if(!file.exists(o$input_zip)||dir.exists(o$input_zip))stop("Supply an existing input ZIP file")
members<-utils::unzip(o$input_zip,list=TRUE)$Name
if(!"lookups/senegal_health_district_department_evidence.csv"%in%members)
 stop("The input bundle lacks the required Senegal geographic evidence lookup")
published<-normalizePath(file.path(root,"reports"),mustWork=TRUE)
if(o$output_dir%in%c(root,published))stop("Select a separate output directory; verification must preserve the published reference reports")
dir.create(o$output_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(o$work_dir,recursive=TRUE,showWarnings=FALSE)
report_dir<-file.path(o$output_dir,"reports");checks_dir<-file.path(o$output_dir,"data_checks")
dir.create(report_dir,recursive=TRUE,showWarnings=FALSE);dir.create(checks_dir,recursive=TRUE,showWarnings=FALSE)
run<-function(path,a)run_stage(file.path(root,path),a,o$python)
existing_join<-if(o$resume_after=="main")file.path(o$work_dir,"linkage")else""
ctx<-prepare_linkage(root,o$input_zip,o$work_dir,o$cache_dir,existing_join)
admin<-file.path(ctx$join_dir,"admin_reference")
aggregate_work<-file.path(o$work_dir,"aggregate")
if(o$resume_after=="none")run("R/run_aggregate_reports.R",c("--input-zip",o$input_zip,"--join-dir",admin,
 "--cache-dir",o$cache_dir,"--work-dir",aggregate_work,"--output-dir",report_dir,"--python",o$python))
spec<-list(lagos=c("Lagos_rural_urban_updated","lagos"),senegal=c("Senegal_rural_urban_updated","senegal"),
 cotedivoire=c("CotedIvoire_rural_urban_updated","cotedivoire"))
dirs<-list();joins<-list()
for(k in names(spec)) {
 name<-spec[[k]][1];w<-file.path(o$work_dir,"residence",k)
 if(o$resume_after=="none")run(paste0("urban_rural/analysis/",k,"/",name,".R"),c("--input-zip",o$input_zip,
  "--join-dir",ctx$join_dir,"--cache-dir",o$cache_dir,"--work-dir",w,"--output-dir",report_dir,"--python",o$python))
 dirs[[k]]<-file.path(w,paste0(name,"_files"))
 joins[[k]]<-file.path(w,"inputs",paste0(k,"_women_manitoba_rural_urban_fp.rds"))
}
dirs$sindh<-file.path(aggregate_work,"sindh/Sindh_analysis_files")
joins$sindh<-file.path(aggregate_work,"sindh/inputs/sindh_women_manitoba_fp_providers.rds")
if(o$resume_after=="main") {
 main_names<-c("Lagos_analysis_aggregate","Senegal_analysis_aggregate","Cote_dIvoire_analysis_aggregate","Sindh_analysis",
  "Lagos_rural_urban_updated","Senegal_rural_urban_updated","CotedIvoire_rural_urban_updated")
 for(name in main_names) {
  a<-file.path(root,"reports",paste0(name,".html"));b<-file.path(report_dir,paste0(name,".html"))
  if(!file.exists(b))stop("Resume requires seven completed reports; rerun the main stage: ",name)
 }
 if(o$verify=="true")run("tests/verify_rebuilt_reports.py",c("--reference-root",root,
  "--rebuilt-root",o$output_dir,"--include-historical","false","--selection","main",
  "--output",file.path(o$output_dir,"main_report_validation.json")))
 main_dirs<-c(file.path(aggregate_work,"lagos/Lagos_analysis_aggregate_files"),
  file.path(aggregate_work,"senegal/Senegal_analysis_aggregate_files"),
  file.path(aggregate_work,"cotedivoire/Cote_dIvoire_analysis_aggregate_files"),dirs$sindh,unlist(dirs[c("lagos","senegal","cotedivoire")]))
 for(path in main_dirs) {
  check<-file.path(path,"checks/verification.json")
  if(!file.exists(check)||!isTRUE(jsonlite::read_json(check)$all_passed))stop("Resume lacks successful numerical verification: ",basename(path))
 }
 message("Resuming after seven completed reports and successful numerical checks; input bundle reverified.")
}
dir_args<-unlist(lapply(names(dirs),function(k)c(paste0("--",k,"-dir"),dirs[[k]])),use.names=FALSE)
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
for(k in c("senegal","cotedivoire"))run(paste0("urban_rural/analysis/",k,"/update_data_checks.R"),
 c("--base-html",data_checks,"--data-dir",dirs[[k]],"--output",data_checks))
if(o$include_historical=="true")run("tests/rebuild_historical_reports.py",c("--input-zip",o$input_zip,
 "--join-dir",ctx$join_dir,"--cache-dir",o$cache_dir,"--work-dir",file.path(o$work_dir,"historical"),
 "--output-dir",report_dir,"--python",o$python))
if(o$verify=="true")run("tests/verify_rebuilt_reports.py",c("--reference-root",root,"--rebuilt-root",o$output_dir,
 "--include-historical",o$include_historical,"--output",file.path(o$output_dir,"reproduction_validation.json")))
cat("Completed the cumulative report rebuild. Input datasets and fitted objects remain outside shared reports.\n")
