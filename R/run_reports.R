#!/usr/bin/env Rscript
# Compatibility entry point for the preserved three-report workflow.
# Historical definitions come from their checksummed public source revision.
script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
root<-normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
args<-commandArgs(trailingOnly=TRUE)
if("--help"%in%args){
 cat("Rscript R/run_reports.R --input-zip FILE [--countries all|lagos,senegal,cotedivoire,sindh]\n",
  " [--output-dir reports] [--work-dir output/historical_reports] [--cache-dir data/cache]\n",
  " [--join-dir VERIFIED_RESIDENCE_JOIN] [--python python3] [--pandoc-dir DIRECTORY]\n",
  "Rebuild earlier country definitions; Sindh uses its current district module.\n",sep="");quit(status=0)
}
o<-list(input_zip=Sys.getenv("SURGO_MANITOBA_INPUT_ZIP",""),countries="all",output_dir="reports",
 work_dir="output/historical_reports",cache_dir="data/cache",join_dir="",python="python3",pandoc_dir=Sys.getenv("RSTUDIO_PANDOC",""))
if(length(args)%%2)stop("Use --name value pairs")
if(length(args))for(i in seq(1,length(args),2)) {k<-gsub("-","_",sub("^--","",args[i]));if(!k%in%names(o))stop("Unknown argument");o[[k]]<-args[i+1L]}
source(file.path(root,"analysis/common/pipeline.R"));use_project_library(root)
if(nzchar(o$pandoc_dir))Sys.setenv(RSTUDIO_PANDOC=o$pandoc_dir)
if(!nzchar(o$input_zip))stop("Supply --input-zip or SURGO_MANITOBA_INPUT_ZIP")
for(k in c("input_zip","output_dir","work_dir","cache_dir"))o[[k]]<-project_path(o[[k]],root)
if(nzchar(o$join_dir))o$join_dir<-project_path(o$join_dir,root)
countries<-if(o$countries=="all")c("lagos","senegal","cotedivoire","sindh")else strsplit(o$countries,",",fixed=TRUE)[[1]]
if(anyDuplicated(countries)||!all(countries%in%c("lagos","senegal","cotedivoire","sindh")))stop("Invalid country selection")
ctx<-prepare_linkage(root,o$input_zip,o$work_dir,o$cache_dir,o$join_dir)
old<-intersect(countries,c("lagos","senegal","cotedivoire"))
if(length(old))run_stage(file.path(root,"tests/rebuild_historical_reports.py"),c("--input-zip",o$input_zip,
 "--join-dir",ctx$join_dir,"--cache-dir",o$cache_dir,"--work-dir",file.path(o$work_dir,"historical_source"),
 "--output-dir",o$output_dir,"--python",o$python,"--countries",paste(old,collapse=",")),o$python)
if("sindh"%in%countries)run_stage(file.path(root,"R/run_aggregate_reports.R"),c("--input-zip",o$input_zip,
 "--join-dir",file.path(ctx$join_dir,"admin_reference"),"--cache-dir",o$cache_dir,
 "--work-dir",file.path(o$work_dir,"district"),"--output-dir",o$output_dir,"--countries","sindh","--python",o$python))
