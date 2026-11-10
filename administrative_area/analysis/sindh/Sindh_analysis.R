# Sindh updated district analysis: Q506 Yes, original cleaned supply indicators,
# PB2A-only health beliefs, authorized fpn2_derived and final Normalizedweights.
# Only mean component z-scores; geographic cutoffs are zero. Personal demand and
# assigned district supply enter five adjusted modern-use models with district CR2.
# No Rural/Urban split, random effects, alternative scoring or PCA in this revision.
# All external inputs are supplied at runtime. See the adjacent README.
build_sindh_aggregate <- function(root, join_dir, cache_dir, work_dir,
                                          python="python3", input_zip="") {
  root<-normalizePath(root,mustWork=TRUE)
  source(file.path(root,"administrative_area/analysis/report_pipeline.R"),local=TRUE)
  use_project_library(root)
  context<-prepare_aggregate_inputs(root,input_zip,work_dir,cache_dir,join_dir)
  module<-file.path(root,"administrative_area/analysis/sindh")
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
  stage("prepare_map.R",c("--boundary",file.path(root,"analysis/assets/boundaries/sindh_districts.geojson"),"--data-dir",data))
  stage("verify_analysis.R",c(inputs,"--raw-women",raw,"--district-aliases",file.path(context$cache_dir,"lookups/sindh_district_aliases.csv"),"--cleaned-facility",
    file.path(context$cache_dir,"uom_pipeline/uom_summary_facility.csv"),"--data-dir",data))
  stage("save_updated_dataset.R",c("--input",file.path(prep,"sindh_women_manitoba_fp_providers.rds"),"--data-dir",data,"--output-dir",prep))
  native<-file.path(work_dir,"Sindh_analysis.html")
  stage("render_analysis.py",c("--data-dir",data,"--output",native))
  stage("verify_html.py",c("--data-dir",data,"--html",native))
  fragment<-file.path(work_dir,"report_fragment.html")
  run_stage(file.path(root,"analysis/common/html_fragment.py"),c("--input",native,"--output",fragment),python)
  list(native=native,fragment=fragment,data_dir=data)
}

if(sys.nframe()==0L){
  args<-commandArgs(trailingOnly=TRUE)
  value<-function(k,default=""){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
  script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
  if("--help"%in%args){
    cat("Sindh_analysis.R\n",
      "Inputs: --input-zip <bundle> OR --join-dir <verified join> --cache-dir <input cache>\n",
      "Options: --project-root <existing project>, --work-dir <working output>,\n",
      "         --output-dir <reports>, --python <executable>, --pandoc-dir <directory>\n",
      "Environment alternatives: SURGO_MANITOBA_INPUT_ZIP, SURGO_MANITOBA_PYTHON, RSTUDIO_PANDOC.\n",
      "Keep this script with its documented project modules; it is not standalone.\n",sep="")
    quit(status=0)
  }
  root<-normalizePath(value("--project-root",file.path(dirname(script),"..","..","..")),mustWork=TRUE)
  pandoc<-value("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
  python<-value("--python",Sys.getenv("SURGO_MANITOBA_PYTHON","python3"))
  r<-build_sindh_aggregate(root,value("--join-dir"),value("--cache-dir","data/cache"),
    value("--work-dir","output/aggregate_reports/sindh"),python,value("--input-zip",Sys.getenv("SURGO_MANITOBA_INPUT_ZIP","")))
  output<-value("--output-dir","reports");if(!grepl("^/|^[A-Za-z]:",output))output<-file.path(root,output)
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  rmarkdown::render(file.path(root,"administrative_area/analysis/sindh/Sindh_analysis.Rmd"),
    output_file="Sindh_analysis.html",output_dir=normalizePath(output),
    params=list(prepared_fragment=r$fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
  source(file.path(root,"administrative_area/analysis/report_pipeline.R"))
  run_stage(file.path(root,"administrative_area/analysis/sindh/verify_html.py"),
    c("--data-dir",r$data_dir,"--html",file.path(output,"Sindh_analysis.html")),python)
  cat("Saved Sindh_analysis.html\n")
}
