# Lagos updated aggregate analysis: original FPN2, Q506 Yes facility summaries.
# Health beliefs use PB2A only. Six supply components include daily provision and
# original cleaned p_any_stockout; stockouts become continuity before scoring.
# Geography: LGA-wide, without Rural/Urban supply splitting; residence is a covariate.
# Primary geographic mean z-score cutoffs are zero; individual models retain personal
# demand responses and assigned LGA supply. Original weights; LGA CR2 inference.
# All external inputs are caller supplied. See the adjacent README for definitions.
build_lagos_aggregate <- function(root, join_dir, cache_dir, work_dir,
                                          python="python3", input_zip="") {
  root<-normalizePath(root,mustWork=TRUE)
  source(file.path(root,"administrative_area/analysis/report_pipeline.R"),local=TRUE)
  use_project_library(root)
  context<-prepare_aggregate_inputs(root,input_zip,work_dir,cache_dir,join_dir)
  module<-file.path(root,"administrative_area/analysis/lagos")
  work_dir<-project_path(work_dir,root)
  data<-file.path(work_dir,"Lagos_analysis_aggregate_files")
  prep<-file.path(work_dir,"inputs");dir.create(data,recursive=TRUE,showWarnings=FALSE)
  stage<-function(name,args)run_stage(file.path(module,name),args,python)
  original<-file.path(context$join_dir,"checked_geography/lagos_women_manitoba.rds")
  fac<-file.path(context$join_dir,"audit/facility_linkage_inputs.rds")
  raw<-file.path(context$cache_dir,"inputs/surgo/lagos/surgo_lagos_women.csv")
  raw_fac<-file.path(context$cache_dir,"inputs/uom/lagos/uom_lagos_facility_pooled.dta")
  stage("prepare_fp_provider_data.R",c("--input",original,"--facility-input",fac,
    "--raw-facility",raw_fac,"--raw-women",raw,"--dictionary",
    file.path(context$cache_dir,"exports/merged_supply_demand_dictionary.csv"),"--output-dir",prep))
  inputs<-c("--input",file.path(prep,"lagos_women_manitoba_fp_providers.rds"),
    "--facility-input",fac,"--raw-facility",raw_fac)
  stage("build_analysis.R",c(inputs,"--output-dir",data))
  stage("fit_models.R",c("--data-dir",data,"--output-dir",file.path(data,"models_zscore")))
  pca<-file.path(data,"pca_appendix")
  stage("build_pca_scores.R",c("--data-dir",data,"--output-dir",pca))
  stage("fit_models.R",c("--data-dir",data,"--output-dir",file.path(pca,"models"),
    "--score-method","pca","--pca-score-file",file.path(pca,"pca_scores.rds")))
  stage("verify_analysis.R",c(inputs,"--raw-women",raw,"--cleaned-facility",
    file.path(context$cache_dir,"uom_pipeline/uom_summary_facility.csv"),"--data-dir",data))
  native<-file.path(work_dir,"Lagos_analysis_aggregate.html")
  stage("render_analysis.py",c("--data-dir",data,"--reference-html",file.path(root,"analysis/assets/lagos_map_template.html"),"--output",native))
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
    cat("Lagos_analysis_aggregate.R\n",
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
  r<-build_lagos_aggregate(root,value("--join-dir"),value("--cache-dir","data/cache"),
    value("--work-dir","output/aggregate_reports/lagos"),python,value("--input-zip",Sys.getenv("SURGO_MANITOBA_INPUT_ZIP","")))
  output<-value("--output-dir","reports");if(!grepl("^/|^[A-Za-z]:",output))output<-file.path(root,output)
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  rmarkdown::render(file.path(root,"administrative_area/analysis/lagos/Lagos_analysis_aggregate.Rmd"),
    output_file="Lagos_analysis_aggregate.html",output_dir=normalizePath(output),
    params=list(prepared_fragment=r$fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
  source(file.path(root,"administrative_area/analysis/report_pipeline.R"))
  run_stage(file.path(root,"administrative_area/analysis/lagos/verify_html.py"),
    c("--data-dir",r$data_dir,"--html",file.path(output,"Lagos_analysis_aggregate.html")),python)
  cat("Saved Lagos_analysis_aggregate.html\n")
}
