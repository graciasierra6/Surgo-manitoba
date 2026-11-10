# Senegal aggregate update: facility aggregation, individual joins and analysis.
#
# Supply sample: raw s5_6 = 1 (FP services on-site). Reaggregate all supply fields
# within each reviewed departement using valid values by measure. Retain genuine
# zero/FALSE values; exclude missing/undefined values. Facilities are unweighted.
#
# Six selected supply components:
#   one_larc_3_sa_offer -> share_offer_larc_and_3_sa
#   p_free_prod         -> mean_p_free_prod
#   p_train             -> mean_p_train
#   s5_7a-g daily / recorded offered categories -> mean_daily_modern_provision
#   one_larc_stockin    -> share_stocked_1plus_larc
#   p_any_stockout      -> mean_facility_any_stockout (original cleaned values)
# Daily uses code 1 / codes 1-4; code 5 is Not at all. Unknowns are not zeros.
# A facility with no recorded offered category has an undefined daily ratio.
#
# Assign each departement profile to every resident woman; keep all original rows
# and survey fields. There is no rural/urban supply split in this analysis.
# Sections 2 onward use original SURGO FPN2 = 1. Health beliefs use PB2A only.
#
# Before geographic scoring and PCA, replace burden with 1 - burden. Average six
# favorable component z-scores across 33 linked departements for readiness.
# Classify demand > 0 and readiness < 0; model supply risk = negative readiness.
# Models use each woman's own demand responses/scores, original survey weights,
# agreed covariates and departement-clustered CR2 inference; no random effects.
# PCA, numerical verification and portable HTML rendering are included below.
#
# Run in the existing project with caller-supplied --input-zip, or --join-dir plus
# --cache-dir. See the adjacent README for full definitions and dependencies.
build_senegal_aggregate <- function(root, join_dir, cache_dir, work_dir,
                                          python="python3", input_zip="") {
  root<-normalizePath(root,mustWork=TRUE)
  source(file.path(root,"administrative_area/analysis/report_pipeline.R"),local=TRUE)
  use_project_library(root)
  context<-prepare_aggregate_inputs(root,input_zip,work_dir,cache_dir,join_dir)
  module<-file.path(root,"administrative_area/analysis/senegal")
  work_dir<-project_path(work_dir,root)
  data<-file.path(work_dir,"Senegal_analysis_aggregate_files")
  prep<-file.path(work_dir,"inputs");dir.create(data,recursive=TRUE,showWarnings=FALSE)
  stage<-function(name,args)run_stage(file.path(module,name),args,python)
  original<-file.path(context$join_dir,"checked_geography/senegal_women_manitoba.rds")
  fac<-file.path(context$join_dir,"audit/facility_linkage_inputs.rds")
  raw<-file.path(context$cache_dir,"inputs/surgo/senegal/surgo_senegal_women.dta")
  raw_fac<-file.path(context$cache_dir,"inputs/uom/senegal/uom_senegal_facility_pooled.dta")
  stage("prepare_fp_provider_data.R",c("--input",original,"--facility-input",fac,
    "--raw-facility",raw_fac,"--raw-women",raw,"--dictionary",
    file.path(context$cache_dir,"exports/merged_supply_demand_dictionary.csv"),"--output-dir",prep))
  inputs<-c("--input",file.path(prep,"senegal_women_manitoba_fp_providers.rds"),
    "--facility-input",fac,"--raw",raw,"--raw-facility",raw_fac,"--department-lookup",
    file.path(context$cache_dir,"lookups/senegal_health_district_department_evidence.csv"))
  stage("build_analysis.R",c(inputs,"--output-dir",data))
  stage("prepare_map.R",c("--boundary",file.path(root,"analysis/assets/boundaries/senegal_departments.geojson"),"--data-dir",data))
  stage("fit_models.R",c("--data-dir",data,"--output-dir",file.path(data,"models_zscore")))
  pca<-file.path(data,"pca_appendix")
  stage("build_pca_scores.R",c("--data-dir",data,"--output-dir",pca))
  stage("fit_models.R",c("--data-dir",data,"--output-dir",file.path(pca,"models"),
    "--score-method","pca","--pca-score-file",file.path(pca,"pca_scores.rds")))
  stage("verify_analysis.R",c(inputs[1:8],"--cleaned-facility",
    file.path(context$cache_dir,"uom_pipeline/uom_summary_facility.csv"),"--data-dir",data))
  native<-file.path(work_dir,"Senegal_analysis_aggregate.html")
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
    cat("Senegal_analysis_aggregate.R\n",
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
  r<-build_senegal_aggregate(root,value("--join-dir"),value("--cache-dir","data/cache"),
    value("--work-dir","output/Senegal_analysis_aggregate"),python,value("--input-zip",Sys.getenv("SURGO_MANITOBA_INPUT_ZIP","")))
  output<-value("--output-dir","reports");if(!grepl("^/|^[A-Za-z]:",output))output<-file.path(root,output)
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  rmarkdown::render(file.path(root,"administrative_area/analysis/senegal/Senegal_analysis_aggregate.Rmd"),
    output_file="Senegal_analysis_aggregate.html",output_dir=normalizePath(output),
    params=list(prepared_fragment=r$fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
  source(file.path(root,"administrative_area/analysis/report_pipeline.R"))
  run_stage(file.path(root,"administrative_area/analysis/senegal/verify_html.py"),
    c("--data-dir",r$data_dir,"--html",file.path(output,"Senegal_analysis_aggregate.html")),python)
  cat("Saved Senegal_analysis_aggregate.html\n")
}
