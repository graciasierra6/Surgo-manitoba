# Reproduce the updated Côte d’Ivoire report with FP-provider département-residence supply.
# External inputs are supplied at runtime; working datasets stay in ignored output.
build_cotedivoire_rural_urban <- function(root,join_dir="",cache_dir="data/cache",
 work_dir="output/rural_urban_reports/cotedivoire",python="python3",input_zip="") {
 root<-normalizePath(root,mustWork=TRUE)
 source(file.path(root,"analysis/common/pipeline.R"),local=TRUE);use_project_library(root)
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
   "checked_geography/cotedivoire_women_manitoba_urban_rural.rds"))
   if(!file.exists(file.path(join_dir,n)))stop("Incomplete verified urban/rural join")
  opts<-jsonlite::read_json(file.path(join_dir,"run_parameters.json"),simplifyVector=TRUE)
  if(opts$geography_version!="both")stop("Use the verified join containing both geography versions")
 }
 verify_report_assets(root)
 module<-file.path(root,"urban_rural/analysis/cotedivoire")
 shared<-file.path(root,"administrative_area/analysis/cotedivoire")
 data<-file.path(work_dir,"CotedIvoire_rural_urban_updated_files")
 prep<-file.path(work_dir,"inputs");dir.create(data,recursive=TRUE,showWarnings=FALSE)
 run<-function(base,name,args)run_stage(file.path(base,name),args,python)
 admin<-file.path(join_dir,"admin_reference")
 fac<-file.path(admin,"audit/facility_linkage_inputs.rds")
 raw<-file.path(cache_dir,"inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta")
 raw_fac<-file.path(cache_dir,"inputs/uom/cdi/uom_cdi_facility_pooled.dta")
 dict<-file.path(cache_dir,"exports/merged_supply_demand_dictionary.csv")
 run(shared,"prepare_fp_provider_data.R",c("--input",file.path(admin,"checked_geography/cotedivoire_women_manitoba.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--raw-women",raw,"--dictionary",dict,"--output-dir",prep))
 run(shared,"build_analysis.R",c("--input",file.path(prep,"cotedivoire_women_manitoba_fp_providers.rds"),
  "--facility-input",fac,"--raw",raw,"--raw-facility",raw_fac,"--district-codebook",
  file.path(cache_dir,"lookups/cdi_health_district_codebook.csv"),"--output-dir",data))
 run(shared,"prepare_map.R",c("--boundary",file.path(root,"analysis/assets/boundaries/civ_departments_2018.geojson"),"--subprefecture-boundary",file.path(root,"analysis/assets/boundaries/civ_subprefectures_2018.geojson"),"--data-dir",data))
 run(module,"assign_residence_supply.R",c("--input",file.path(join_dir,"checked_geography/cotedivoire_women_manitoba_urban_rural.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--dictionary",dict,"--data-dir",data,"--output-dir",prep))
 run(module,"fit_models.R",c("--data-dir",data,"--output-dir",file.path(data,"models_zscore")))
 pca<-file.path(data,"pca_appendix")
 run(module,"build_pca_scores.R",c("--data-dir",data,"--output-dir",pca))
 run(module,"fit_models.R",c("--data-dir",data,"--output-dir",file.path(pca,"models"),
  "--score-method","pca","--pca-score-file",file.path(pca,"pca_scores.rds")))
 run(module,"verify_analysis.R",c("--input",file.path(prep,"cotedivoire_women_manitoba_rural_urban_fp.rds"),
  "--facility-input",fac,"--raw-facility",raw_fac,"--raw",raw,
  "--cleaned-facility",file.path(cache_dir,"uom_pipeline/uom_summary_facility.csv"),"--data-dir",data))
 native<-file.path(work_dir,"CotedIvoire_rural_urban_updated.html")
 run(module,"render_analysis.py",c("--data-dir",data,"--output",native))
 run(module,"verify_html.py",c("--data-dir",data,"--html",native))
 fragment<-file.path(work_dir,"report_fragment.html")
 run_stage(file.path(root,"analysis/common/html_fragment.py"),c("--input",native,"--output",fragment),python)
 list(native=native,fragment=fragment,data_dir=data)
}

if(sys.nframe()==0L){
 args<-commandArgs(trailingOnly=TRUE)
 value<-function(k,default=""){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
 if("--help"%in%args){cat("Rscript urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.R\n",
  " --input-zip FILE OR --join-dir VERIFIED_URBAN_RURAL_JOIN --cache-dir CACHE\n",
  " [--output-dir reports] [--work-dir output/rural_urban_reports/cotedivoire]\n",
  " [--python python3] [--pandoc-dir DIRECTORY]\n",sep="");quit(status=0)}
 script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
 root<-normalizePath(file.path(dirname(script),"..","..",".."),mustWork=TRUE)
 pandoc<-value("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
 r<-build_cotedivoire_rural_urban(root,value("--join-dir"),value("--cache-dir","data/cache"),
  value("--work-dir","output/rural_urban_reports/cotedivoire"),value("--python",Sys.getenv("SURGO_MANITOBA_PYTHON","python3")),
  value("--input-zip",Sys.getenv("SURGO_MANITOBA_INPUT_ZIP","")))
 output<-value("--output-dir","reports");if(!grepl("^/|^[A-Za-z]:",output))output<-file.path(root,output)
 dir.create(output,recursive=TRUE,showWarnings=FALSE)
 rmarkdown::render(file.path(root,"urban_rural/analysis/cotedivoire/CotedIvoire_rural_urban_updated.Rmd"),
  output_file="CotedIvoire_rural_urban_updated.html",output_dir=normalizePath(output),
  params=list(prepared_fragment=r$fragment),quiet=TRUE,envir=new.env(parent=globalenv()))
 source(file.path(root,"analysis/common/pipeline.R"))
 run_stage(file.path(root,"urban_rural/analysis/cotedivoire/verify_html.py"),
  c("--data-dir",r$data_dir,"--html",file.path(output,"CotedIvoire_rural_urban_updated.html")),
  value("--python",Sys.getenv("SURGO_MANITOBA_PYTHON","python3")))
 cat("Saved CotedIvoire_rural_urban_updated.html\n")
}
