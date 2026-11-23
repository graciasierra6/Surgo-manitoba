#!/usr/bin/env Rscript
# Complete project rebuild: seven country reports and eight supplementary checks
#
# Required R packages (restore once: Rscript setup.R):
#   haven, dplyr, readr, readxl, jsonlite, digest, sf, survey, clubSandwich, ggplot2, svglite,
#   tidyselect, rmarkdown, knitr, base64enc.
# Exact package versions: renv.lock. Project dependencies: DESCRIPTION.
# setup.R uses renv to restore the recorded project package environment.
# stats, utils, tools, grid, graphics and grDevices are bundled with R where used.
#
# Primary analytical input members (one complete authorized ZIP):
#   inputs/surgo/lagos/surgo_lagos_women.csv
#   inputs/surgo/senegal/surgo_senegal_women.dta
#   inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta
#   inputs/surgo/sindh/20250124_forms_wt.dta
#   inputs/uom/lagos/uom_lagos_facility_pooled.dta
#   inputs/uom/senegal/uom_senegal_facility_pooled.dta
#   inputs/uom/cdi/uom_cdi_facility_pooled.dta
#   inputs/uom/sindh/uom_sindh_facility_pooled.dta
#   uom_pipeline/uom_summary_facility.csv
#   uom_pipeline/Facilities data with archetype assignments.csv
#   uom_pipeline/new_demand_vol_facility.csv
#   exports/uom_surgo_geo_crosswalk_reviewed.csv
#   inputs/SenegalURlookup.xlsx
#   surgo_pipeline/sindh/sindh_wra_design_ready.rds
#
# Supporting files:
#   exports/merged_supply_demand_dictionary.csv
#   lookups/senegal_health_district_department_evidence.csv
#   lookups/cdi_health_district_codebook.csv
#   lookups/sindh_district_aliases.csv
#   lookups/geographic_sources.json
#   bundle_manifest.json
#
# All 56 ZIP members are required and verified, including upstream reconciliation/reference files
#   listed in docs/INPUTS.md.
# Run: Rscript run_all.R --input-zip FILE. Also requires Python 3 and Pandoc.
# This command prepares both joins and all current reports; no pre-existing joins or fitted models
#   are required. Outputs default to ignored output/rebuilt/ and output/build/.
# Full filename inventory: docs/DATASET_FILENAMES.md; roles/checksums: docs/INPUTS.md.
# Input locations and credentials are runtime parameters, never saved personal paths.

a<-commandArgs(TRUE)
value<-function(k,d=""){i<-match(k,a);if(is.na(i))return(d);if(i==length(a))stop("Supply ",k);a[i+1L]}
entry<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
root<-dirname(normalizePath(entry,mustWork=TRUE))
source(file.path(root,"support/runtime.R"));use_project_library(root)
source(file.path(root,"support/linkage.R"))
if("--help" %in% a){cat("run_all.R --input-zip FILE [--work-dir DIR] [--cache-dir DIR] [--output-dir DIR] [--verify true|false] [--python EXE] [--pandoc-dir DIR]\n");quit(status=0)}
input<-value("--input-zip",Sys.getenv("SURGO_MANITOBA_INPUT_ZIP",""));if(!nzchar(input))stop("Supply input ZIP")
input<-project_path(input,root);work<-project_path(value("--work-dir","output/build"),root)
cache<-project_path(value("--cache-dir","data/cache"),root);out<-project_path(value("--output-dir","output/rebuilt"),root)
python<-value("--python",Sys.getenv("SURGO_MANITOBA_PYTHON",""))
python<-select_python(python)$command
pandoc<-value("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
verify<-value("--verify","true");stopifnot(verify %in% c("true","false"))
if(verify=="true"){
 lock<-jsonlite::read_json(file.path(root,"renv.lock"));stopifnot(getRversion()==package_version(lock$R$Version))
 for(k in names(lock$Packages))stopifnot(utils::packageVersion(k)==package_version(lock$Packages[[k]]$Version))
}
dir.create(work,recursive=TRUE,showWarnings=FALSE);dir.create(out,recursive=TRUE,showWarnings=FALSE)
join<-file.path(work,"joins/urban_rural")
build_residence_join(root,input,join,cache)
run_r<-function(file,args){status<-system2(file.path(R.home("bin"),if(.Platform$OS.type=="windows")"Rscript.exe"else"Rscript"),vapply(c(file.path(root,file),args),shQuote,character(1)));if(status!=0)stop("Failed: ",file)}
specs<-list(
  list(mode="aggregate",country="lagos",entry="aggregate/lagos/Lagos_analysis_aggregate.R"),
  list(mode="aggregate",country="senegal",entry="aggregate/senegal/Senegal_analysis_aggregate.R"),
  list(mode="aggregate",country="cotedivoire",entry="aggregate/cotedivoire/Cote_dIvoire_analysis_aggregate.R"),
  list(mode="aggregate",country="sindh",entry="aggregate/sindh/Sindh_analysis.R"),
  list(mode="rural_urban",country="lagos",entry="rural_urban/lagos/Lagos_rural_urban_updated.R"),
  list(mode="rural_urban",country="senegal",entry="rural_urban/senegal/Senegal_rural_urban_updated.R"),
  list(mode="rural_urban",country="cotedivoire",entry="rural_urban/cotedivoire/CotedIvoire_rural_urban_updated.R"))
for(s in specs) {
  run_r(s$entry,c("--input-zip",input,"--join-dir",if(s$mode=="aggregate")file.path(join,"admin_reference")else join,
    "--cache-dir",cache,"--work-dir",file.path(work,s$mode,s$country),"--output-dir",file.path(out,s$mode,s$country),"--python",python))
}
run_r("data_checks/Build_data_checks.R",c("--work-dir",work,"--cache-dir",cache,"--output-dir",out,"--python",python))
if(verify=="true")run_python(root,"validation/verify_reports.py",c("--reference-root",root,"--rebuilt-root",out,"--output",file.path(out,"validation.json")),python)
message("Completed seven country reports and eight supplementary checks in ",out)
