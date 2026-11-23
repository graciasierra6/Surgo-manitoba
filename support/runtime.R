# Shared runtime: parameterized paths, stage isolation, renderer dispatch and join preparation.
project_path <- function(path,root) {
  path<-path.expand(path)
  if(!grepl("^/|^[A-Za-z]:|^\\\\",path))path<-file.path(root,path)
  normalizePath(path,mustWork=FALSE)
}
find_project_root <- function(path) {
  p<-normalizePath(path,mustWork=TRUE)
  if(!dir.exists(p))p<-dirname(p)
  while(!file.exists(file.path(p,"support/runtime.R"))) {
    q<-dirname(p);if(identical(p,q))stop("Keep the script inside the complete project");p<-q
  }
  p
}
use_project_library <- function(root) {
  p<-file.path(root,"renv/library",paste0("R-",getRversion()),R.version$platform)
  if(dir.exists(p)){.libPaths(c(p,.libPaths()));Sys.setenv(R_LIBS_USER=paste(.libPaths(),collapse=.Platform$path.sep))}
  invisible(TRUE)
}
run_python <- function(root,path,args=character(),python="python3") {
  previous<-Sys.getenv(c("PYTHONUTF8","PYTHONIOENCODING"),unset=NA_character_)
  on.exit(for(k in names(previous))if(is.na(previous[[k]]))Sys.unsetenv(k)else do.call(Sys.setenv,setNames(list(previous[[k]]),k)),add=TRUE)
  Sys.setenv(PYTHONUTF8="1",PYTHONIOENCODING="utf-8")
  status<-system2(python,vapply(c(file.path(root,path),args),shQuote,character(1)))
  if(!identical(status,0L))stop("Renderer/verification failed: ",basename(path))
  invisible(TRUE)
}
verify_report_assets <- function(root) {
  base<-file.path(root,"support/assets");m<-jsonlite::read_json(file.path(base,"manifest.json"),simplifyVector=TRUE)
  for(i in seq_len(nrow(m$files)))stopifnot(digest::digest(file=file.path(base,m$files$file[i]),algo="sha256")==m$files$sha256[i])
}
prepare_aggregate_inputs <- function(root,input_zip="",work_dir,cache_dir,join_dir="") {
  cache_dir<-project_path(cache_dir,root);work_dir<-project_path(work_dir,root)
  if(nzchar(input_zip))input_zip<-project_path(input_zip,root)
  if(!nzchar(join_dir)) {
    if(!nzchar(input_zip))stop("Supply an input ZIP")
    join_dir<-file.path(work_dir,"linkage")
    build_administrative_join(root,input_zip,join_dir,cache_dir)
  } else join_dir<-project_path(join_dir,root)
  if(!file.exists(file.path(join_dir,"audit/facility_linkage_inputs.rds")))join_dir<-file.path(join_dir,"admin_reference")
  stopifnot(file.exists(file.path(join_dir,"checked_geography/lagos_women_manitoba.rds")))
  if(nzchar(input_zip)) {
    verify_input_bundle(input_zip,cache_dir)
    record<-jsonlite::read_json(file.path(join_dir,"run_parameters.json"),simplifyVector=TRUE)
    stopifnot(record$input_sha256==digest::digest(file=input_zip,algo="sha256"),record$geography_version=="both")
  }
  list(join_dir=join_dir,cache_dir=cache_dir,bundle_sha256=if(nzchar(input_zip))digest::digest(file=input_zip,algo="sha256")else NA_character_)
}
prepare_linkage <- function(root,input_zip,work_dir,cache_dir,join_dir="") {
  input_zip<-project_path(input_zip,root);cache_dir<-project_path(cache_dir,root);work_dir<-project_path(work_dir,root)
  if(!nzchar(join_dir)){join_dir<-file.path(work_dir,"linkage");build_residence_join(root,input_zip,join_dir,cache_dir)}
  else join_dir<-project_path(join_dir,root)
  verify_input_bundle(input_zip,cache_dir)
  a<-jsonlite::read_json(file.path(join_dir,"run_parameters.json"),simplifyVector=TRUE)
  b<-jsonlite::read_json(file.path(join_dir,"admin_reference/run_parameters.json"),simplifyVector=TRUE)
  stopifnot(a$geography_version=="both",b$input_sha256==digest::digest(file=input_zip,algo="sha256"))
  list(join_dir=join_dir,cache_dir=cache_dir,bundle_sha256=b$input_sha256)
}
render_score_threshold_appendix <- function(root,country,data_dir,html,python="python3",workflow="administrative_area") {
  run_python(root,"support/renderers/build_threshold_map_report.py",c("--countries",country,"--workflow",workflow,"--source-report",html,
    "--data-file",file.path(data_dir,"report_data.json"),"--output",file.path(data_dir,"score_threshold_maps.html"),"--embed-in-source"),python)
}
report_arguments <- function(default_work,default_output) {
  args<-commandArgs(TRUE)
  value<-function(k,default=""){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Supply ",k);args[i+1L]}
  list(input_zip=value("--input-zip",Sys.getenv("SURGO_MANITOBA_INPUT_ZIP","")),join_dir=value("--join-dir"),
       cache_dir=value("--cache-dir","data/cache"),work_dir=value("--work-dir",default_work),output_dir=value("--output-dir",default_output),
       python=value("--python",Sys.getenv("SURGO_MANITOBA_PYTHON","python3")),pandoc_dir=value("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC","")))
}
