#!/usr/bin/env Rscript
# One entry point for the four woman-level joins. No model fitting is performed.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1])
root <- normalizePath(file.path(dirname(script), ".."), mustWork=TRUE)
args <- commandArgs(trailingOnly=TRUE)
if ("--help" %in% args) {
  cat("Usage: Rscript R/run_join.R --input-zip /path/inputs.zip [--output-dir output] [--cache-dir data/cache] [--geography-version github|both] [--country-config R/00_config.R]\n")
  quit(status=0)
}
run_options <- list(input_zip=Sys.getenv("SURGO_MANITOBA_INPUT_ZIP", ""), output_dir="output",
                cache_dir="data/cache", geography_version="github", country_config="R/00_config.R")
if (length(args) %% 2 != 0) stop("Arguments must be --name value pairs. Use --help.")
if (length(args)) for (i in seq(1,length(args),by=2)) {
  key <- gsub("-", "_", sub("^--", "", args[i]))
  if (!key %in% names(run_options)) stop("Unknown argument: ", args[i])
  run_options[[key]] <- args[i+1]
}
if (run_options$input_zip=="") stop("Provide --input-zip or set SURGO_MANITOBA_INPUT_ZIP.")
if (!run_options$geography_version %in% c("github","both")) stop("--geography-version must be github or both.")
required <- c("haven","dplyr","readr","jsonlite","digest")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Install required R packages first: ", paste(missing,collapse=", "))
resolve <- function(x) {
  x <- path.expand(x)
  if (!grepl("^/|^[A-Za-z]:|^\\\\",x)) x<-file.path(root,x)
  normalizePath(x,mustWork=FALSE)
}
bundle <- resolve(run_options$input_zip)
output <- resolve(run_options$output_dir)
cache <- resolve(run_options$cache_dir)
config <- resolve(run_options$country_config)
if (!file.exists(bundle)) stop("Input ZIP not found: ",bundle)
if (!file.exists(config)) stop("Country configuration not found: ",config)
if (identical(output,cache)) stop("Output and cache directories must differ.")
dir.create(output,recursive=TRUE,showWarnings=FALSE)
dir.create(cache,recursive=TRUE,showWarnings=FALSE)
members <- utils::unzip(bundle,list=TRUE)$Name
unsafe <- grepl("^/|^[A-Za-z]:",members) |
  vapply(strsplit(members,"/",fixed=TRUE),function(x)any(x==".."),logical(1))
if (any(unsafe)) stop("Input ZIP contains an unsafe member path.")
if (!"bundle_manifest.json" %in% members) stop("ZIP lacks bundle_manifest.json; use the supplied input bundle.")
tmp <- tempfile("linkage_manifest_");dir.create(tmp)
utils::unzip(bundle,files="bundle_manifest.json",exdir=tmp)
manifest <- jsonlite::read_json(file.path(tmp,"bundle_manifest.json"),simplifyVector=TRUE)
if (!identical(as.integer(manifest$schema_version),1L)) stop("Unsupported input-bundle schema.")
files <- manifest$files
if (!all(files$member %in% members)) stop("Manifest lists files missing from ZIP.")
sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
message("Verifying and preparing ",nrow(files)," input files")
for (i in seq_len(nrow(files))) {
  member <- files$member[i]
  target <- file.path(cache,member)
  ok <- file.exists(target) && file.info(target)$size==files$bytes[i] && sha(target)==files$sha256[i]
  if (!ok) {
    utils::unzip(bundle,files=member,exdir=cache)
    if (!file.exists(target)||file.info(target)$size!=files$bytes[i]||sha(target)!=files$sha256[i])
      stop("Input integrity check failed: ",member)
  }
}
jsonlite::write_json(manifest,file.path(cache,"_input_manifest.json"),auto_unbox=TRUE,pretty=TRUE)
Sys.setenv(LINKAGE_INPUT_DIR=cache,LINKAGE_OUTPUT_DIR=output,LINKAGE_COUNTRY_CONFIG=config,
           LINKAGE_EXPORT_MODES=if(run_options$geography_version=="both")"pipeline_crosswalk,audited_geography"else"pipeline_crosswalk")
jsonlite::write_json(list(input_bundle=basename(bundle),input_sha256=sha(bundle),
  geography_version=run_options$geography_version,keep_all_source_women=TRUE,lagos_sample="main",
  country_config=basename(config),input_file_count=nrow(files)),
  file.path(output,"run_parameters.json"),auto_unbox=TRUE,pretty=TRUE)
source(file.path(root,"R/04_join.R"),local=.GlobalEnv)
source(file.path(root,"R/05_validate.R"),local=.GlobalEnv)
capture.output(sessionInfo(),file=file.path(output,"audit/R_session_info.txt"))
message("Join and reference validation complete. Output: ",output)
