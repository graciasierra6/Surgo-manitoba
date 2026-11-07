#!/usr/bin/env Rscript
# Rebuild joins and knit the four reports from one checksummed external bundle.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args) {
  cat(paste0("Usage: Rscript R/run_reports.R [--input-zip FILE] [--countries all|lagos,sindh,senegal,cotedivoire]\n",
    "  [--output-dir reports] [--work-dir output/report_build] [--cache-dir data/cache]\n",
    "  [--join-dir EXISTING_VERIFIED_URBAN_RURAL_OUTPUT] [--python python3]\n",
    "Input defaults to SURGO_MANITOBA_INPUT_ZIP. Paths resolve from the repository root.\n"))
  quit(status = 0)
}
opts <- list(input_zip = Sys.getenv("SURGO_MANITOBA_INPUT_ZIP", ""), countries = "all",
  output_dir = "reports", work_dir = "output/report_build", cache_dir = "data/cache",
  join_dir = "", python = Sys.getenv("SURGO_MANITOBA_PYTHON", "python3"))
if (length(args) %% 2 != 0) stop("Arguments must be --name value pairs")
if (length(args)) for (i in seq(1, length(args), by = 2)) {
  key <- gsub("-", "_", sub("^--", "", args[i]))
  if (!key %in% names(opts)) stop("Unknown argument: ", args[i])
  opts[[key]] <- args[i + 1L]
}
source(file.path(root, "analysis/common/pipeline.R"))
use_project_library(root)
for (p in c("rmarkdown", "knitr", "jsonlite", "digest"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Install dependencies with Rscript R/install_dependencies.R")
if (!rmarkdown::pandoc_available()) stop("Pandoc is required; install it or set RSTUDIO_PANDOC to its directory")
if (!nzchar(opts$input_zip)) stop("Supply --input-zip or SURGO_MANITOBA_INPUT_ZIP")
for (k in c("input_zip", "output_dir", "work_dir", "cache_dir")) opts[[k]] <- project_path(opts[[k]], root)
if (nzchar(opts$join_dir)) opts$join_dir <- project_path(opts$join_dir, root)
countries <- if (opts$countries == "all") c("lagos", "sindh", "senegal", "cotedivoire") else strsplit(opts$countries, ",", fixed = TRUE)[[1]]
if (anyDuplicated(countries) || !all(countries %in% c("lagos", "sindh", "senegal", "cotedivoire"))) stop("Invalid country selection")
context <- prepare_linkage(root, opts$input_zip, opts$work_dir, opts$cache_dir, opts$join_dir)
dir.create(opts$output_dir, recursive = TRUE, showWarnings = FALSE)
reports <- c(lagos = "Lagos_analysis", sindh = "Sindh_analysis", senegal = "Senegal_analysis", cotedivoire = "Cote_dIvoire_analysis")
records <- list()
for (country in countries) {
  message("Building and knitting: ", country)
  name <- reports[[country]]
  target <- rmarkdown::render(file.path(root, "analysis", country, paste0(name, ".Rmd")),
    output_file = paste0(name, ".html"), output_dir = opts$output_dir,
    intermediates_dir = file.path(opts$work_dir, country, "knit"),
    params = list(input_zip = opts$input_zip, work_dir = opts$work_dir,
                  cache_dir = opts$cache_dir, join_dir = context$join_dir, python = opts$python),
    envir = new.env(parent = globalenv()), quiet = TRUE)
  # Recheck the final R Markdown output, not just the pre-knit document.
  data <- file.path(opts$work_dir, country, paste0(name, "_files"))
  run_stage(file.path(root, "tests", paste0("verify_", country, "_html.py")), c("--data-dir", data, "--html", target), opts$python)
  # The custom offline template does not use R Markdown's automatic header helper.
  # Remove only that recognized, unused dependency; never remove analysis data.
  dependencies <- file.path(opts$output_dir, paste0(name, "_files"))
  members <- list.files(dependencies, recursive = TRUE, all.files = TRUE)
  if (length(members) && all(grepl("^header-attrs-[^/]+/header-attrs[.]js$", members)))
    unlink(dependencies, recursive = TRUE)
  records[[country]] <- list(file = basename(target), sha256 = digest::digest(file = target, algo = "sha256"))
}
packages <- c("rmarkdown", "knitr", "haven", "dplyr", "readr", "readxl", "jsonlite", "digest", "sf", "survey", "clubSandwich", "ggplot2", "svglite")
versions <- setNames(lapply(packages, function(p) as.character(utils::packageVersion(p))), packages)
jsonlite::write_json(list(schema_version = 1, input_sha256 = context$bundle_sha256,
  r_version = as.character(getRversion()), packages = versions,
  pandoc_version = as.character(rmarkdown::pandoc_version()), reports = records),
  file.path(opts$output_dir, "build_manifest.json"), auto_unbox = TRUE, pretty = TRUE)
message("Completed ", length(countries), " R Markdown reports: ", opts$output_dir)
