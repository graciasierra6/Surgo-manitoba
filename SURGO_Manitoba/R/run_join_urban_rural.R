#!/usr/bin/env Rscript
# Separate entry point; the original administrative-only entry point is unchanged.
# Input: same checksummed ZIP as run_join.R; all paths are runtime arguments.
# Output: a separate directory with admin_reference/, exact area/type profiles,
# woman-level DTA/RDS files and coverage/validation audits. No models are fitted.
# Policy: retain women, zero unmatched profile record counts, leave rates missing.
# See README for Senegal/Sindh qualifications and the unimplemented area fallback.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args) {
  cat(paste0("Usage: Rscript R/run_join_urban_rural.R --input-zip inputs.zip ",
             "[--output-dir output_urban_rural] [--cache-dir data/cache] ",
             "[--geography-version github|both] [--country-config R/00_config.R] ",
             "[--urban-rural-config R/urban_rural_config.R] [--sindh-residence-lookup residence.csv]\n"))
  quit(status = 0)
}
run_options <- list(input_zip = Sys.getenv("SURGO_MANITOBA_INPUT_ZIP", ""),
                    output_dir = "output_urban_rural", cache_dir = "data/cache",
                    geography_version = "github", country_config = "R/00_config.R",
                    urban_rural_config = "R/urban_rural_config.R", sindh_residence_lookup = "")
if (length(args) %% 2 != 0) stop("Arguments must be --name value pairs. Use --help.")
if (length(args)) for (i in seq(1, length(args), by = 2)) {
  key <- gsub("-", "_", sub("^--", "", args[i]))
  if (!key %in% names(run_options)) stop("Unknown argument: ", args[i])
  run_options[[key]] <- args[i + 1]
}
if (!nzchar(run_options$input_zip)) stop("Provide --input-zip or set SURGO_MANITOBA_INPUT_ZIP.")
if (!run_options$geography_version %in% c("github", "both")) stop("--geography-version must be github or both.")
required <- c("haven", "dplyr", "readr", "jsonlite", "digest", "readxl")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required R packages first: ", paste(missing, collapse = ", "))
resolve <- function(x) {
  x <- path.expand(x)
  if (!grepl("^/|^[A-Za-z]:|^\\\\", x)) x <- file.path(root, x)
  normalizePath(x, mustWork = FALSE)
}
input_bundle <- resolve(run_options$input_zip)
out <- resolve(run_options$output_dir)
input <- resolve(run_options$cache_dir)
config <- resolve(run_options$country_config)
rurb_config_path <- resolve(run_options$urban_rural_config)
sindh_lookup <- if (nzchar(run_options$sindh_residence_lookup)) resolve(run_options$sindh_residence_lookup) else ""
if (!file.exists(rurb_config_path)) stop("Urban/rural configuration not found")
if (nzchar(sindh_lookup) && !file.exists(sindh_lookup)) stop("Sindh residence lookup not found")
if (identical(out, input)) stop("Output and cache directories must differ")
previous_run <- file.path(out, "run_parameters.json")
if (file.exists(previous_run)) {
  previous <- jsonlite::read_json(previous_run, simplifyVector = TRUE)
  if (!identical(previous$component, "administrative_area_urban_rural"))
    stop("Choose a separate output directory; this directory contains another join component")
}
base_out <- file.path(out, "admin_reference")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
# Reuse the original preparation, SHA-256 verification and reference checks.
child_args <- c("R/run_join.R", "--input-zip", input_bundle,
                "--output-dir", base_out, "--cache-dir", input,
                "--country-config", config, "--geography-version", run_options$geography_version)
previous_directory <- getwd()
status <- tryCatch({
  setwd(root)
  executable <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  system2(file.path(R.home("bin"), executable), args = vapply(child_args, shQuote, character(1)))
}, finally = setwd(previous_directory))
if (status != 0) stop("Administrative preparation/validation failed; urban/rural join was not run")
Sys.setenv(LINKAGE_COUNTRY_CONFIG = config)
source(file.path(root, "R/helpers.R"))
source(file.path(root, "R/aggregate_supply.R"))
source(file.path(root, "R/urban_rural_helpers.R"))
rurb_config <- source(rurb_config_path, local = new.env(parent = baseenv()))$value
source(file.path(root, "R/06_join_urban_rural.R"))
capture.output(sessionInfo(), file = file.path(out, "audit/R_session_info.txt"))
message("Separate administrative-area plus urban/rural component complete: ", out)
