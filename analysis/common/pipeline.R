# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: digest, jsonlite, utils.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Shared pipeline helper.
# Inputs: Caller-supplied data objects, runtime file arguments or configured input bundle members.
# Outputs: Shared helper functions/results; used through documented entry points.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Shared orchestration for the four country reports. Source from a country Rmd
# or R/run_reports.R. Confidential locations are supplied only at runtime.
use_project_library <- function(root) {
  if (dir.exists(file.path(root, "renv/library"))) {
    library <- file.path(root, "renv", "library", paste0("R-", getRversion()), R.version$platform)
    if (dir.exists(library)) {
      .libPaths(c(library, .libPaths()))
      Sys.setenv(R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep))
    }
  }
  invisible(TRUE)
}

project_path <- function(path, root) {
  path <- path.expand(path)
  if (!grepl("^/|^[A-Za-z]:|^\\\\", path)) path <- file.path(root, path)
  normalizePath(path, mustWork = FALSE)
}

run_stage <- function(script, args = character(), python = "python3") {
  # Keep accented geography names stable across platform locale defaults.
  encoding_env <- Sys.getenv(c("PYTHONUTF8", "PYTHONIOENCODING"), unset = NA_character_)
  on.exit(for (name in names(encoding_env)) {
    if (is.na(encoding_env[[name]])) Sys.unsetenv(name)
    else do.call(Sys.setenv, setNames(list(encoding_env[[name]]), name))
  }, add = TRUE)
  Sys.setenv(PYTHONUTF8 = "1", PYTHONIOENCODING = "utf-8")
  executable <- if (grepl("\\.R$", script)) file.path(R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript") else python
  status <- system2(executable, vapply(c(script, args), shQuote, character(1)))
  if (!identical(status, 0L)) stop("Stage failed: ", basename(script), call. = FALSE)
  invisible(TRUE)
}

# Sensitivity analyses ---------------------------------------------------------
# Area-score alternatives and SD cutoffs are computed in risk_scores.R. This
# stage renders their distributions and the zero, 0.5, 1 and 1.5 SD maps using
# the existing boundaries and palette, then embeds a self-contained appendix.
render_score_threshold_appendix <- function(root, country, data_dir, html,
                                            python = "python3", workflow = "administrative_area") {
  run_stage(file.path(root, "analysis/build_threshold_map_report.py"),
    c("--countries", country, "--workflow", workflow,
      "--source-report", html, "--data-file", file.path(data_dir, "report_data.json"),
      "--output", file.path(data_dir, "score_threshold_maps.html"),
      "--embed-in-source"), python)
  invisible(TRUE)
}

verify_report_assets <- function(root) {
  base <- file.path(root, "analysis/assets")
  manifest <- jsonlite::read_json(file.path(base, "manifest.json"), simplifyVector = TRUE)
  for (i in seq_len(nrow(manifest$files))) {
    p <- file.path(base, manifest$files$file[i])
    if (!file.exists(p) || digest::digest(file = p, algo = "sha256") != manifest$files$sha256[i])
      stop("Display/reference asset integrity check failed: ", manifest$files$file[i])
  }
  invisible(TRUE)
}

prepare_linkage <- function(root, input_zip, work_dir, cache_dir, join_dir = "") {
  if (!file.exists(input_zip)) stop("Supply a valid input ZIP through --input-zip or SURGO_MANITOBA_INPUT_ZIP")
  bundle_sha <- digest::digest(file = input_zip, algo = "sha256")
  if (!nzchar(join_dir)) {
    join_dir <- file.path(work_dir, "linkage")
    # The existing urban/rural module builds its own administrative reference,
    # validates the bundle and exports both documented geographic alternatives.
    run_stage(file.path(root, "urban_rural/R/run_join_urban_rural.R"),
      c("--input-zip", input_zip, "--output-dir", join_dir,
        "--cache-dir", cache_dir, "--geography-version", "both"))
  }
  parameters <- file.path(join_dir, "run_parameters.json")
  reference <- file.path(join_dir, "admin_reference/run_parameters.json")
  if (!file.exists(parameters) || !file.exists(reference)) stop("Join output is incomplete")
  a <- jsonlite::read_json(parameters, simplifyVector = TRUE)
  b <- jsonlite::read_json(reference, simplifyVector = TRUE)
  if (a$geography_version != "both" || b$input_sha256 != bundle_sha)
    stop("Existing join must contain both geography versions and match the supplied input ZIP")
  # Even when resuming from a join, extract/verify input members needed below.
  manifest_path <- file.path(cache_dir, "bundle_manifest.json")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  members <- utils::unzip(input_zip, list = TRUE)$Name
  if (any(grepl("^/|^[A-Za-z]:|\\\\", members)) ||
      any(vapply(strsplit(members, "/", fixed = TRUE), function(x) any(x == ".."), logical(1))))
    stop("Unsafe ZIP member")
  utils::unzip(input_zip, files = "bundle_manifest.json", exdir = cache_dir)
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  for (i in seq_len(nrow(manifest$files))) {
    f <- manifest$files[i, ]; p <- file.path(cache_dir, f$member)
    valid <- file.exists(p) && file.info(p)$size == f$bytes &&
      digest::digest(file = p, algo = "sha256") == f$sha256
    if (!valid) utils::unzip(input_zip, files = f$member, exdir = cache_dir)
    if (!file.exists(p) || file.info(p)$size != f$bytes ||
        digest::digest(file = p, algo = "sha256") != f$sha256)
      stop("Input integrity check failed: ", f$member)
  }
  list(join_dir = join_dir, cache_dir = cache_dir, bundle_sha256 = bundle_sha)
}

build_country_report <- function(country, root, input_zip, work_dir, cache_dir,
                                 join_dir = "", python = "python3") {
  if (!country %in% c("lagos", "sindh", "senegal", "cotedivoire")) stop("Unknown country")
  for (p in c("jsonlite", "digest", "rmarkdown", "knitr"))
    if (!requireNamespace(p, quietly = TRUE)) stop("Install R package: ", p)
  root <- normalizePath(root, mustWork = TRUE)
  use_project_library(root)
  input_zip <- project_path(input_zip, root)
  work_dir <- project_path(work_dir, root); cache_dir <- project_path(cache_dir, root)
  if (nzchar(join_dir)) join_dir <- project_path(join_dir, root)
  context <- prepare_linkage(root, input_zip, work_dir, cache_dir, join_dir)
  verify_report_assets(root)
  join <- context$join_dir; cache <- context$cache_dir
  if (country != "sindh") {
    # Earlier Rmds retain their original definitions through the pinned source.
    names <- c(lagos="Lagos",senegal="Senegal",cotedivoire="Cote_dIvoire")
    name <- paste0(names[[country]],"_analysis")
    out <- file.path(work_dir,country)
    historical <- file.path(work_dir,"historical_source",country)
    run_stage(file.path(root,"tests/rebuild_historical_reports.py"),
      c("--input-zip",input_zip,"--join-dir",join,"--cache-dir",cache,
        "--work-dir",historical,"--output-dir",out,"--countries",country,"--python",python),python)
    native <- file.path(out,paste0(name,".html"))
    fragment <- file.path(out,"report_fragment.html")
    run_stage(file.path(root,"analysis/common/html_fragment.py"),c("--input",native,"--output",fragment),python)
    return(list(native=native,fragment=fragment,
      data_dir=file.path(historical,"calculation",country,paste0(name,"_files")),
      input_sha256=context$bundle_sha256,country=country))
  }
  if (country == "sindh") {
    env <- new.env(parent = globalenv())
    source(file.path(root,"administrative_area/analysis/sindh/Sindh_analysis.R"),local=env)
    return(env$build_sindh_aggregate(root,file.path(join,"admin_reference"),cache,
      file.path(work_dir,"sindh"),python,""))
  }
  stop("No report builder selected")
}
