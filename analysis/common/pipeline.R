# Shared orchestration for the four country reports. Source from a country Rmd
# or R/run_reports.R. Confidential locations are supplied only at runtime.
use_project_library <- function(root) {
  if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE)) {
    library <- renv::paths$library(project = root)
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
  executable <- if (grepl("\\.R$", script)) file.path(R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript") else python
  status <- system2(executable, vapply(c(script, args), shQuote, character(1)))
  if (!identical(status, 0L)) stop("Stage failed: ", basename(script), call. = FALSE)
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
  admin <- file.path(join, "admin_reference")
  fac <- file.path(admin, "audit/facility_linkage_inputs.rds")
  module <- file.path(root, "analysis", country)
  names <- c(lagos = "Lagos", sindh = "Sindh", senegal = "Senegal", cotedivoire = "Cote_dIvoire")
  output_name <- paste0(names[[country]], "_analysis")
  out <- file.path(work_dir, country); data <- file.path(out, paste0(output_name, "_files"))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  native <- file.path(out, paste0(output_name, ".html"))
  bounds <- file.path(root, "analysis/assets/boundaries")
  # Sindh intentionally retains the source-crosswalk district subset used in the
  # existing report; the other country analyses use checked geography.
  if (country == "lagos") {
    original <- file.path(join, "checked_geography/lagos_women_manitoba_urban_rural.rds")
    exception <- file.path(out, "analysis_join")
    run_stage(file.path(module, "apply_ibeju_free_provision_exception.R"),
      c("--input", original, "--output-dir", exception))
    input <- file.path(exception, "lagos_women_manitoba_urban_rural.rds")
    run_stage(file.path(module, "build_analysis.R"),
      c("--input", input, "--facility-input", fac, "--output-dir", data))
    model <- file.path(data, "models_urban_rural")
    fit_args <- c("--input", input, "--data-dir", data, "--demand-resolution", "individual",
                  "--supply-resolution", "urban_rural", "--missing-popular", "lga_one_component")
    run_stage(file.path(module, "fit_models.R"), c(fit_args, "--output-dir", model))
    pca <- file.path(data, "pca_appendix")
    run_stage(file.path(module, "build_pca_scores.R"),
      c("--input", input, "--data-dir", data, "--output-dir", pca))
    run_stage(file.path(module, "fit_models.R"),
      c(fit_args, "--output-dir", file.path(pca, "models"), "--score-method", "pca",
        "--pca-score-file", file.path(pca, "pca_scores.rds")))
    run_stage(file.path(root, "tests/verify_lagos.R"),
      c("--facility-input", fac, "--model-dir", model, "--data-dir", data))
    run_stage(file.path(module, "render_analysis.py"),
      c("--data-dir", data, "--reference-html", file.path(root, "analysis/assets/lagos_map_template.html"),
        "--model-dir", model, "--pca-dir", pca,
        "--output", native), python)
  } else {
    raw_name <- if (country == "sindh") "20250124_forms_wt.dta" else paste0("surgo_", country, "_women.dta")
    args <- c("--raw", file.path(cache, "inputs/surgo", country, raw_name),
              "--facility-input", fac, "--output-dir", out)
    if (country == "sindh") {
      args <- c(args, "--input", file.path(admin, "datasets/sindh_women_manitoba.rds"),
        "--need-file", file.path(cache, "surgo_pipeline/sindh/sindh_wra_design_ready.rds"),
        "--boundary", file.path(bounds, "sindh_districts.geojson"))
    } else {
      facility_country <- if (country == "cotedivoire") "cdi" else country
      args <- c(args, "--input", file.path(join, "checked_geography", paste0(country, "_women_manitoba_urban_rural.rds")),
        "--admin-input", file.path(admin, "checked_geography", paste0(country, "_women_manitoba.rds")),
        "--raw-facility", file.path(cache, "inputs/uom", facility_country,
                                    paste0("uom_", facility_country, "_facility_pooled.dta")))
      if (country == "senegal") args <- c(args,
        "--residence-lookup", file.path(cache, "inputs/SenegalURlookup.xlsx"),
        "--department-lookup", file.path(cache, "lookups/senegal_health_district_department_evidence.csv"),
        "--boundary", file.path(bounds, "senegal_departments.geojson"))
      else args <- c(args,
        "--district-codebook", file.path(cache, "lookups/cdi_health_district_codebook.csv"),
        "--boundary", file.path(bounds, "civ_departments_2018.geojson"),
        "--subprefecture-boundary", file.path(bounds, "civ_subprefectures_2018.geojson"))
    }
    pick <- function(fields) unlist(lapply(fields, function(field) {
      i <- match(paste0("--", field), args)
      if (!is.na(i)) args[c(i, i + 1L)] else character()
    }), use.names = FALSE)
    run_stage(file.path(module, "build_analysis.R"),
      c(pick(c("input", "admin-input", "raw", "facility-input", "raw-facility",
               "need-file", "residence-lookup", "department-lookup", "district-codebook")),
        "--output-dir", data))
    run_stage(file.path(module, "prepare_map.R"),
      c(pick(c("boundary", "subprefecture-boundary")), "--data-dir", data))
    run_stage(file.path(module, "fit_models.R"),
      c("--data-dir", data, "--output-dir", file.path(data, "models_zscore")))
    pca <- file.path(data, "pca_appendix")
    run_stage(file.path(module, "build_pca_scores.R"),
      c("--data-dir", data, "--output-dir", pca))
    run_stage(file.path(module, "fit_models.R"),
      c("--data-dir", data, "--output-dir", file.path(pca, "models"),
        "--score-method", "pca", "--pca-score-file", file.path(pca, "pca_scores.rds")))
    run_stage(file.path(root, "tests", paste0("verify_", country, ".R")),
      c(pick(c("input", "admin-input", "raw", "need-file", "facility-input")), "--data-dir", data))
    run_stage(file.path(module, "render_analysis.py"),
      c("--data-dir", data, "--output", native), python)
  }
  run_stage(file.path(root, "tests", paste0("verify_", country, "_html.py")),
    c("--data-dir", data, "--html", native), python)
  # Retain the substantive report's inline figures/CSS/JS. R Markdown supplies
  # the final document container using a minimal, offline Pandoc template.
  fragment <- file.path(out, "report_fragment.html")
  run_stage(file.path(root, "analysis/common/html_fragment.py"),
    c("--input", native, "--output", fragment), python)
  list(native = native, fragment = fragment, data_dir = data,
       input_sha256 = context$bundle_sha256, country = country)
}
