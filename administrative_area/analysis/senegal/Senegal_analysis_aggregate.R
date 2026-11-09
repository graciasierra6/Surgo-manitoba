# Department-only Senegal report. All external inputs are supplied at runtime.
build_senegal_aggregate <- function(root, join_dir, cache_dir, work_dir,
                                    python = "python3", input_zip = "") {
  root <- normalizePath(root, mustWork = TRUE)
  source(file.path(root, "administrative_area/analysis/report_pipeline.R"), local = TRUE)
  use_project_library(root)
  if (nzchar(join_dir)) join_dir <- project_path(join_dir, root)
  cache_dir <- project_path(cache_dir, root)
  work_dir <- project_path(work_dir, root)
  context <- prepare_aggregate_inputs(root, input_zip, work_dir, cache_dir, join_dir)
  join_dir <- context$join_dir
  cache_dir <- context$cache_dir
  verify_report_assets(root)
  module <- file.path(root, "administrative_area/analysis/senegal")
  admin <- join_dir
  data <- file.path(work_dir, "Senegal_analysis_aggregate_files")
  dir.create(data, recursive = TRUE, showWarnings = FALSE)
  inputs <- c(
    "--input", file.path(admin, "checked_geography/senegal_women_manitoba.rds"),
    "--facility-input", file.path(admin, "audit/facility_linkage_inputs.rds"),
    "--raw", file.path(cache_dir, "inputs/surgo/senegal/surgo_senegal_women.dta"),
    "--raw-facility", file.path(cache_dir, "inputs/uom/senegal/uom_senegal_facility_pooled.dta"),
    "--department-lookup", file.path(cache_dir, "lookups/senegal_health_district_department_evidence.csv")
  )
  required <- inputs[seq(2, length(inputs), by = 2)]
  if (!all(file.exists(required))) stop("Missing parameterized inputs: ",
    paste(basename(required[!file.exists(required)]), collapse = ", "))
  hashes <- unname(tools::md5sum(required))
  stage <- function(name, args) run_stage(file.path(module, name), args, python)
  stage("build_analysis.R", c(inputs, "--output-dir", data))
  stage("prepare_map.R", c("--boundary", file.path(root,
    "analysis/assets/boundaries/senegal_departments.geojson"), "--data-dir", data))
  stage("fit_models.R", c("--data-dir", data, "--output-dir", file.path(data, "models_zscore")))
  pca <- file.path(data, "pca_appendix")
  stage("build_pca_scores.R", c("--data-dir", data, "--output-dir", pca))
  stage("fit_models.R", c("--data-dir", data, "--output-dir", file.path(pca, "models"),
    "--score-method", "pca", "--pca-score-file", file.path(pca, "pca_scores.rds")))
  five <- file.path(data, "five_measure_reference")
  stage("build_analysis.R", c(inputs, "--output-dir", five, "--counseling", "exclude"))
  stage("fit_models.R", c("--data-dir", five, "--output-dir", file.path(five, "models_zscore")))
  sensitivity <- file.path(data, "complete_facility_sensitivity")
  stage("build_analysis.R", c(inputs, "--output-dir", sensitivity,
    "--facility-sample", "complete", "--counseling", "exclude", "--reference-dir", five))
  stage("fit_models.R", c("--data-dir", sensitivity,
    "--output-dir", file.path(sensitivity, "models_zscore")))
  stage("build_analysis.R", c(inputs, "--output-dir", file.path(data, "complete_demand_sensitivity"),
    "--demand-sample", "complete", "--reference-dir", data))
  stage("verify_analysis.R", c(inputs[c(1:2, 5:6, 3:4)], "--data-dir", data))
  native <- file.path(work_dir, "Senegal_analysis_aggregate.html")
  stage("render_analysis.py", c("--data-dir", data, "--output", native))
  stage("verify_html.py", c("--data-dir", data, "--html", native))
  fragment <- file.path(work_dir, "report_fragment.html")
  run_stage(file.path(root, "analysis/common/html_fragment.py"),
    c("--input", native, "--output", fragment), python)
  if (!identical(hashes, unname(tools::md5sum(required)))) stop("Source inputs changed")
  list(native = native, fragment = fragment, data_dir = data)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  value <- function(k, default = "") {
    i <- match(k, args)
    if (is.na(i)) return(default)
    if (i == length(args)) stop("Supply a value for ", k)
    args[i + 1L]
  }
  if ("--help" %in% args) {
    cat("Rscript administrative_area/analysis/senegal/Senegal_analysis_aggregate.R\n",
        "  --join-dir <verified administrative-area join output>\n",
        "  --cache-dir <extracted input bundle> [--input-zip <bundle.zip>]\n",
        "  [--work-dir output/aggregate_reports/senegal] [--output-dir reports]\n")
    quit(status = 0)
  }
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  script <- gsub("~+~", " ", script, fixed = TRUE)
  root <- normalizePath(file.path(dirname(script), "..", "..", ".."), mustWork = TRUE)
  join <- value("--join-dir")
  cache <- value("--cache-dir", "data/cache")
  if (!nzchar(join) && !nzchar(value("--input-zip", Sys.getenv("SURGO_MANITOBA_INPUT_ZIP", "")))) stop("Supply --join-dir or --input-zip")
  python <- value("--python", Sys.getenv("SURGO_MANITOBA_PYTHON", "python3"))
  pandoc_dir <- value("--pandoc-dir", Sys.getenv("RSTUDIO_PANDOC", ""))
  if (nzchar(pandoc_dir)) Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
  report <- build_senegal_aggregate(root, join, cache,
    value("--work-dir", "output/aggregate_reports/senegal"), python,
    value("--input-zip", Sys.getenv("SURGO_MANITOBA_INPUT_ZIP", "")))
  output <- value("--output-dir", "reports")
  if (!grepl("^/|^[A-Za-z]:", output)) output <- file.path(root, output)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  rmarkdown::render(file.path(root, "administrative_area/analysis/senegal/Senegal_analysis_aggregate.Rmd"),
    output_file = "Senegal_analysis_aggregate.html", output_dir = normalizePath(output),
    params = list(prepared_fragment = report$fragment), quiet = TRUE,
    envir = new.env(parent = globalenv()))
  source(file.path(root, "administrative_area/analysis/report_pipeline.R"))
  run_stage(file.path(root, "administrative_area/analysis/senegal/verify_html.py"),
    c("--data-dir", report$data_dir, "--html", file.path(output, "Senegal_analysis_aggregate.html")), python)
  cat("Saved Senegal_analysis_aggregate.html\n")
}
