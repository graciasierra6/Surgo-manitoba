#!/usr/bin/env Rscript
# Lagos | Aggregate administrative-area analysis
#
# QUICK START IN RSTUDIO
#   1. Open this R script in a fresh RStudio session.
#   2. Click Source in the script-editor toolbar (runs the whole file).
#   3. Select the confidential input ZIP in the native file picker.
#   The script automatically installs/loads its recorded R packages, prepares
#   the data, runs the analysis and sensitivities, then builds the HTML/maps.
#   The console prints the output folder. Open the resulting .html in a browser.
#
# QUICK START FROM A TERMINAL / POWERSHELL (Rscript must be on PATH)
#   Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip INPUT.zip
#   Use --help to see optional output/cache/software arguments.
#
# REQUIREMENTS
#   R (validation: 4.6.0), Python 3 and Pandoc (normally bundled with RStudio).
#   R libraries install automatically at startup; no separate R helper is needed.
#   Run from the full repo to use its local public display assets. A single
#   downloaded script obtains the same pinned, verified public map/layout files
#   automatically. First-time package/resource downloads require internet.
#   Only public assets are downloaded; confidential input data are never sent.
#   --no-html runs numerical results in R without Python/Pandoc/display assets.
#
# ORIGINAL INPUT DATASETS (exact bundle members):
#   SURGO women:
#     inputs/surgo/lagos/surgo_lagos_women.csv
#   Original facility survey:
#     inputs/uom/lagos/uom_lagos_facility_pooled.dta
#   Cleaned facility indicators:
#     uom_pipeline/uom_summary_facility.csv
#   Facility archetypes:
#     uom_pipeline/Facilities data with archetype assignments.csv
#   Facility volumes:
#     uom_pipeline/new_demand_vol_facility.csv
#   Reviewed upstream crosswalk:
#     exports/uom_surgo_geo_crosswalk_reviewed.csv
#   Supply column definitions:
#     exports/merged_supply_demand_dictionary.csv
#   Original facility geography check:
#     merge_pipeline/uom_facility_geo.csv
#   Upstream assignment check:
#     merge_pipeline/uom_summary_facility_crosswalked.csv
#   Upstream area-summary check:
#     exports/merged_supply_demand_department.csv
#   With ZIP input: bundle_manifest.json verifies all supplied archive members.
#   Extracted-directory input: the files listed above retain their relative names.
#   Geographic evidence may cite lookups/geographic_sources.json for provenance;
#   the assignment code reads the explicit country mapping listed above.
#
# All recodes, joins, facility aggregation, scores, models, implemented
# sensitivities and independent numerical checks are included in this file.
# Main SURGO analyses use women in need of FP; coverage retains all source women.
# SURGO source weight: weight -> internal weight; sampling design ids=~1,
# no strata/FPC. Main regression standard errors cluster by LGA (CR2).
# Facility averages use confirmed FP providers, equal facility weight and valid
# responses per measure (including genuine 0/FALSE; excluding undefined values).
# SURGO uses survey weights, ids=~1, no strata/FPC; main regression inference
# separately uses administrative-area CR2/Satterthwaite clustering.
# Source cluster/psu fields are retained for reference, but are not used as
# sampling IDs in this design; regression clustering uses the assigned geography.
# Scores run higher = more demand barriers/supply constraints; primary flags >0.
# Existing scientific definitions and findings are unchanged by this interface.
# Generated inputs, joined microdata, model objects and package libraries stay
# in ignored working outputs and must never be uploaded to the repository.

# 1. User settings and automatic package setup ---------------------------------

input_zip <- Sys.getenv("SURGO_MANITOBA_INPUT_ZIP", "")
input_dir <- Sys.getenv("SURGO_MANITOBA_INPUT_DIR", "")
render_html <- TRUE
install_missing_packages <- TRUE
check_package_versions <- TRUE
run_analysis <- Sys.getenv("SURGO_ANALYSIS_RUN", "true") != "false" &&
  !isTRUE(getOption("surgo.define_only", FALSE)) && (interactive() || sys.nframe() == 0L)

.source_files <- vapply(sys.frames(), function(frame) {
  if (is.null(frame$ofile)) "" else as.character(frame$ofile)[1]
}, character(1))
.source_files <- .source_files[nzchar(.source_files)]
.entry <- grep("^--file=", commandArgs(), value = TRUE)
.report_file <- if (length(.source_files)) tail(.source_files, 1) else if (length(.entry))
  gsub("~+~", " ", sub("^--file=", "", .entry[1]), fixed = TRUE) else "Lagos_analysis_aggregate.R"
.report_file <- normalizePath(.report_file, mustWork = TRUE)
script_dir <- dirname(.report_file)

required_packages <- c("haven", "dplyr", "readr", "readxl", "jsonlite", "digest", "sf", "survey", "clubSandwich", "ggplot2", "svglite", "tidyselect", "rmarkdown", "knitr", "base64enc")
# Only sf, rmarkdown and knitr are display-only dependencies.
locked_environment <- list(
  R = list(Version = "4.6.0", Repositories = c(CRAN = "https://cloud.r-project.org")),
  Packages = list(
    DBI = list(Package = "DBI", Version = "1.3.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    KernSmooth = list(Package = "KernSmooth", Version = "2.23-26", Source = "Repository", Repository = "CRAN", Requirements = character()),
    MASS = list(Package = "MASS", Version = "7.3-65", Source = "Repository", Repository = "CRAN", Requirements = character()),
    Matrix = list(Package = "Matrix", Version = "1.7-5", Source = "Repository", Repository = "CRAN", Requirements = c("lattice")),
    R6 = list(Package = "R6", Version = "2.6.1", Source = "Repository", Repository = "CRAN", Requirements = character()),
    RColorBrewer = list(Package = "RColorBrewer", Version = "1.1-3", Source = "Repository", Repository = "CRAN", Requirements = character()),
    Rcpp = list(Package = "Rcpp", Version = "1.1.1-1.1", Source = "Repository", Repository = "CRAN", Requirements = character()),
    RcppArmadillo = list(Package = "RcppArmadillo", Version = "15.2.7-1", Source = "Repository", Repository = "CRAN", Requirements = c("Rcpp")),
    S7 = list(Package = "S7", Version = "0.2.2", Source = "Repository", Repository = "CRAN", Requirements = character()),
    base64enc = list(Package = "base64enc", Version = "0.1-6", Source = "Repository", Repository = "CRAN", Requirements = character()),
    bit = list(Package = "bit", Version = "4.6.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    bit64 = list(Package = "bit64", Version = "4.8.2", Source = "Repository", Repository = "CRAN", Requirements = c("bit")),
    bslib = list(Package = "bslib", Version = "0.11.0", Source = "Repository", Repository = "CRAN", Requirements = c("base64enc", "cachem", "fastmap", "htmltools", "jquerylib", "jsonlite", "lifecycle", "memoise", "mime", "rlang", "sass")),
    cachem = list(Package = "cachem", Version = "1.1.0", Source = "Repository", Repository = "CRAN", Requirements = c("fastmap", "rlang")),
    cellranger = list(Package = "cellranger", Version = "1.1.0", Source = "Repository", Repository = "CRAN", Requirements = c("rematch", "tibble")),
    class = list(Package = "class", Version = "7.3-23", Source = "Repository", Repository = "CRAN", Requirements = c("MASS")),
    classInt = list(Package = "classInt", Version = "0.4-11", Source = "Repository", Repository = "CRAN", Requirements = c("KernSmooth", "class", "e1071")),
    cli = list(Package = "cli", Version = "3.6.6", Source = "Repository", Repository = "CRAN", Requirements = character()),
    clipr = list(Package = "clipr", Version = "0.8.1", Source = "Repository", Repository = "CRAN", Requirements = character()),
    clubSandwich = list(Package = "clubSandwich", Version = "0.7.0", Source = "Repository", Repository = "CRAN", Requirements = c("lifecycle", "sandwich")),
    cpp11 = list(Package = "cpp11", Version = "0.5.5", Source = "Repository", Repository = "CRAN", Requirements = character()),
    crayon = list(Package = "crayon", Version = "1.5.3", Source = "Repository", Repository = "CRAN", Requirements = character()),
    digest = list(Package = "digest", Version = "0.6.39", Source = "Repository", Repository = "CRAN", Requirements = character()),
    dplyr = list(Package = "dplyr", Version = "1.2.1", Source = "Repository", Repository = "CRAN", Requirements = c("R6", "cli", "generics", "glue", "lifecycle", "magrittr", "pillar", "rlang", "tibble", "tidyselect", "vctrs")),
    e1071 = list(Package = "e1071", Version = "1.7-17", Source = "Repository", Repository = "CRAN", Requirements = c("class", "proxy")),
    evaluate = list(Package = "evaluate", Version = "1.0.5", Source = "Repository", Repository = "CRAN", Requirements = character()),
    farver = list(Package = "farver", Version = "2.1.2", Source = "Repository", Repository = "CRAN", Requirements = character()),
    fastmap = list(Package = "fastmap", Version = "1.2.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    fontawesome = list(Package = "fontawesome", Version = "0.5.3", Source = "Repository", Repository = "CRAN", Requirements = c("htmltools", "rlang")),
    forcats = list(Package = "forcats", Version = "1.0.1", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "glue", "lifecycle", "magrittr", "rlang", "tibble")),
    fs = list(Package = "fs", Version = "2.1.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    generics = list(Package = "generics", Version = "0.1.4", Source = "Repository", Repository = "CRAN", Requirements = character()),
    ggplot2 = list(Package = "ggplot2", Version = "4.0.3", Source = "Repository", Repository = "CRAN", Requirements = c("S7", "cli", "gtable", "isoband", "lifecycle", "rlang", "scales", "vctrs", "withr")),
    glue = list(Package = "glue", Version = "1.8.1", Source = "Repository", Repository = "CRAN", Requirements = character()),
    gtable = list(Package = "gtable", Version = "0.3.6", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "glue", "lifecycle", "rlang")),
    haven = list(Package = "haven", Version = "2.5.5", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "cpp11", "forcats", "hms", "lifecycle", "readr", "rlang", "tibble", "tidyselect", "vctrs")),
    highr = list(Package = "highr", Version = "0.12", Source = "Repository", Repository = "CRAN", Requirements = c("xfun")),
    hms = list(Package = "hms", Version = "1.1.4", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "lifecycle", "pkgconfig", "rlang", "vctrs")),
    htmltools = list(Package = "htmltools", Version = "0.5.9", Source = "Repository", Repository = "CRAN", Requirements = c("base64enc", "digest", "fastmap", "rlang")),
    isoband = list(Package = "isoband", Version = "0.3.0", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "cpp11", "rlang")),
    jquerylib = list(Package = "jquerylib", Version = "0.1.4", Source = "Repository", Repository = "CRAN", Requirements = c("htmltools")),
    jsonlite = list(Package = "jsonlite", Version = "2.0.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    knitr = list(Package = "knitr", Version = "1.51", Source = "Repository", Repository = "CRAN", Requirements = c("evaluate", "highr", "xfun", "yaml")),
    labeling = list(Package = "labeling", Version = "0.4.3", Source = "Repository", Repository = "CRAN", Requirements = character()),
    lattice = list(Package = "lattice", Version = "0.22-9", Source = "Repository", Repository = "CRAN", Requirements = character()),
    lifecycle = list(Package = "lifecycle", Version = "1.0.5", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "rlang")),
    magrittr = list(Package = "magrittr", Version = "2.0.5", Source = "Repository", Repository = "CRAN", Requirements = character()),
    memoise = list(Package = "memoise", Version = "2.0.1", Source = "Repository", Repository = "CRAN", Requirements = c("cachem", "rlang")),
    mime = list(Package = "mime", Version = "0.13", Source = "Repository", Repository = "CRAN", Requirements = character()),
    minqa = list(Package = "minqa", Version = "1.2.8", Source = "Repository", Repository = "CRAN", Requirements = c("Rcpp")),
    mitools = list(Package = "mitools", Version = "2.4", Source = "Repository", Repository = "CRAN", Requirements = c("DBI")),
    numDeriv = list(Package = "numDeriv", Version = "2016.8-1.1", Source = "Repository", Repository = "CRAN", Requirements = character()),
    pillar = list(Package = "pillar", Version = "1.11.1", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "glue", "lifecycle", "rlang", "utf8", "vctrs")),
    pkgconfig = list(Package = "pkgconfig", Version = "2.0.3", Source = "Repository", Repository = "CRAN", Requirements = character()),
    prettyunits = list(Package = "prettyunits", Version = "1.2.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    progress = list(Package = "progress", Version = "1.2.3", Source = "Repository", Repository = "CRAN", Requirements = c("R6", "crayon", "hms", "prettyunits")),
    proxy = list(Package = "proxy", Version = "0.4-29", Source = "Repository", Repository = "CRAN", Requirements = character()),
    rappdirs = list(Package = "rappdirs", Version = "0.3.4", Source = "Repository", Repository = "CRAN", Requirements = character()),
    readr = list(Package = "readr", Version = "2.2.0", Source = "Repository", Repository = "CRAN", Requirements = c("R6", "cli", "clipr", "cpp11", "crayon", "glue", "hms", "lifecycle", "rlang", "tibble", "tzdb", "vroom", "withr")),
    readxl = list(Package = "readxl", Version = "1.5.0", Source = "Repository", Repository = "CRAN", Requirements = c("cellranger", "cpp11", "progress", "tibble")),
    rematch = list(Package = "rematch", Version = "2.0.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    renv = list(Package = "renv", Version = "1.2.4", Source = "Repository", Repository = "CRAN", Requirements = character()),
    rlang = list(Package = "rlang", Version = "1.2.0", Source = "Repository", Repository = "CRAN", Requirements = character()),
    rmarkdown = list(Package = "rmarkdown", Version = "2.31", Source = "Repository", Repository = "CRAN", Requirements = c("bslib", "evaluate", "fontawesome", "htmltools", "jquerylib", "jsonlite", "knitr", "tinytex", "xfun", "yaml")),
    s2 = list(Package = "s2", Version = "1.1.9", Source = "Repository", Repository = "CRAN", Requirements = c("Rcpp", "wk")),
    sandwich = list(Package = "sandwich", Version = "3.1-3", Source = "Repository", Repository = "CRAN", Requirements = c("zoo")),
    sass = list(Package = "sass", Version = "0.4.10", Source = "Repository", Repository = "CRAN", Requirements = c("R6", "fs", "htmltools", "rappdirs", "rlang")),
    scales = list(Package = "scales", Version = "1.4.0", Source = "Repository", Repository = "CRAN", Requirements = c("R6", "RColorBrewer", "cli", "farver", "glue", "labeling", "lifecycle", "rlang", "viridisLite")),
    sf = list(Package = "sf", Version = "1.1-1", Source = "Repository", Repository = "CRAN", Requirements = c("DBI", "Rcpp", "classInt", "s2", "units")),
    stringi = list(Package = "stringi", Version = "1.8.7", Source = "Repository", Repository = "CRAN", Requirements = character()),
    survey = list(Package = "survey", Version = "4.5", Source = "Repository", Repository = "CRAN", Requirements = c("Matrix", "Rcpp", "RcppArmadillo", "lattice", "minqa", "mitools", "numDeriv", "survival")),
    survival = list(Package = "survival", Version = "3.8-6", Source = "Repository", Repository = "CRAN", Requirements = c("Matrix")),
    svglite = list(Package = "svglite", Version = "2.2.2", Source = "Repository", Repository = "CRAN", Requirements = c("base64enc", "cli", "cpp11", "lifecycle", "rlang", "systemfonts", "textshaping")),
    systemfonts = list(Package = "systemfonts", Version = "1.3.2", Source = "Repository", Repository = "CRAN", Requirements = c("base64enc", "cpp11", "jsonlite", "lifecycle")),
    textshaping = list(Package = "textshaping", Version = "1.0.5", Source = "Repository", Repository = "CRAN", Requirements = c("cpp11", "lifecycle", "stringi", "systemfonts")),
    tibble = list(Package = "tibble", Version = "3.3.1", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "lifecycle", "magrittr", "pillar", "pkgconfig", "rlang", "vctrs")),
    tidyselect = list(Package = "tidyselect", Version = "1.2.1", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "glue", "lifecycle", "rlang", "vctrs", "withr")),
    tinytex = list(Package = "tinytex", Version = "0.59", Source = "Repository", Repository = "CRAN", Requirements = c("xfun")),
    tzdb = list(Package = "tzdb", Version = "0.5.0", Source = "Repository", Repository = "CRAN", Requirements = c("cpp11")),
    units = list(Package = "units", Version = "1.0-1", Source = "Repository", Repository = "CRAN", Requirements = c("Rcpp")),
    utf8 = list(Package = "utf8", Version = "1.2.6", Source = "Repository", Repository = "CRAN", Requirements = character()),
    vctrs = list(Package = "vctrs", Version = "0.7.3", Source = "Repository", Repository = "CRAN", Requirements = c("cli", "glue", "lifecycle", "rlang")),
    viridisLite = list(Package = "viridisLite", Version = "0.4.3", Source = "Repository", Repository = "CRAN", Requirements = character()),
    vroom = list(Package = "vroom", Version = "1.7.1", Source = "Repository", Repository = "CRAN", Requirements = c("bit64", "cli", "cpp11", "crayon", "glue", "hms", "lifecycle", "progress", "rlang", "tibble", "tidyselect", "tzdb", "vctrs", "withr")),
    withr = list(Package = "withr", Version = "3.0.2", Source = "Repository", Repository = "CRAN", Requirements = character()),
    wk = list(Package = "wk", Version = "0.9.5", Source = "Repository", Repository = "CRAN", Requirements = character()),
    xfun = list(Package = "xfun", Version = "0.57", Source = "Repository", Repository = "CRAN", Requirements = character()),
    yaml = list(Package = "yaml", Version = "2.3.12", Source = "Repository", Repository = "CRAN", Requirements = character()),
    zoo = list(Package = "zoo", Version = "1.8-15", Source = "Repository", Repository = "CRAN", Requirements = c("lattice"))
  )
)


# Interactive R/RStudio uses the native file picker. Explicit input parameters
# bypass it for batch runs. Canceling stops before the analysis starts.
select_input_zip <- function(chooser = base::file.choose) {
  message("Select the confidential SURGO–Manitoba input ZIP in the file picker.")
  selected <- tryCatch(chooser(), error = function(e) {
    stop("No ZIP selected. Source the script again when ready.", call. = FALSE)
  })
  if (length(selected) != 1L || is.na(selected) || !nzchar(selected) || !file.exists(selected)) {
    stop("Select one existing ZIP file.", call. = FALSE)
  }
  if (!grepl("[.]zip$", selected, ignore.case = TRUE)) stop("Select the .zip file itself.", call. = FALSE)
  normalizePath(selected, mustWork = TRUE)
}

# Install recorded versions in a local library; do not alter global libraries.
# The embedded environment includes transitive dependencies, so a separate
# lockfile or another R script is unnecessary for running the analysis.
setup_analysis_packages <- function(packages, locked_environment, library_dir,
                                    install_missing = TRUE, check_versions = TRUE) {
  expected <- vapply(locked_environment$Packages, function(x) x$Version, character(1))
  check_packages <- packages
  repeat {
    dependencies <- unique(unlist(lapply(locked_environment$Packages[check_packages], function(x) x$Requirements)))
    expanded <- unique(c(check_packages, dependencies))
    if (setequal(expanded, check_packages)) break
    check_packages <- expanded
  }
  if (length(setdiff(check_packages, names(expected)))) stop("A required package lacks a recorded version.")
  if (dir.exists(library_dir)) .libPaths(c(library_dir, .libPaths()))
  installed_version <- function(package) {
    description <- suppressWarnings(utils::packageDescription(package))
    if (is.list(description)) description$Version else NA_character_
  }
  matches <- function(package) {
    actual <- installed_version(package)
    isTRUE(!is.na(actual) && (!check_versions || package_version(actual) == package_version(expected[[package]])))
  }
  changes <- check_packages[!vapply(check_packages, matches, logical(1))]
  if (length(changes)) {
    if (!install_missing) stop("Install/restore these packages: ", paste(changes, collapse = ", "), call. = FALSE)
    loaded_changes <- intersect(changes, loadedNamespaces())
    if (length(loaded_changes)) stop("Restart R, then Source this script again to install the recorded versions of: ",
      paste(loaded_changes, collapse = ", "), call. = FALSE)
    dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(library_dir) || file.access(library_dir, 2L) != 0L) {
      stop("Choose a writable library_dir at the top of the script.", call. = FALSE)
    }
    .libPaths(c(library_dir, .libPaths()))
    if (!requireNamespace("renv", quietly = TRUE)) {
      install.packages("renv", lib = library_dir, repos = "https://cloud.r-project.org")
    }
    if (!requireNamespace("renv", quietly = TRUE)) stop("The package installer renv could not be installed.")
    lock_file <- file.path(library_dir, "analysis_environment.lock")
    renv::lockfile_write(locked_environment, file = lock_file)
    message("Installing recorded packages: ", paste(changes, collapse = ", "))
    tryCatch(
      renv::restore(project = library_dir, library = library_dir, lockfile = lock_file,
                    packages = check_packages, prompt = FALSE, clean = FALSE),
      error = function(e) stop("Package installation did not finish. Check internet access, R version and any required system libraries, ",
        "then Source the script again.\n", conditionMessage(e), call. = FALSE)
    )
  }
  if (any(!vapply(check_packages, matches, logical(1)))) stop("Some required packages are unavailable or differ from the recorded versions.")
  for (package in packages) {
    if (package %in% loadedNamespaces() && check_versions &&
        package_version(as.character(getNamespaceVersion(package))) != package_version(expected[[package]])) {
      stop("Restart R to load the recorded version of ", package, ".", call. = FALSE)
    }
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }
  invisible(TRUE)
}



# Locate the complete repo when present. A downloaded single script uses its own folder.
locate_project <- function(script_dir) {
  at <- script_dir
  repeat {
    if (file.exists(file.path(at, "support/report.html")) &&
        file.exists(file.path(at, "run_all.R")) && file.exists(file.path(at, "renv.lock"))) return(at)
    parent <- dirname(at)
    if (identical(parent, at)) return(script_dir)
    at <- parent
  }
}
portable_path <- function(path, root) {
  path <- path.expand(path)
  if (!grepl("^/|^[A-Za-z]:|^\\\\", path)) path <- file.path(root, path)
  normalizePath(path, mustWork = FALSE)
}
select_python <- function(requested = "") {
  candidates <- if (nzchar(requested)) requested else c("python3", "python", "py")
  for (candidate in candidates) {
    command <- unname(Sys.which(candidate))
    if (!nzchar(command) && file.exists(candidate)) command <- candidate
    if (!nzchar(command)) next
    prefix <- if (tolower(tools::file_path_sans_ext(basename(command))) == "py") "-3" else character()
    version <- suppressWarnings(tryCatch(system2(command, shQuote(c(prefix, "-c", "import sys; print(int(sys.version_info >= (3, 9)))")),
      stdout = TRUE, stderr = TRUE, timeout = 30), error = function(e) character()))
    if (length(version) && is.null(attr(version, "status")) && tail(version, 1) == "1") return(list(command = command, prefix = prefix))
  }
  stop("Python 3.9 or later is needed for HTML/maps. Install Python 3.9 or later or supply --python / SURGO_MANITOBA_PYTHON, then run again.", call. = FALSE)
}
run_report_python <- function(root, file, args = character(), python = "") {
  launch <- select_python(python)
  old <- Sys.getenv(c("PYTHONUTF8", "PYTHONIOENCODING"), unset = NA_character_)
  on.exit(for (key in names(old)) {
    if (is.na(old[[key]])) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(old[[key]]), key))
  }, add = TRUE)
  Sys.setenv(PYTHONUTF8 = "1", PYTHONIOENCODING = "utf-8")
  status <- system2(launch$command, shQuote(c(launch$prefix, file.path(root, file), args)))
  if (!identical(status, 0L)) stop("Report rendering/check failed: ", basename(file), call. = FALSE)
  invisible(TRUE)
}
# Public display files only. No private input or survey values are transmitted.
# A complete checkout uses its local files. A single downloaded R script fetches
# the pinned, checksum-verified map/layout assets into its private working output.
prepare_report_resources <- function(project_root, work_dir, resources, resource_url) {
  valid <- function(root) all(vapply(seq_len(nrow(resources)), function(i) {
    file <- file.path(root, resources$file[i])
    file.exists(file) && digest::digest(file = file, algo = "sha256") == resources$sha256[i]
  }, logical(1)))
  if (valid(project_root)) return(project_root)
  cache <- file.path(work_dir, "report_resources")
  for (i in seq_len(nrow(resources))) {
    target <- file.path(cache, resources$file[i])
    if (file.exists(target) && digest::digest(file = target, algo = "sha256") == resources$sha256[i]) next
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    url <- paste0(resource_url, "/", utils::URLencode(resources$file[i], reserved = FALSE))
    tryCatch(utils::download.file(url, target, mode = "wb", quiet = TRUE), error = function(e) {
      stop("Could not obtain a public display resource: ", resources$file[i],
        ". Check internet access or run the script from the complete repository.", call. = FALSE)
    })
    if (digest::digest(file = target, algo = "sha256") != resources$sha256[i]) stop("Public resource checksum mismatch: ", resources$file[i])
  }
  if (!valid(cache)) stop("Display resources did not verify.")
  cache
}
# All rendering uses already-checked results; no statistical estimates are refit.
render_report <- function(country, mode, report_name, title, data_dir, work_dir,
                          output_dir, resource_root, python = "") {
  native <- file.path(work_dir, report_name)
  renderer <- paste0("support/renderers/", mode, "/", country, "_render.py")
  args <- c("--data-dir", data_dir, "--output", native)
  if (country == "lagos") args <- c(args, "--reference-html", file.path(resource_root, "support/assets/lagos_map_template.html"))
  run_report_python(resource_root, renderer, args, python)
  run_report_python(resource_root, "support/renderers/build_threshold_map_report.py",
    c("--countries", country, "--workflow", if (mode == "aggregate") "administrative_area" else "urban_rural",
      "--source-report", native, "--data-file", file.path(data_dir, "report_data.json"),
      "--output", file.path(data_dir, "score_threshold_maps.html"), "--embed-in-source"), python)
  if (mode == "aggregate") run_report_python(resource_root,
    paste0("support/renderers/", if (country == "lagos") "lagos_regression_sensitivity.py" else "render_regression_sensitivity.py"),
    c("--data-dir", file.path(data_dir, "regression_sensitivity"), "--html", native), python)
  verifier <- paste0("support/renderers/", mode, "/", country, "_verify.py")
  run_report_python(resource_root, verifier, c("--data-dir", data_dir, "--html", native), python)
  fragment <- file.path(work_dir, "report_fragment.html")
  run_report_python(resource_root, "support/renderers/html_fragment.py", c("--input", native, "--output", fragment), python)
  template <- file.path(work_dir, "report_render.Rmd")
  writeLines(c("---", paste0("title: ", encodeString(title, quote = '"')), "params:", '  fragment: ""', "---", "",
    '```{r setup, include=FALSE}', 'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)', '```', "",
    "```{r report, results='asis'}", 'cat("```{=html}\\n")',
    'cat(readLines(params$fragment, warn = FALSE, encoding = "UTF-8"), sep = "\\n")', 'cat("\\n```\\n")', '```'), template, useBytes = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  target <- rmarkdown::render(template, output_file = report_name, output_dir = output_dir,
    output_format = rmarkdown::html_document(template = file.path(resource_root, "support/report.html"), theme = NULL,
      mathjax = NULL, self_contained = FALSE, keep_md = FALSE, pandoc_args = "--wrap=none"),
    params = list(fragment = fragment), quiet = TRUE, envir = new.env(parent = globalenv()))
  run_report_python(resource_root, verifier, c("--data-dir", data_dir, "--html", target), python)
  list(native = native, fragment = fragment, data_dir = data_dir, html = target)
}

# Public display snapshot matching the current validated reports.
resource_url <- "https://raw.githubusercontent.com/graciasierra6/Surgo-manitoba/623be86c412e8f35d01ea20a320726db04b4a9aa"
report_resources <- data.frame(
  file = c("support/report.html",
    "support/report.css",
    "support/renderers/html_fragment.py",
    "support/renderers/build_threshold_map_report.py",
    "support/renderers/new_maps.py",
    "support/renderers/regional_sd_maps.py",
    "support/renderers/lagos_threshold_comparison.py",
    "support/renderers/aggregate/lagos_render.py",
    "support/renderers/aggregate/lagos_verify.py",
    "support/assets/lagos_map_template.html",
    "support/renderers/lagos_regression_sensitivity.py"),
  sha256 = c("a1614c7c1467e4adff8653c04cb60a87f8e491ce566f0d38eb690b854f8b6294",
    "2aa4da8b3257aa8ab1bda1dcbe7f3d65b03932b27ce3ea7dbd93f4b6b7ad8448",
    "dfaef69903df000b7e37fd54095f2ab892ec9d90803f54a09b3675f18b745219",
    "d752996b2c61b404660e5c6b3fb7db73edf76f510fbf15ff823be3f64c0e4eaf",
    "3382f02f1a965acb63a44dbbf0c28321923529dfce02efa79a976581bf4de0c4",
    "f5fa87f2e2866e9f2270430dadf68e3307e1c874007140f6b421efe098bacb6d",
    "bfccf55d9023446a521f10a3e30ae5e98e500e71f95b889cf80efebc9952f96a",
    "5c8db1d25a704db2177c08967d2ce7c0e8da754fbb1fe0bf3c401547ca76512f",
    "472e12f6adde7250b363d1ce02d228fb81ce37f1d67f45451babfffcc3288167",
    "4282fe0a4d50e8b00b59c3b1d74c124c0234b44dbfae9da01d3cd8db18bc792a",
    "537235d9389ccc9ec951feba29feeb0d0a8bf64b01ebe8139658f3f6100ddd54"),
  stringsAsFactors = FALSE)

.report_root <- locate_project(script_dir)
output_dir <- file.path("output", "rebuilt", "aggregate", "lagos")
work_dir <- file.path("output", "build", "aggregate", "lagos")
cache_dir <- file.path("data", "cache")
library_dir <- file.path(.report_root, "renv/library", paste0("R-", getRversion()), R.version$platform)

cli <- commandArgs(trailingOnly = TRUE)
cli_value <- function(name, default = "") {
  i <- match(name, cli)
  if (is.na(i)) return(default)
  if (i == length(cli)) stop("Supply a value for ", name)
  cli[i + 1L]
}
if (run_analysis && "--help" %in% cli) {
  cat("Run: Rscript aggregate/lagos/Lagos_analysis_aggregate.R --input-zip FILE\n",
    "Options: --input-dir DIR --output-dir DIR --work-dir DIR --cache-dir DIR --join-dir VERIFIED_JOIN --library-dir DIR --python EXE --pandoc-dir DIR --no-html --no-install-packages\n",
    "RStudio: open this script, click Source, then choose the ZIP.\n", sep = "")
  if (!interactive()) quit(status = 0)
  run_analysis <- FALSE
}
input_zip <- cli_value("--input-zip", input_zip)
input_dir <- cli_value("--input-dir", input_dir)
output_dir <- cli_value("--output-dir", output_dir)
work_dir <- cli_value("--work-dir", work_dir)
cache_dir <- cli_value("--cache-dir", cache_dir)
library_dir <- cli_value("--library-dir", library_dir)
if ("--no-html" %in% cli) render_html <- FALSE
if ("--no-install-packages" %in% cli) install_missing_packages <- FALSE
if (run_analysis && !nzchar(input_zip) && !nzchar(input_dir) && !nzchar(cli_value("--join-dir"))) {
  if (interactive()) input_zip <- select_input_zip() else
    stop("Supply --input-zip FILE, or Source this script in RStudio to select the ZIP.", call. = FALSE)
}



# 2. Input readers, identifiers and geographic assignments --------------------


country_config <- list(lagos = list(loc = "Lagos", facility = "lagos", raw_name = "uom_lagos_facility_pooled.dta",
    woman_file = "inputs/surgo/lagos/surgo_lagos_women.csv", woman_geo = "lga", woman_region = "state",
    join_level = "geo_level2", id = "uid", weight = "weight", counselor = "q702"), senegal = list(loc = "Senegal",
    facility = "senegal", raw_name = "uom_senegal_facility_pooled.dta", woman_file = "inputs/surgo/senegal/surgo_senegal_women.dta",
    woman_geo = "RB0C", woman_region = "RB0B", join_level = "geo_level2", id = "IDW", weight = "WEIGHT",
    counselor = "s7_2"), cotedivoire = list(loc = "CDI", facility = "cdi", raw_name = "uom_cdi_facility_pooled.dta",
    woman_file = "inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta", woman_geo = "RB0C", woman_region = "RB0B",
    join_level = "geo_level3", id = "IDW", weight = "WEIGHT", counselor = "s7_2"), sindh = list(loc = "Sindh",
    facility = "sindh", raw_name = "uom_sindh_facility_pooled.dta", woman_file = "inputs/surgo/sindh/20250124_forms_wt.dta",
    woman_geo = "rb0c", woman_region = "rb0b", join_level = "geo_level2", id = "col_id", weight = "Normalizedweights",
    counselor = "q702"))

read_chars <- function(path) {
    as.data.frame(readr::read_csv(path, col_types = cols(.default = col_character()), na = c("", "NA",
        "NaN"), show_col_types = FALSE, name_repair = "minimal"), check.names = FALSE)
}

write_table <- function(x, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(x, path, na = "")
}

num <- function(x) suppressWarnings(as.numeric(x))

lab <- function(x) {
    if (inherits(x, "haven_labelled"))
        as.character(haven::as_factor(x))
    else as.character(x)
}

norm <- function(x) {
    x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
    x <- gsub("[^A-Z0-9]", "", toupper(trimws(x)))
    x[is.na(x) | x %in% c("", "NA")] <- NA_character_
    x
}

bool <- function(x) {
    s <- tolower(trimws(as.character(x)))
    ifelse(s %in% c("true", "1"), 1, ifelse(s %in% c("false", "0"), 0, NA_real_))
}

mean_sql <- function(x) {
    x <- num(x)
    if (all(is.na(x)))
        NA_real_
    else mean(x, na.rm = TRUE)
}

sum_sql <- function(x) {
    x <- num(x)
    if (all(is.na(x)))
        NA_real_
    else sum(x, na.rm = TRUE)
}

ratio <- function(a, b) if (is.na(b) || b == 0) NA_real_ else a/b

assert_unique <- function(x, cols, what) {
    k <- do.call(paste, c(lapply(x[cols], function(z) ifelse(is.na(z), "<NA>", z)), sep = "\r"))
    if (anyDuplicated(k))
        stop(what, " is not unique on ", paste(cols, collapse = ", "))
    invisible(TRUE)
}

left_join_safe <- function(x, y, by, what) {
    assert_unique(y, unname(by), paste(what, "lookup"))
    n <- nrow(x)
    out <- left_join(x, y, by = by, relationship = "many-to-one", na_matches = "never")
    if (nrow(out) != n)
        stop(what, " changed row count")
    out
}

read_women <- function(path) {
    if (grepl("\\.dta$", path, ignore.case = TRUE))
        haven::read_dta(path)
    else readr::read_csv(path, show_col_types = FALSE, guess_max = 20000, name_repair = "minimal", na = c("",
        "NA"))
}

case_name <- function(names, wanted) {
    exact <- match(wanted, names)
    if (!is.na(exact))
        return(names[exact])
    j <- which(tolower(names) == tolower(wanted))
    if (length(j) != 1)
        stop("Cannot uniquely resolve field ", wanted)
    names[j]
}

source_geo <- function(raw, country) {
    cfg <- country_config[[country]]
    get <- function(v) lab(raw[[case_name(names(raw), v)]])
    if (country == "lagos") {
        out <- data.frame(fid = get("key"), admin1 = get("state_name"), admin2 = get("IBILE_dist"), admin3 = get("district_name"))
    }
    else if (country == "senegal") {
        out <- data.frame(fid = get("fid"), admin1 = get("s1_1"), admin2 = get("s1_3"), admin3 = NA_character_)
    }
    else if (country == "cotedivoire") {
        out <- data.frame(fid = get("fid"), admin1 = get("district"), admin2 = get("s1_1"), admin3 = get("s1_2"))
    }
    else {
        fid <- get("key")
        empty <- is.na(fid) | trimws(fid) == ""
        fid[empty] <- get("facility_id")[empty]
        out <- data.frame(fid = fid, admin1 = "Sindh", admin2 = get("division"), admin3 = get("district_name"))
    }
    out$loc <- cfg$loc
    out$fid <- trimws(out$fid)
    out
}

aggregate_supply <- function(fac, dictionary, geo_col = "geo_join_pipeline") {
    supply_names <- dictionary$column_name[dictionary$topic == "Supply"]
    out <- list()
    category_map <- c(n_flvl_ = "flvl", n_ftyp_ = "ftyp", n_rurb_ = "rurb", n_mpop_ = "mpop_prdct", n_mp_in_stock_ = "mp_in_stock",
        n_mp_stockout_ = "mp_stockout", n_pubpriv_ = "pubpriv", n_ftyp2_ = "ftyp2")
    clean_val <- function(x) {
        x[is.na(x)] <- "NA"
        gsub("^_+|_+$", "", tolower(gsub("[^A-Za-z0-9]+", "_", as.character(x))))
    }
    flag_map <- c(share_offer_1plus_larc = "one_larc_offer", share_offer_3plus_larc = "three_larc_offer",
        share_offer_larc_and_3_sa = "one_larc_3_sa_offer", share_stocked_1plus_larc = "one_larc_stockin",
        share_stocked_3plus_larc = "three_larc_stockin", share_stocked_larc_and_3_sa = "one_larc_3_sa_stockin")
    mean_map <- c(mean_facility_any_stockout = "p_any_stockout", mean_p_free_prod = "p_free_prod", mean_avg_duration = "avg_duration",
        mean_mp_stockout_dur = "mp_stockout_dur", mean_p_train = "p_train", mean_mdn_skill_lvl = "mdn_skill_lvl",
        mean_n_unique_prod = "n_unique_prod", mean_n_srvc_offr = "n_srvc_offr", mean_pp_avail = "pp_avail")
    sum_map <- c(vol_total = "dmnd_vol", vol_larc = "t_larc", vol_inj = "t_inj", vol_short_acting = "t_sa",
        vol_steril = "t_steril", vol_new_client_total = "new_dmnd_vol", vol_new_larc = "t_new_larc",
        vol_new_inj = "t_new_inj", vol_new_short_acting = "t_new_sa", vol_new_steril = "t_new_steril",
        sum_t_in_stock = "t_in_stock", sum_t_out_of_stock = "t_out_of_stock", sum_t_free_prod = "t_free_prod",
        sum_t_not_free_prod = "t_not_free_prod", vol_returning_total = "dmnd_ret_t", vol_returning_inj = "dmnd_ret_i",
        vol_returning_short_acting = "dmnd_ret_s", vol_returning_larc = "dmnd_ret_l")
    countries <- unique(fac$country)
    for (country in countries) {
        fc <- fac[fac$country == country & !is.na(fac[[geo_col]]) & fac[[geo_col]] != "", , drop = FALSE]
        for (g in sort(unique(fc[[geo_col]]))) {
            z <- fc[fc[[geo_col]] == g, , drop = FALSE]
            r <- list(country = country, join_level = unique(z$join_level), geo_join = g, n_facilities = nrow(z))
            for (v in names(flag_map)) {
                r[[v]] <- round(mean_sql(bool(z[[flag_map[[v]]]])), 5)
            }
            for (v in names(mean_map)) r[[v]] <- mean_sql(z[[mean_map[[v]]]])
            for (v in names(sum_map)) r[[v]] <- sum_sql(z[[sum_map[[v]]]])
            r$stockout_rate_pooled <- ratio(sum_sql(z$t_out_of_stock), sum_sql(num(z$t_in_stock) + num(z$t_out_of_stock)))
            r$share_products_free <- ratio(sum_sql(z$t_free_prod), sum_sql(num(z$t_free_prod) + num(z$t_not_free_prod)))
            h <- !is.na(num(z$n_tot)) & !is.na(num(z$n_train))
            r$n_hcp_total <- sum_sql(z$n_tot[h])
            r$n_hcp_trained <- sum_sql(z$n_train[h])
            r$share_providers_trained <- ratio(r$n_hcp_trained, r$n_hcp_total)
            r$n_facilities_reporting_volume <- sum(!is.na(num(z$dmnd_vol)))
            for (v in c("p_free_prod", "mp_stockout_dur", "p_train", "mdn_skill_lvl")) {
                r[[paste0("n_reporting_", v)]] <- sum(!is.na(num(z[[v]])))
            }
            for (prefix in names(category_map)) {
                source <- category_map[[prefix]]
                source_values <- clean_val(z[[source]])
                wanted <- supply_names[startsWith(supply_names, prefix)]
                for (v in wanted) r[[v]] <- sum(source_values == substring(v, nchar(prefix) + 1))
            }
            for (k in 3:6) {
                cluster <- num(z[[paste0("cluster_k", k)]])
                for (j in seq_len(k)) r[[paste0("n_cluster_k", k, "_", j)]] <- sum(cluster == j, na.rm = TRUE)
            }
            for (k in 3:6) for (j in seq_len(k)) {
                r[[paste0("share_cluster_k", k, "_", j)]] <- r[[paste0("n_cluster_k", k, "_", j)]]/nrow(z)
            }
            valid <- !is.na(z$counselor_available)
            r$n_fp_counselor_yes <- sum(z$counselor_available == 1, na.rm = TRUE)
            r$n_fp_counselor_no <- sum(z$counselor_available == 0, na.rm = TRUE)
            r$n_fp_counselor_valid <- sum(valid)
            r$n_fp_counselor_missing <- sum(!valid)
            r$share_fp_counselor <- if (any(valid))
                mean(z$counselor_available[valid])
            else NA_real_
            r$coverage_fp_counselor <- mean(valid)
            review <- if (geo_col == "geo_join_pipeline")
                z$crosswalk_status %in% "needs_review"
            else z$geo_review_required %in% TRUE
            r$link_n_geo_review <- sum(review)
            r$link_share_geo_review <- mean(review)
            r$link_n_geo_changed <- sum(z$geo_changed %in% TRUE)
            r$link_geo_mode <- if (geo_col == "geo_join_pipeline")
                "pipeline_crosswalk"
            else "audited_geography"
            rollup <- if (geo_col == "geo_join_pipeline")
                z$geo_rollup_pipeline
            else z$geo_rollup
            r$link_n_rollup <- sum(rollup %in% TRUE)
            r$link_rollup_dominated <- r$link_n_rollup > nrow(z)/2
            missing <- setdiff(supply_names, names(r))
            if (length(missing))
                stop("Unimplemented upstream supply columns: ", paste(missing, collapse = ", "))
            out[[length(out) + 1]] <- as.data.frame(r, stringsAsFactors = FALSE)
        }
    }
    ans <- bind_rows(out)
    for (c in unique(ans$country)) for (v in c("share_offer_3plus_larc", "share_stocked_3plus_larc")) {
        z <- ans$country == c
        if (all(is.na(ans[[v]][z]) | ans[[v]][z] == 0))
            ans[[v]][z] <- NA_real_
    }
    assert_unique(ans, c("country", "join_level", "geo_join"), "Aggregated supply")
    for (k in 3:6) {
        cs <- paste0("n_cluster_k", k, "_", seq_len(k))
        ss <- paste0("share_cluster_k", k, "_", seq_len(k))
        stopifnot(all(rowSums(ans[cs]) == ans$n_facilities), all(abs(rowSums(ans[ss]) - 1) < 1e-12))
    }
    ans
}

check_upstream_parity <- function(aggregated, reference, supply_names) {
    rows <- list()
    for (i in seq_len(nrow(reference))) {
        rr <- reference[i, , drop = FALSE]
        z <- aggregated[aggregated$country == rr$country & aggregated$geo_join == rr$geo_join, , drop = FALSE]
        for (v in c("n_facilities", supply_names)) {
            actual <- if (nrow(z))
                num(z[[v]][1])
            else if (v == "n_facilities")
                0
            else NA_real_
            expected <- num(rr[[v]])
            same <- (is.na(actual) && is.na(expected)) || (!is.na(actual) && !is.na(expected) && abs(actual -
                expected) <= 1e-09 * max(1, abs(expected)))
            rows[[length(rows) + 1]] <- data.frame(country = rr$country, geo_join = rr$geo_join, variable = v,
                expected = expected, reproduced = actual, passed = same)
        }
    }
    bind_rows(rows)
}

attach_geography_audit <- function(fac, raw, country, women, crosswalk, source_geography, project_root,
    input_dir) {
    cfg <- country_config[[country]]
    g <- source_geo(raw, country)
    assert_unique(g, c("fid", "loc"), paste(country, "raw facility geography"))
    exported <- source_geography[source_geography$loc == cfg$loc, , drop = FALSE]
    ii <- match(g$fid, exported$fid)
    if (anyNA(ii))
        stop(country, ": raw facility IDs missing from exported geography")
    for (v in c("admin1", "admin2", "admin3")) {
        a <- g[[v]]
        b <- exported[[v]][ii]
        a[is.na(a) | a == ""] <- "<NA>"
        b[is.na(b) | b == ""] <- "<NA>"
        if (!identical(a, b))
            stop(country, ": raw/exported ", v, " differs")
    }
    fac <- left_join_safe(fac, g[, c("fid", "loc", "admin1", "admin2", "admin3")], by = c("fid", "loc"),
        what = paste(country, "raw geography"))
    cw <- crosswalk[crosswalk$loc == cfg$loc, , drop = FALSE]
    composite <- function(df, prefix = "") {
        apply(df[paste0(prefix, c("admin1", "admin2", "admin3"))], 1, function(v) paste(ifelse(is.na(v) |
            v == "", "<NA>", v), collapse = "\r"))
    }
    kk <- composite(cw, "uom_")
    if (anyDuplicated(kk))
        stop(country, ": duplicate full crosswalk keys")
    ci <- match(composite(fac), kk)
    if (anyNA(ci))
        stop(country, ": facility geography absent from reviewed crosswalk")
    fac$crosswalk_method <- cw$match_method[ci]
    fac$crosswalk_status <- cw$status[ci]
    fac$crosswalk_note <- cw$notes[ci]
    candidate <- if (cfg$join_level == "geo_level3")
        cw$surgo_geo_level3[ci]
    else cw$surgo_geo_level2[ci]
    unmatched <- is.na(candidate) | candidate == "" | cw$match_method[ci] == "none" | cw$status[ci] ==
        "unmappable"
    fac$geo_join_pipeline <- toupper(trimws(candidate))
    fac$geo_join_pipeline[unmatched] <- NA_character_
    fac$country <- country
    fac$join_level <- cfg$join_level
    fac$geo_rollup <- cw$match_method[ci] %in% "parent-rollup" | (!is.na(cw$rollup_basis[ci]) & startsWith(cw$rollup_basis[ci],
        "parent-rollup"))
    fac$geo_rollup_pipeline <- fac$geo_rollup
    targets <- sort(unique(toupper(trimws(lab(women[[case_name(names(women), cfg$woman_geo)]])))))
    targets <- targets[!is.na(targets) & targets != ""]
    nk <- norm(targets)
    if (anyDuplicated(nk))
        stop(country, ": ambiguous normalized SURGO geography labels")
    canonical <- function(x) targets[match(norm(x), nk)]
    fac$geo_join_audited <- fac$geo_join_pipeline
    fac$assigned_join_area <- if (country == "senegal")
        NA_character_
    else fac$admin3
    fac$geo_assignment_basis <- "Reviewed upstream geographic crosswalk"
    fac$geo_evidence <- "reference/upstream/workflow_merge/m04_apply_crosswalk_review.sql and exported reviewed crosswalk (inside input bundle)"
    fac$geo_review_required <- !unmatched & fac$crosswalk_status %in% "needs_review"
    fac$source_geo_field <- if (country == "senegal")
        "s1_3"
    else if (country == "lagos")
        "district_name"
    else if (country == "sindh")
        "district_name"
    else "s1_2"
    fac$source_geo_name <- if (country == "senegal")
        fac$admin2
    else fac$admin3
    fac$corrected_source_geo_name <- fac$source_geo_name
    if (country == "senegal") {
        evidence <- read_chars(file.path(input_dir, "lookups/senegal_health_district_department_evidence.csv"))
        ei <- match(norm(fac$admin2), norm(evidence[["Health district"]]))
        if (anyNA(ei))
            stop("Senegal: district absent from documented department lookup")
        dept <- evidence[["Department-checked"]][ei]
        dept <- trimws(sub(" —.*$", "", dept))
        dept <- trimws(sub(" \\(.*$", "", dept))
        fac$assigned_join_area <- dept
        fac$geo_join_audited <- canonical(dept)
        fac$geo_assignment_basis <- "Health district to administrative department from documented official-source review"
        fac$geo_evidence <- evidence[["Source of data checks*"]][ei]
        fac$geo_review_required <- norm(fac$admin2) %in% norm(c("diakhao", "sangalkam", "yeumbeul"))
        fac$geo_rollup <- FALSE
    }
    if (country == "sindh") {
        district_field <- case_name(names(raw), "district")
        raw_district <- lab(raw[[district_field]])
        ri <- match(fac$fid, g$fid)
        district <- raw_district[ri]
        aliases <- read_chars(file.path(input_dir, "lookups/sindh_district_aliases.csv"))
        key <- norm(district)
        ai <- match(key, aliases$source_normalized)
        key[!is.na(ai)] <- aliases$target_normalized[ai[!is.na(ai)]]
        fac$geo_join_audited <- targets[match(key, nk)]
        if (anyNA(fac$geo_join_audited))
            stop("Sindh: raw named district could not be mapped to a SURGO district")
        fac$assigned_join_area <- fac$geo_join_audited
        fac$corrected_source_geo_name <- district
        fac$source_geo_field <- "district (Name of District), preferred to incomplete district_name"
        fac$geo_assignment_basis <- "Explicit named district in original facility survey; explicit aliases to SURGO names"
        fac$geo_evidence <- "Raw district value labels; lookups/sindh_district_aliases.csv; government district naming reference in lookups/geographic_sources.json"
        fac$geo_review_required <- FALSE
        fac$geo_rollup <- FALSE
        same_existing <- is.na(fac$geo_join_pipeline) | norm(fac$geo_join_pipeline) == norm(fac$geo_join_audited)
        if (!all(same_existing))
            stop("Sindh: named district conflicts with an existing matched assignment")
    }
    if (country == "cotedivoire") {
        codebook <- read_chars(file.path(input_dir, "lookups/cdi_health_district_codebook.csv"))
        raw_code <- num(raw[[case_name(names(raw), "s1_2")]])
        codes <- raw_code[match(fac$fid, g$fid)]
        restored <- codebook$label[match(codes, num(codebook$code))]
        if (anyNA(restored))
            stop("CDI: code absent from companion health-district codebook")
        known <- codes >= 14
        if (!all(norm(restored[known]) == norm(fac$admin3[known])))
            stop("CDI companion district-code correspondence failed")
        fac$corrected_source_geo_name <- restored
        fac$assigned_join_area <- restored
        new_target <- canonical(restored)
        fac$geo_join_audited <- ifelse(!is.na(new_target), new_target, fac$geo_join_pipeline)
        abidjan <- codes %in% 1:10
        fac$geo_join_audited[abidjan] <- canonical("ABIDJAN")
        fac$assigned_join_area[abidjan] <- "ABIDJAN"
        subdistrict <- c(KORHOGO1 = "KORHOGO", KORHOGO2 = "KORHOGO", GAGNOA1 = "GAGNOA", GAGNOA2 = "GAGNOA",
            BOUAKENORDEST = "BOUAKE", BOUAKENORDOUEST = "BOUAKE", BOUAKESUD = "BOUAKE", TOULEUPLEU = "TOULEPLEU")
        si <- match(norm(restored), names(subdistrict))
        fac$assigned_join_area[!is.na(si)] <- unname(subdistrict[si[!is.na(si)]])
        fac$geo_join_audited[!is.na(si)] <- canonical(fac$assigned_join_area[!is.na(si)])
        fac$assigned_join_area[!is.na(fac$geo_join_audited)] <- fac$geo_join_audited[!is.na(fac$geo_join_audited)]
        fac$geo_assignment_basis[codes <= 13] <- "Restore mislabeled health-district codes 1-13 using companion codebook, checked against ministry district names"
        fac$geo_evidence[codes <= 13] <- "lookups/cdi_health_district_codebook.csv; ministry RASS district list in lookups/geographic_sources.json; codes 14-113 agree between raw and companion labels"
        fac$geo_review_required <- FALSE
        fac$geo_rollup <- abidjan
    }
    if (country == "lagos") {
        fac$assigned_join_area <- fac$admin3
        fac$geo_review_required <- FALSE
        fac$geo_rollup <- FALSE
    }
    fac$geo_changed <- (is.na(fac$geo_join_pipeline) != is.na(fac$geo_join_audited)) | (!is.na(fac$geo_join_pipeline) &
        !is.na(fac$geo_join_audited) & norm(fac$geo_join_pipeline) != norm(fac$geo_join_audited))
    fac$geo_changed[is.na(fac$geo_changed)] <- FALSE
    fac
}

rurb_value <- function(x) {
    value <- toupper(trimws(lab(x)))
    value <- gsub("[[:space:]]+", " ", value)
    ifelse(value %in% c("RURAL", "1"), "Rural", ifelse(value %in% c("URBAN", "URBAIN", "2"), "Urban",
        NA_character_))
}

rurb_key <- function(area, residence) {
    a <- norm(area)
    r <- rurb_value(residence)
    ifelse(is.na(a) | is.na(r), NA_character_, paste(a, r, sep = "|"))
}

rurb_match <- function(area, residence, profiles) {
    key <- rurb_key(profiles$geo_join, profiles$rurb_join)
    if (anyNA(key) || anyDuplicated(key))
        stop("Invalid or duplicate area/residence profiles")
    match(rurb_key(area, residence), key)
}

rurb_same <- function(x, y) {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

rurb_issue <- function(area, residence, matched, n_admin, n_rural, n_urban, woman_available = TRUE) {
    r <- rurb_value(residence)
    issue <- rep("no_area_type_profile", length(area))
    issue[!matched & n_admin == 0] <- "no_area_facility_records"
    issue[!matched & n_admin > 0 & n_rural + n_urban == 0] <- "facility_type_unavailable"
    issue[!matched & n_admin > 0 & n_rural == 0 & n_urban > 0 & !is.na(r) & r == "Rural"] <- "no_rural_classified_records"
    issue[!matched & n_admin > 0 & n_urban == 0 & n_rural > 0 & !is.na(r) & r == "Urban"] <- "no_urban_classified_records"
    issue[matched] <- "matched"
    issue[is.na(r)] <- if (woman_available)
        "missing_woman_residence"
    else "woman_residence_unavailable"
    issue[is.na(norm(area))] <- "missing_woman_geography"
    issue
}

verify_input_bundle <- function(input_zip, cache_dir) {
  stopifnot(file.exists(input_zip))
  members <- utils::unzip(input_zip,list=TRUE)$Name
  if(any(grepl("^/|^[A-Za-z]:|\\\\",members)) ||
     any(vapply(strsplit(members,"/",fixed=TRUE),function(x)any(x==".."),logical(1))))stop("Unsafe ZIP member")
  if(!"bundle_manifest.json" %in% members)stop("Input ZIP lacks bundle_manifest.json")
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  utils::unzip(input_zip,files="bundle_manifest.json",exdir=cache_dir)
  manifest<-jsonlite::read_json(file.path(cache_dir,"bundle_manifest.json"),simplifyVector=TRUE)
  for(i in seq_len(nrow(manifest$files))) {
    f<-manifest$files[i,];p<-file.path(cache_dir,f$member)
    valid<-file.exists(p) && file.info(p)$size==f$bytes && digest::digest(file=p,algo="sha256")==f$sha256
    if(!f$member %in% members)stop("Missing input ZIP member: ",f$member)
    if(!valid)utils::unzip(input_zip,files=f$member,exdir=cache_dir)
    stopifnot(file.exists(p),file.info(p)$size==f$bytes,digest::digest(file=p,algo="sha256")==f$sha256)
  }
  manifest
}

# A ZIP can be used directly, or the caller can supply its extracted directory.
# In directory mode, only this report's documented source files are required.
resolve_inputs <- function(input_zip = "", input_dir = "", output_dir) {
  if (nzchar(input_zip) && nzchar(input_dir)) {
    stop("Supply input_zip OR input_dir, not both.")
  }
  if (!nzchar(input_zip) && !nzchar(input_dir) && interactive()) {
    input_zip <- file.choose()
  }
  if (nzchar(input_zip)) {
    if (!file.exists(input_zip)) stop("Input ZIP does not exist.")
    members <- utils::unzip(input_zip, list = TRUE)$Name
    unsafe <- any(grepl("^/|^[A-Za-z]:|\\\\", members)) ||
      any(vapply(strsplit(members, "/", fixed = TRUE), function(x) any(x == ".."), logical(1)))
    if (unsafe || anyDuplicated(members)) stop("Unsafe or duplicate ZIP member.")
    if (!"bundle_manifest.json" %in% members) stop("Input ZIP lacks bundle_manifest.json.")
    input_dir <- file.path(output_dir, "private_inputs")
    dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(input_zip, files = "bundle_manifest.json", exdir = input_dir)
    manifest <- jsonlite::read_json(file.path(input_dir, "bundle_manifest.json"), simplifyVector = TRUE)
    for (i in seq_len(nrow(manifest$files))) {
      entry <- manifest$files[i, ]
      if (!entry$member %in% members) stop("Missing ZIP member: ", entry$member)
      path <- file.path(input_dir, entry$member)
      valid <- file.exists(path) && file.info(path)$size == entry$bytes &&
        digest::digest(file = path, algo = "sha256") == entry$sha256
      if (!valid) utils::unzip(input_zip, files = entry$member, exdir = input_dir)
      stopifnot(file.exists(path), file.info(path)$size == entry$bytes,
                digest::digest(file = path, algo = "sha256") == entry$sha256)
    }
  } else if (!nzchar(input_dir) || !dir.exists(input_dir)) {
    stop("Supply the input ZIP or extracted input directory using the parameters at the top.")
  }
  normalizePath(input_dir, mustWork = TRUE)
}

# Stage 1: read the original surveys and facility summary tables, attach the
# checked administrative geography, and retain every original woman and column.
# This intermediate is used to validate the linkage. Stage 2 below reaggregates
# the final supply variables among confirmed FP providers only.
read_and_link_country <- function(country, input_dir, output_dir) {
  cfg <- country_config[[country]]
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  data_file <- function(member) {
    path <- file.path(input_dir, member)
    if (!file.exists(path)) stop("Missing documented input: ", member)
    path
  }
  women <- read_women(data_file(cfg$woman_file))
  raw_path <- data_file(file.path("inputs/uom", cfg$facility, cfg$raw_name))
  summary <- read_chars(data_file("uom_pipeline/uom_summary_facility.csv"))
  archetypes <- read_chars(data_file("uom_pipeline/Facilities data with archetype assignments.csv"))
  volumes <- read_chars(data_file("uom_pipeline/new_demand_vol_facility.csv"))
  crosswalk <- read_chars(data_file("exports/uom_surgo_geo_crosswalk_reviewed.csv"))
  source_geography <- read_chars(data_file("merge_pipeline/uom_facility_geo.csv"))
  crosswalked_reference <- read_chars(data_file("merge_pipeline/uom_summary_facility_crosswalked.csv"))
  upstream_reference <- read_chars(data_file("exports/merged_supply_demand_department.csv"))
  dictionary <- read_chars(data_file("exports/merged_supply_demand_dictionary.csv"))
  dictionary <- dictionary[dictionary$table_name == "merged_supply_demand_department", ]
  for (table in list(summary, archetypes, volumes)) {
    assert_unique(table, c("fid", "loc"), "Input facility table")
  }

  # Read only the original geographic and counselor fields for initial linkage.
  # The FP-provision and frequency questions are read explicitly in Stage 2.
  header <- haven::read_dta(raw_path, n_max = 0)
  desired <- unique(c("key", "facility_id", "fid", "state_name", "IBILE_dist", "district_name",
                      "district_code", "s1_1", "s1_2", "s1_3", "district", "division", cfg$counselor))
  fields <- unique(unlist(lapply(desired, function(k) {
    if (k %in% names(header)) k else names(header)[tolower(names(header)) == tolower(k)][1]
  })))
  fields <- fields[!is.na(fields)]
  raw <- haven::read_dta(raw_path, col_select = tidyselect::all_of(fields))
  raw_geo <- source_geo(raw, country)
  counselor_code <- num(raw[[case_name(names(raw), cfg$counselor)]])
  no_codes <- if (country == "senegal") c(0, 2) else 2
  counselor <- ifelse(counselor_code %in% 1, 1,
                      ifelse(counselor_code %in% no_codes, 0, NA_real_))
  detail <- data.frame(fid = raw_geo$fid, loc = cfg$loc,
                       counselor_available = counselor, counselor_raw_code = counselor_code)
  assert_unique(detail, c("fid", "loc"), "Original facility IDs")
  facilities <- summary[summary$loc == cfg$loc, , drop = FALSE]
  facilities <- left_join_safe(facilities, archetypes[, c("fid", "loc", "pp_avail", "ftyp2", "pubpriv",
    "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6", "dmnd_ret_t", "dmnd_ret_i", "dmnd_ret_s", "dmnd_ret_l")],
    c("fid", "loc"), "Facility archetypes")
  facilities <- left_join_safe(facilities, volumes[, c("fid", "loc", "new_dmnd_vol", "t_new_larc", "t_new_inj", "t_new_sa", "t_new_steril")],
    c("fid", "loc"), "Facility volumes")
  facilities <- left_join_safe(facilities, detail, c("fid", "loc"), "Counselor responses")
  facilities <- attach_geography_audit(facilities, raw, country, women, crosswalk,
                                        source_geography, project_root = NULL, input_dir = input_dir)

  # Check the upstream assignment separately from the checked assignment used here.
  ref <- crosswalked_reference[crosswalked_reference$loc == cfg$loc, ]
  i <- match(facilities$fid, ref$fid)
  expected <- toupper(trimws(ref$surgo_value[i]))
  expected[ref$match_type[i] == "no_match" | is.na(expected)] <- NA_character_
  same <- (is.na(expected) & is.na(facilities$geo_join_pipeline)) |
    (!is.na(expected) & !is.na(facilities$geo_join_pipeline) & expected == facilities$geo_join_pipeline)
  stopifnot(all(same), nrow(facilities) == nrow(raw), setequal(facilities$fid, raw_geo$fid))
  pipeline <- aggregate_supply(facilities, dictionary, "geo_join_pipeline")
  parity <- check_upstream_parity(pipeline, upstream_reference[upstream_reference$country == country, ],
                                  dictionary$column_name[dictionary$topic == "Supply"])
  stopifnot(all(parity$passed))
  write.csv(parity, file.path(output_dir, "upstream_supply_parity.csv"), row.names = FALSE, na = "")

  # Audited geography may recover facilities absent from the supplied crosswalk.
  profiles <- aggregate_supply(facilities, dictionary, "geo_join_audited")
  area <- toupper(trimws(lab(women[[case_name(names(women), cfg$woman_geo)]])))
  stopifnot(!anyDuplicated(norm(profiles$geo_join)))
  lookup <- match(norm(area), norm(profiles$geo_join))
  added <- profiles[lookup, setdiff(names(profiles), c("country", "join_level", "geo_join")), drop = FALSE]
  rownames(added) <- NULL
  if (length(intersect(tolower(names(women)), tolower(names(added))))) {
    stop("Supply columns would overwrite original SURGO fields.")
  }
  joined <- dplyr::bind_cols(women, added)
  joined$link_source_row <- seq_len(nrow(women))
  joined$link_country <- country
  joined$link_geo_level <- cfg$join_level
  joined$link_geo_join <- area
  joined$link_has_supply <- !is.na(lookup)
  joined$link_geo_mode <- "audited_geography"
  joined$link_status <- ifelse(is.na(area) | area == "", "missing_woman_geography",
                               ifelse(!is.na(lookup), "matched", "no_matched_facility_profile"))
  joined$n_facilities[is.na(joined$n_facilities)] <- 0
  weight <- num(women[[case_name(names(women), cfg$weight)]])
  joined$link_valid_weight <- is.finite(weight) & weight > 0
  stopifnot(nrow(joined) == nrow(women),
            all(vapply(names(women), function(k) identical(joined[[k]], women[[k]]), logical(1))))
  facility_path <- file.path(output_dir, "facility_linkage_inputs.rds")
  woman_path <- file.path(output_dir, paste0(country, "_women_manitoba.rds"))
  saveRDS(facilities, facility_path)
  saveRDS(joined, woman_path, compress = "gzip")
  geography_fields <- c("fid", "admin1", "admin2", "admin3", "assigned_join_area",
                        "geo_join_pipeline", "geo_join_audited", "geo_changed", "geo_assignment_basis", "geo_evidence")
  write.csv(facilities[geography_fields], file.path(output_dir, "facility_geography_audit.csv"), row.names = FALSE, na = "")
  list(woman_path = woman_path, facility_path = facility_path, raw_women_path = data_file(cfg$woman_file),
       raw_facility_path = raw_path, dictionary_path = data_file("exports/merged_supply_demand_dictionary.csv"),
       cleaned_facility_path = data_file("uom_pipeline/uom_summary_facility.csv"))
}



# 3. Country recodes and facility measures ------------------------------------


lagos_derive_women <- function(x) {
    nv <- function(k) suppressWarnings(as.numeric(x[[k]]))
    key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
    flag <- function(k, pos, valid) {
        v <- nv(k)
        ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_)
    }
    any_concern <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(m)) >
        0, NA_real_, 0))
    d <- data.frame(row = seq_len(nrow(x)), geo = key(x$link_geo_join), weight = nv("weight"), need = nv("fpn2") %in%
        1, lga = as.character(x$link_geo_join))
    d$agency <- flag("hs7c", 1, 1:2)
    d$stigma <- any_concern(cbind(flag("pb7b", 1:2, 1:4), flag("pb7c", 1:2, 1:4)))
    d$health <- flag("pb2a", 1:2, 1:4)
    mp <- sapply(paste0("mp4_", letters[1:9]), nv)
    selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
    none <- mp[, 9] %in% 1
    coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & nv("mp4_99") %in% 0 & (selected | none) &
        !(selected & none)
    d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) >
        0), NA_real_)
    d$confidence <- any_concern(cbind(flag("mc5a", 3:4, 1:4), flag("mc5b", 3:4, 1:4)))
    d$opposes <- flag("fp1", 3:4, 1:4)
    prompts <- sapply(paste0("cu5", letters[1:4]), nv)
    self_use <- nv("cu1") %in% c(1, 3)
    current <- ifelse(self_use | rowSums(prompts == 1, na.rm = TRUE) > 0, 1, ifelse(nv("cu1") %in% 2 &
        rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
    d$user <- current %in% 1
    modern <- rowSums(sapply(paste0("cu4_", 1:8), function(k) nv(k) %in% 1)) > 0 | nv("cu5c") %in% 1 |
        nv("cu5d") %in% 1 | (nv("cu4_11") %in% 1 & nv("cu4_lam") %in% 1)
    traditional <- rowSums(sapply(paste0("cu4_", c(9, 10, 12:15)), function(k) nv(k) %in% 1)) > 0 | nv("cu5a") %in%
        1 | nv("cu5b") %in% 1
    d$modern <- d$user & modern
    d$traditional <- d$user & !modern & traditional
    d$intender <- current %in% 0 & nv("itu1") %in% 1:2
    d$no_intent <- current %in% 0 & nv("itu1") %in% c(3, 4, 99)
    d$unresolved <- !(d$user | d$intender | d$no_intent)
    d$wanted_start <- d$user & nv("cub8") %in% 1
    d$other_start <- d$user & nv("cub8") %in% 2
    modern_reference <- tolower(as.character(x$cu12)) %in% c("male condom", "injectables", "daily pills",
        "iud", "implant", "female sterilization", "male sterilization", "emergency pill", "lactational amenorrhea")
    d$modern_other_start <- d$modern & modern_reference & nv("cub8") %in% 2
    modern_target <- rowSums(sapply(paste0("its2_", 1:8), function(k) nv(k) %in% 1)) > 0
    d$traditional_modern_switch <- d$traditional & nv("itc2") %in% 1:2 & modern_target
    d$y_modern <- ifelse(current %in% 0, 0, ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
    d$residence <- as.character(x$link_rurb)
    d$age10 <- (nv("current_age") - 30)/10
    d$age10_sq <- d$age10^2
    edu <- ifelse(nv("rb3") %in% c(1, 2, 7), "Primary or less", ifelse(nv("rb3") %in% 3:4, "Secondary",
        ifelse(nv("rb3") %in% 5:6, "Post-secondary", NA_character_)))
    d$education <- factor(edu, levels = c("Primary or less", "Secondary", "Post-secondary"))
    d$partnered <- ifelse(nv("r1") %in% 1:4, as.numeric(nv("r1") %in% 1:3), NA_real_)
    goal <- rep("Uncertain or unstated", nrow(x))
    goal[nv("sf2") %in% 4] <- "After marriage"
    goal[nv("sf2") %in% 3] <- "After 3 years"
    goal[nv("sf2") %in% 2] <- "In 1-3 years"
    goal[nv("sf1a") %in% 2 | nv("sf1b") %in% 2] <- "No more children"
    d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children",
        "After marriage", "Uncertain or unstated"))
    d$urban <- ifelse(d$residence %in% c("Urban", "Rural"), as.numeric(d$residence == "Urban"), NA_real_)
    d
}


lagos_domains <- function() list(D = c("agency", "stigma", "health", "fertility", "confidence", "opposes"),
    DL = c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs",
        "Future-fertility concerns", "Low method knowledge", "Opposes family planning"), S = c("offering",
        "free", "training", "daily_provision", "larc_stock", "stockout_burden"), SL = c("Short + long-acting offering",
        "Free-product provision", "Trained-provider share", "Daily provision of offered modern methods",
        "At least one LARC in stock", "Stockout burden"))

lagos_favorable_supply <- function(x) {
    x <- as.data.frame(x)
    x$stockout_burden <- 1 - x$stockout_burden
    x
}

lagos_frequency_fields <- function() paste0("q507a", c("a", "b", "d", "e", "f", "g", "h", "i", "j", "o"))

lagos_frequency <- function(raw) {
    q <- as.data.frame(lapply(raw[lagos_frequency_fields()], as.numeric))
    collapse <- function(fields) apply(as.matrix(q[fields]), 1, function(v) {
        offered <- v[v %in% 1:4]
        if (length(offered))
            min(offered)
        else if (all(v %in% 5))
            5
        else NA_real_
    })
    m <- cbind(pills = q$q507ad, injectables = collapse(c("q507ag", "q507ah", "q507ai")), male_condoms = q$q507ae,
        female_condoms = q$q507af, ecp = q$q507ao, iud = collapse(c("q507aa", "q507ab")), implants = q$q507aj)
    offered <- rowSums(m >= 1 & m <= 4, na.rm = TRUE)
    daily <- rowSums(m == 1, na.rm = TRUE)
    data.frame(daily_provision = ifelse(offered > 0, daily/offered, NA_real_), at_least_weekly_provision = ifelse(offered >
        0, rowSums(m >= 1 & m <= 2, na.rm = TRUE)/offered, NA_real_), frequency_daily_categories = daily,
        frequency_offered_categories = offered, frequency_valid_categories = rowSums(m >= 1 & m <= 5,
            na.rm = TRUE), frequency_unknown_categories = 7L - rowSums(m >= 1 & m <= 5, na.rm = TRUE),
        frequency_unknown_source_items = rowSums(vapply(q, function(v) !v %in% 1:5, logical(nrow(raw)))))
}



# 4. Survey estimation, inference, scores and plots ---------------------------


survey_weighted_mean <- function(x, w, na.rm = FALSE, ...) {
  x <- as.numeric(x); w <- as.numeric(w)
  stopifnot(length(x) == length(w))
  if (!length(x) || anyNA(w)) return(NA_real_)
  if (any(!is.finite(w)) || any(w < 0)) stop("Weights must be finite and nonnegative")
  keep <- w > 0
  if (!na.rm && anyNA(x[keep])) return(NA_real_)
  keep <- keep & !is.na(x)
  if (!any(keep)) return(NA_real_)
  a <- data.frame(value = x[keep], weight = w[keep])
  # One valid respondent determines the point mean exactly; a sampling variance
  # is not estimable. This helper returns no interval or variance in any case.
  if (nrow(a) == 1L) return(a$value[1])
  des <- survey::svydesign(ids = ~1, strata = NULL, weights = ~weight, data = a)
  as.numeric(coef(survey::svymean(~value, des, na.rm = na.rm)))
}

surgo_design <- function(data) {
  stopifnot(all(is.finite(data$weight)), all(data$weight > 0))
  survey::svydesign(ids = ~1, strata = NULL, weights = ~weight,
                    fpc = NULL, data = data)
}

fit_survey_logistic <- function(formula, data, design = NULL) {
  if (is.null(design)) design <- surgo_design(data)
  warnings <- character()
  fit <- withCallingHandlers(
    survey::svyglm(formula, design = design,
      family = quasibinomial(link = "logit"), rescale = TRUE,
      control = glm.control(maxit = 100)),
    warning = function(e) {
      warnings <<- c(warnings, conditionMessage(e))
      invokeRestart("muffleWarning")
    })
  stopifnot(fit$converged, fit$rank == length(coef(fit)),
            all(is.finite(coef(fit))))
  attr(fit, "fit_warnings") <- unique(warnings)
  fit
}

geographic_cr2 <- function(fit, cluster) {
  # CR2 is applied to the weighted logistic estimating equations. A native
  # svyglm object's existing survey sandwich must not be used as the GLM bread.
  # Keep the fitted object and its class unchanged; request the documented CR2
  # meat, then apply the inverse weighted information matrix explicitly.
  stopifnot(length(cluster) == nrow(model.matrix(fit)), !anyNA(cluster))
  if (!inherits(fit, "svyglm")) {
    return(clubSandwich::vcovCR(fit, cluster = cluster,
                              type = "CR2", inverse_var = FALSE))
  }
  stopifnot(fit$family$family == "quasibinomial")
  X <- model.matrix(fit)
  working_weights <- weights(fit, type = "working")
  weighted_residuals <- as.numeric(residuals(fit, type = "working")) * working_weights
  dispersion <- sum(weighted_residuals^2) / sum(working_weights)
  scale <- sum(summary(fit)$df[1:2]) * dispersion
  bread <- solve(crossprod(X, working_weights * X)) * scale
  meat <- clubSandwich::vcovCR(fit, cluster = cluster, type = "CR2",
                              inverse_var = FALSE, form = "meat")
  V <- meat
  V[,] <- bread %*% meat %*% bread / attr(meat, "v_scale")
  attr(V, "bread") <- bread
  V
}


risk_category <- function(demand, supply) {
  ifelse(is.na(demand) | is.na(supply), "unassessed",
    ifelse(demand & supply, "both",
      ifelse(demand, "demand", ifelse(supply, "supply", "neither"))))
}

supply_constraint_score <- function(supply, component_trace, constraints_reference = TRUE) {
  fields <- unique(component_trace$variable[component_trace$side == "Supply"])
  standardized <- sapply(fields, function(k) {
    ref <- component_trace[component_trace$side == "Supply" & component_trace$variable == k, ]
    mu <- unique(ref$indicator_mean); s <- unique(ref$indicator_SD)
    if (!constraints_reference) mu <- 100 - mu
    stopifnot(length(mu) == 1, length(s) == 1, is.finite(s), s > 0)
    risk <- if (k == "stockout_burden") supply[[k]] else 1 - supply[[k]]
    (100 * risk - mu) / s
  })
  rowMeans(standardized)
}

orient_supply_constraints <- function(payload) {
  previous <- payload$scores
  if (!identical(payload$audit$supply_score_orientation, "constraints")) {
  ii <- payload$scores$method != "median_count"
  payload$scores$supply_score[ii] <- -payload$scores$supply_score[ii]
  ti <- payload$thresholds$method != "median_count"
  payload$thresholds$supply_cutoff[ti] <- -payload$thresholds$supply_cutoff[ti]
  payload$thresholds$supply_rule[ti] <- ifelse(
    payload$thresholds$method[ti] == "zscore",
    "strictly above constraints-score mean (zero)",
    "strictly above constraints-score median")
  tr <- payload$component_trace
  si <- tr$side == "Supply"
  tr$aligned_pct[si] <- 100 - tr$aligned_pct[si]
  tr$indicator_mean[si] <- 100 - tr$indicator_mean[si]
  if ("indicator_median" %in% names(tr)) tr$indicator_median[si] <- 100 - tr$indicator_median[si]
  tr$zscore[si] <- -tr$zscore[si]
  if ("robust" %in% names(tr)) tr$robust[si] <- -tr$robust[si]
  if ("component_rank" %in% names(tr)) {
    for (k in unique(tr$variable[si])) {
      j <- si & tr$variable == k
      n <- if ("reference_n" %in% names(tr)) unique(tr$reference_n[j]) else sum(is.finite(tr$component_rank[j]))
      tr$component_rank[j] <- n + 1 - tr$component_rank[j]
    }
  }
  payload$component_trace <- tr
  if (is.data.frame(payload$planning)) {
    payload$planning$supply_score <- -payload$planning$supply_score
  } else for (j in seq_along(payload$planning)) {
    payload$planning[[j]]$supply_score <- -payload$planning[[j]]$supply_score
  }
  }
  primary <- payload$scores$method == "zscore"
  # A sign reversal, with the inequality reversed, must preserve every flag.
  stopifnot(identical(payload$scores$category, previous$category),
    identical(payload$scores$supply_flag[primary],
              payload$scores$supply_score[primary] > 1e-10))
  payload$audit$supply_score_orientation <- "constraints"
  payload$audit$supply_alignment <- paste(
    "All six scored supply components increase with constraints.",
    "Stockout burden retains its risk direction; the other five use 1 - provision.",
    "Risk equals negative readiness. Raw heatmap percentages are unchanged.")
  payload$audit$scoring_direction <- "Demand > 0 and supply constraints > 0; strict zero ties are unflagged."
  payload$audit$scoring_reference <- paste(
    "Each side uses its complete-area reference set with equal weight per area.",
    "Center each risk-aligned component on its area mean and divide by its sample SD; average six z-scores.",
    "Demand percentages use SURGO weights; facility summaries use equal-facility valid-response means.")
  payload$audit$four_category_labels <- "Relative above-average demand barriers and supply constraints; neither flagged does not mean no barriers or adequate services."
  payload$audit$survey_design <- "SURGO weights with ids=~1, no strata or FPC. Original sampling PSUs are not specified. Regression uncertainty is separately clustered by administrative area using CR2 and Satterthwaite tests."
  payload$audit$sign_reversal_flags_unchanged <- TRUE
  payload
}

# Sensitivity analyses ---------------------------------------------------------
# The primary index is the equal-weight mean of six component z-scores. These
# alternatives use the same inputs and retain their earlier decision rules.
alternative_area_scores <- function(payload) {
  areas <- payload$scores$geo[payload$scores$method == "zscore"]
  matrices <- references <- list()
  for (side in c("Demand", "Supply")) {
    fields <- if (side == "Demand") payload$demand_variables else payload$supply_variables
    tr <- payload$component_trace[payload$component_trace$side == side, ]
    m <- sapply(fields, function(k) {
      a <- tr[tr$variable == k, ]; a$aligned_pct[match(areas, a$geo)]
    })
    rownames(m) <- areas
    ref <- complete.cases(m)
    med <- apply(m[ref, , drop = FALSE], 2, median)
    iqr <- apply(m[ref, , drop = FALSE], 2, IQR)
    ranks <- matrix(NA_real_, nrow(m), ncol(m), dimnames = dimnames(m))
    ranks[ref, ] <- apply(m[ref, , drop = FALSE], 2, rank, ties.method = "average")
    robust <- if (all(iqr > 0)) rowMeans(sweep(sweep(m, 2, med, "-"), 2, iqr, "/")) else rep(NA_real_, nrow(m))
    matrices[[side]] <- list(rank = rowMeans(ranks), raw = rowMeans(m),
      robust = robust, median_count = rowSums(sweep(m, 2, med, "-") > 1e-10))
    references[[side]] <- sum(ref)
  }
  z <- payload$scores[payload$scores$method == "zscore", ]
  result <- list(z); cuts <- list(payload$thresholds[payload$thresholds$method == "zscore", ])
  for (id in c("rank", "raw", "robust", "median_count")) {
    ds <- matrices$Demand[[id]]; ss <- matrices$Supply[[id]]
    dc <- if (id == "median_count") 4 else if (any(is.finite(ds))) median(ds, na.rm = TRUE) else NA_real_
    sc <- if (id == "median_count") 4 else if (any(is.finite(ss))) median(ss, na.rm = TRUE) else NA_real_
    df <- if (id == "median_count") ds >= dc else ds > dc + 1e-10
    sf <- if (id == "median_count") ss >= sc else ss > sc + 1e-10
    result[[id]] <- data.frame(method = id, geo = areas, demand_score = ds,
      supply_score = ss, demand_flag = df, supply_flag = sf,
      demand_priority_rank = rank(-ds, ties.method = "average", na.last = "keep"),
      supply_priority_rank = rank(-ss, ties.method = "average", na.last = "keep"),
      category = risk_category(df, sf))
    cuts[[id]] <- data.frame(method = id, demand_cutoff = dc, supply_cutoff = sc,
      demand_rule = if (id == "median_count") "4 or more of six above component medians" else "strictly above final-score median",
      supply_rule = if (id == "median_count") "4 or more of six constraint components above medians" else "strictly above constraints-score median",
      demand_reference_n = sum(is.finite(ds)), supply_reference_n = sum(is.finite(ss)))
  }
  payload$scores <- do.call(rbind, result)
  payload$thresholds <- do.call(rbind, cuts)
  payload
}

add_sd_thresholds <- function(payload, cutoffs = c(0, 0.5, 1, 1.5)) {
  z <- payload$scores[payload$scores$method == "zscore", ]
  dm <- mean(z$demand_score, na.rm = TRUE); ds <- sd(z$demand_score, na.rm = TRUE)
  sm <- mean(z$supply_score, na.rm = TRUE); ss <- sd(z$supply_score, na.rm = TRUE)
  stopifnot(ds > 0, ss > 0, abs(dm) < 1e-8, abs(sm) < 1e-8)
  du <- (z$demand_score - dm) / ds; su <- (z$supply_score - sm) / ss
  payload$risk_distribution <- data.frame(geo = z$geo, demand_score = z$demand_score,
    supply_score = z$supply_score, demand_sd = du, supply_sd = su)
  payload$score_distribution_reference <- list(
    demand = list(n = sum(is.finite(z$demand_score)), mean = dm, SD = ds),
    supply = list(n = sum(is.finite(z$supply_score)), mean = sm, SD = ss),
    definition = "Each side uses its own finite area scores, with equal weight per area. SD refers to the final composite, not an individual component or a standard error.")
  rows <- lapply(cutoffs, function(t) {
    df <- if (t == 0) z$demand_score > 1e-10 else du >= t
    sf <- if (t == 0) z$supply_score > 1e-10 else su >= t
    data.frame(geo = z$geo, threshold_sd = t, demand_score = z$demand_score,
      supply_score = z$supply_score, demand_sd = du, supply_sd = su,
      demand_flag = df, supply_flag = sf, category = risk_category(df, sf))
  })
  payload$sd_thresholds <- do.call(rbind, rows)
  stopifnot(identical(payload$sd_thresholds$category[payload$sd_thresholds$threshold_sd == 0], z$category))
  payload
}

export_risk_scores <- function(payload, out) {
  payload <- orient_supply_constraints(payload)
  payload <- alternative_area_scores(payload)
  payload <- add_sd_thresholds(payload)
  write <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
  write(payload$scores, "all_method_scores_and_classifications.csv")
  write(payload$thresholds, "classification_thresholds.csv")
  write(payload$component_trace, "component_score_trace.csv")
  write(payload$sd_thresholds, "sd_threshold_classifications.csv")
  write(payload$risk_distribution, "area_risk_score_distributions.csv")
  jsonlite::write_json(payload, file.path(out, "report_data.json"), dataframe = "rows",
                       auto_unbox = TRUE, na = "null", digits = 12)
  jsonlite::write_json(payload$audit, file.path(out, "analysis_audit.json"),
                       auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(payload)
}


render_modern_use_plots <- function(coefficients, curves, gaps, cc, demand_labels,
                                   demand_variables, out, score_method,
                                   demand_resolution, resolution, country, supply_pca_mixed = NULL) {
  cluster <- switch(country, lagos = "LGA", sindh = "district", senegal = "department", cotedivoire = "department")
  adjustment <- switch(country,
    lagos = "All models adjust for age + age squared, education, partnership, fertility goal and urban/rural residence.",
    sindh = "All models adjust for age + age squared, country-specific education and fertility goal.",
    senegal = "All models adjust for age + age squared, Senegal-specific education, partnership, fertility goal and Rural/Urban residence.",
    cotedivoire = "All models adjust for age + age squared, Côte d’Ivoire-specific education, partnership, fertility goal and Rural/Urban residence.")
  pca_note <- switch(country,
    lagos = "Higher scores follow the average concern / weaker-provision direction; PC1 captures only part of each side's variation.",
    sindh = "Demand PC1 has mixed signs and describes a contrast; supply risk runs higher = weaker provision.",
    senegal = "Interpret PCA using its signed coefficients; supply PC1 is negated for modeling.",
    cotedivoire = "Supply PC1 has mixed signs: its negative is a pattern score, not a monotonic service-risk index. Interpret signed coefficients.")
  inference <- switch(country,
    lagos = "Geographic small-sample inference; official EA/stratum design remains unconfirmed. No random effects or interactions.",
    sindh = "District CR2 intervals; sampling-cluster design sensitivity is available. No urban/rural variable. No random effects or interactions.",
    senegal = "Department CR2 intervals; sampling-cluster design sensitivity is available. Residence-specific supply where available; flagged department fallback otherwise. No random effects or interactions.",
    cotedivoire = "Department CR2 intervals; sampling-cluster design sensitivity is available. Exact department-residence supply; no geographic fallback. No random effects or interactions.")
  supply_note <- switch(country,
    lagos = if (resolution == "urban_rural") "Supply uses matched LGA-rural/urban profiles; " else "",
    sindh = "Supply uses matched district profiles; ",
    senegal = "Supply uses assigned department-residence / fallback profiles; ",
    cotedivoire = "Supply uses assigned department-residence profiles; ")
  if (country == "lagos" && resolution == "urban_rural_fallback") {
    supply_note <- paste0("LGA-residence supply; ", sum(cc$fallback),
      " modeled women use a flagged LGA-wide fallback; ")
    inference <- paste0("LGA CR2 inference; ", sum(cc$fallback),
      " women use a flagged LGA-wide fallback. Official EA/strata remain unconfirmed. No random effects or interactions.")
  }
  if (country == "senegal" && resolution == "urban_rural_fallback") {
    supply_note <- paste0("Département-residence supply; ", sum(cc$fallback),
      " modeled women use flagged département-wide fallbacks; ")
    inference <- "Département CR2 intervals; sampling-cluster sensitivity available. Facility residence is a health-district lookup proxy. No random effects or interactions."
  }
  if (country %in% c("senegal", "cotedivoire") && resolution == "department") {
    inference <- "Department CR2 intervals; sampling-cluster sensitivity is available. Department-wide supply for every woman. No random effects or interactions."
    supply_note <- "Supply uses matched department-wide profiles; "
  }
  mixed_supply <- score_method == "pca" && if (is.null(supply_pca_mixed)) country == "cotedivoire" else isTRUE(supply_pca_mixed)
  if (mixed_supply) pca_note <- "Supply PC1 has mixed signs; its negative is a pattern, not uniformly weaker provision."
  D <- demand_variables
  labels <- setNames(demand_labels, D)
  labels <- c(labels, demand_score = if (score_method == "pca" && country != "lagos") switch(country,
    sindh = "Demand pattern score (PCA)", senegal = "Demand risk score (PCA)", cotedivoire = "Demand score (PCA)") else
    if (demand_resolution == "individual") "Demand risk score" else "LGA demand risk score",
    supply_risk = if (mixed_supply) "Supply pattern score (PCA)" else "Supply risk score")
  colors <- c(Demand = "#087f8a", Supply = "#be6b23")
  base_theme <- ggplot2::theme_minimal(base_family = "sans", base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#244354"),
      plot.title = ggplot2::element_text(face = "bold", size = 17, color = "#17374a"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#5f7886", lineheight = 1.2),
      strip.text = ggplot2::element_text(face = "bold", color = "#17374a", size = 11),
      strip.background = ggplot2::element_rect(fill = "#edf4f6", color = NA),
      legend.position = "bottom", legend.title = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.4, "lines"),
      plot.caption = ggplot2::element_text(hjust = 0, color = "#607a87", size = 9, lineheight = 1.2),
      plot.margin = ggplot2::margin(15, 18, 12, 12))
  saveplot <- function(g, name, h) {
    ggplot2::ggsave(file.path(out, paste0(name, ".png")), g, width = 14, height = h, dpi = 170, bg = "white")
    ggplot2::ggsave(file.path(out, paste0(name, ".svg")), g, width = 14, height = h, device = svglite::svglite, bg = "white")
  }
  forest <- function(ids, titles, fields, title, name, height, caption) {
    a <- coefficients[coefficients$model %in% ids & coefficients$term %in% fields, ]
    a$panel <- factor(titles[match(a$model, ids)], levels = titles)
    a$row <- factor(a$term, levels = rev(fields)); a$source <- ifelse(a$term == "supply_risk", "Supply", "Demand")
    stopifnot(all(is.finite(a$or_lo)), all(is.finite(a$or_hi)), all(a$or_lo > 0))
    g <- ggplot2::ggplot(a, ggplot2::aes(odds_ratio, row, color = source)) +
      ggplot2::geom_vline(xintercept = 1, color = "#8095a0", linetype = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = or_lo, xmax = or_hi), orientation = "y", width = .08, linewidth = .8) +
      ggplot2::geom_point(size = 3) + ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") +
      ggplot2::scale_x_log10(breaks = if (country == "lagos") c(.01, .025, .05, .1, .25, .5, .75, 1, 1.25, 1.5, 2, 4, 10, 25, 50, 100) else c(.01, .05, .1, .25, .5, 1, 2, 4, 10, 25, 50, 100)) +
      ggplot2::scale_y_discrete(labels = labels) + ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(title = title,
        subtitle = paste0("Modern contraceptive use · same ", format(nrow(cc), big.mark = ","), " women · SURGO weights · 95% ", cluster, "-clustered CR2/Satterthwaite intervals"),
        x = "Adjusted odds ratio (log scale; 1 = no association)", y = NULL, caption = caption) + base_theme
    saveplot(g, name, height)
  }
  forest(c("score_demand", "supply", "score_joint"), if (mixed_supply) c("Demand PCA + covariates", "Supply PCA + covariates", "Both PCA scores + covariates") else c("Demand risk + covariates", "Supply risk + covariates", "Both scores + covariates"),
    c("demand_score", "supply_risk"), if (mixed_supply) "PCA sensitivity: demand and supply pattern scores" else if (score_method == "pca") "PCA sensitivity: demand and supply risk scores" else "Adjusted associations: demand and supply risk scores", "01_score_models", 5.2,
    paste0(if (score_method == "pca") paste0("Odds ratios are per 1 oriented PC1 score unit, with no final-score restandardization.\n", pca_note, "\n") else "Each odds ratio is per 1-point increase in the original average component z-score; the final index is not restandardized.\nHigher demand and higher supply risk both represent less favorable conditions. ", adjustment,
      paste0("\n", inference)))
  curves$panel <- factor(curves$term, levels = c("demand_score", "supply_risk"), labels = c(unname(labels["demand_score"]), unname(labels["supply_risk"])))
  curves$source <- ifelse(curves$term == "supply_risk", "Supply", "Demand")
  curves$spec <- ifelse(curves$model == "score_joint", "Both-score model", "Single-score model")
  g <- ggplot2::ggplot(curves, ggplot2::aes(value, estimate * 100, color = source, linetype = spec)) +
    ggplot2::geom_ribbon(data = curves[curves$model == "score_joint", ], ggplot2::aes(ymin = lo * 100, ymax = hi * 100, fill = source), alpha = .12, color = NA) +
    ggplot2::geom_line(linewidth = .95) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_x") + ggplot2::scale_color_manual(values = colors, guide = "none") +
    ggplot2::scale_fill_manual(values = colors, guide = "none") + ggplot2::scale_linetype_manual(values = c("Both-score model" = "solid", "Single-score model" = "dashed")) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    ggplot2::labs(title = if (mixed_supply) "PCA sensitivity: adjusted modern use across demand and supply patterns" else "How adjusted modern-use probabilities vary with demand and local supply risk",
      subtitle = "Weighted average predictions over the same women's observed covariates; the other score stays at its observed value",
      x = if (score_method == "pca") if (mixed_supply) "PCA score (interpret using the signed coefficients)" else if (country == "lagos") "PCA score (higher = greater concern or weaker provision)" else "PCA score (interpret using the signed coefficients)" else "Risk score (average standardized components)", y = "Predicted modern use (%)",
      caption = paste0("Solid lines: both-score model, with pointwise 95% ", cluster, "-clustered CR2 intervals. Dashed lines: single-score models.\nCurves span the observed score ranges. ",
        supply_note, "these are associations, not intervention effects.")) + base_theme +
    ggplot2::scale_x_continuous(breaks = function(limits) pretty(limits, n = 5)) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e8eff2", linewidth = .3))
  if (country != "lagos") g <- g + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(), panel.border = ggplot2::element_blank())
  saveplot(g, "02_score_probabilities", 6)
  forest(c("components_demand", "supply", "components_joint"), if (mixed_supply) c("Six demand indicators", "Supply PCA pattern", "Six indicators + supply PCA") else c("Six demand indicators", "Supply risk score", "Six indicators + supply risk"),
    c(D, "supply_risk"), if (mixed_supply) "PCA sensitivity: individual concerns alongside the local supply pattern" else if (score_method == "pca") "PCA sensitivity: individual concerns alongside local supply PC1" else "Adjusted associations: individual concerns alongside local supply risk", "03_component_models", 7.4,
    paste0(if (mixed_supply) "Demand indicators: concern present versus absent. Supply: per 1 negative-PC1 pattern point; mixed coefficients preclude an overall risk interpretation.\n" else if (score_method == "pca") "Demand indicators: concern present versus absent. Supply: per 1 risk-oriented PC1 point.\n" else "Demand indicators: concern present versus absent. Supply risk: per 1 original index point higher.\n", adjustment,
      "\nThe supply-only model is identical to the preceding section. The MC5 label denotes confidence, not an objective knowledge test."))
  gaps$row <- factor(gaps$term, levels = rev(D))
  g <- ggplot2::ggplot(gaps, ggplot2::aes(estimate * 100, row)) +
    ggplot2::geom_vline(xintercept = 0, color = "#8095a0", linetype = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo * 100, xmax = hi * 100), orientation = "y", width = .18, linewidth = .85, color = colors["Demand"]) +
    ggplot2::geom_point(size = 3, color = colors["Demand"]) + ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::labs(title = "Adjusted modern-use differences for individual reported concerns",
      subtitle = "Six-indicator + supply-risk model; same women and covariates as the coefficient plot",
      x = "Percentage-point difference: concern present minus absent (0 = no gap)", y = NULL,
      caption = "For each concern, predict every woman's modern use with that indicator set to 1 and then 0; average with SURGO weights.\nCapped 95% intervals include covariance between the paired predictions. Other predictors remain observed; differences are not causal effects.") + base_theme
  saveplot(g, "04_component_probability_gaps", 6)
  labels
}




# Prepare fp provider data ----------------------------------------------------


prepare_fp_facility_profiles <- function(input_path, facility_path, raw_facility_path, raw_women_path, dictionary_path, output_dir) {
    input <- input_path
    facility_input <- facility_path
    raw_facility <- raw_facility_path
    raw_women <- raw_women_path
    dictionary_file <- dictionary_path
    out <- output_dir
    inputs <- c(input, facility_input, raw_facility, raw_women, dictionary_file)
    hash <- unname(tools::md5sum(inputs))
    fac <- readRDS(facility_input)
    fac <- fac[fac$country == "lagos", ]
    r <- haven::read_dta(raw_facility, col_select = tidyselect::all_of(c("key", "q506", "q702", lagos_frequency_fields())))
    assert_unique(data.frame(fid = as.character(r$key)), "fid", "Raw Lagos facilities")
    ix <- match(fac$fid, as.character(r$key))
    stopifnot(!anyNA(ix), nrow(fac) == nrow(r), nrow(fac) == 1741)
    fp <- num(r$q506[ix])
    fac$fp_provider <- fp %in% 1
    counsel <- num(r$q702[ix])
    cf <- ifelse(counsel %in% 1, 1, ifelse(counsel %in% 2, 0, NA_real_))
    stopifnot(isTRUE(all.equal(cf, fac$counselor_available, check.attributes = FALSE)))
    frequency <- lagos_frequency(r[ix, ])
    fac <- cbind(fac, frequency)
    eligible <- fac[fac$fp_provider, ]
    dictionary <- read_chars(dictionary_file)
    dictionary <- dictionary[dictionary$table_name == "merged_supply_demand_department", ]
    profile <- aggregate_supply(eligible, dictionary, "assigned_join_area")
    profile$mean_daily_modern_provision <- profile$mean_weekly_modern_provision <- NA_real_
    profile$n_daily_modern_valid <- profile$n_frequency_incomplete <- 0L
    for (j in seq_len(nrow(profile))) {
        z <- eligible[eligible$assigned_join_area == profile$geo_join[j], ]
        mean_valid <- function(v) {
            v <- as.numeric(v)
            if (any(is.finite(v)))
                mean(v[is.finite(v)])
            else NA_real_
        }
        profile$share_offer_larc_and_3_sa[j] <- mean_valid(bool(z$one_larc_3_sa_offer))
        profile$share_stocked_1plus_larc[j] <- mean_valid(bool(z$one_larc_stockin))
        profile$mean_daily_modern_provision[j] <- mean_valid(z$daily_provision)
        profile$mean_weekly_modern_provision[j] <- mean_valid(z$at_least_weekly_provision)
        profile$n_daily_modern_valid[j] <- sum(is.finite(z$daily_provision))
        profile$n_frequency_incomplete[j] <- sum(z$frequency_unknown_categories > 0)
    }
    original <- readRDS(input)
    joined <- original
    key <- function(v) {
        v <- toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
        v[v %in% "IFAKOIJAIYE"] <- "IFAKOIJAYE"
        v
    }
    gi <- match(key(original$link_geo_join), key(profile$geo_join))
    stopifnot(!anyNA(gi), !anyDuplicated(key(profile$geo_join)), nrow(profile) == 20)
    replace <- setdiff(names(profile), c("country", "join_level", "geo_join"))
    for (k in replace) joined[[k]] <- profile[[k]][gi]
    joined$link_has_supply <- !is.na(gi)
    joined$link_fp_provider_only <- TRUE
    joined$link_geo_mode <- "audited_fp_providers"
    raw <- read_women(raw_women)
    same <- function(a, b) isTRUE(all.equal(as.vector(a), as.vector(b), check.attributes = FALSE, tolerance = 0))
    preserved <- vapply(names(raw), function(k) same(raw[[k]], joined[[k]]), logical(1))
    stopifnot(nrow(joined) == 4555, nrow(raw) == 4555, identical(joined$uid, original$uid), all(preserved))
    linked <- key(fac$assigned_join_area) %in% key(original$link_geo_join)
    coverage <- do.call(rbind, lapply(sort(unique(fac$assigned_join_area)), function(g) {
        z <- fac[fac$assigned_join_area == g, ]
        data.frame(lga = g, source_records = nrow(z), fp_yes = sum(z$fp_provider))
    }))
    exception <- key(fac$assigned_join_area) == "IBEJULEKKI" & fac$rurb %in% "Urban"
    stopifnot(sum(exception) == 1, fp[exception] == 2, is.na(fac$p_free_prod[exception]))
    audit <- list(source_women = nrow(raw), original_survey_columns = length(preserved), original_fields_preserved = all(preserved),
        source_facilities = nrow(fac), fp_yes = sum(fp %in% 1), fp_no = sum(fp %in% 2), fp_unknown = sum(!fp %in%
            1:2), eligibility_question = attr(r$q506, "label"), eligibility_rule = "Q506=1 (Yes), on-site FP services; No and unknown excluded.",
        linked_source_facilities = sum(linked), linked_fp_providers = sum(linked & fac$fp_provider),
        linked_lgas = length(unique(gi)), daily_valid_linked = sum(linked & fac$fp_provider & is.finite(fac$daily_provision)),
        daily_undefined_linked = sum(linked & fac$fp_provider & !is.finite(fac$daily_provision)), daily_definition = "Daily classes (code 1) divided by recorded offered classes (codes 1-4); code 5 Not at all excluded. Seven method classes; most frequent recorded IUD/injectable variant. No removals or sterilization. Average valid facility ratios among Q506 Yes.",
        stockout_definition = "Original cleaned UoM p_any_stockout, averaged among Q506 Yes with valid values; unchanged upstream coding.",
        zero_exception = "The prior Ibeju-Lekki Urban free-product exception concerns a Q506 No facility, which is excluded. No undefined free-product value is recoded in this update.",
        all_supply_columns_reaggregated = replace, source_files_unchanged = identical(hash, unname(tools::md5sum(inputs))))
    stopifnot(audit$source_files_unchanged, audit$fp_yes == 1040, audit$linked_fp_providers == 882, audit$linked_lgas ==
        17)
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    saveRDS(joined, file.path(out, "lagos_women_manitoba_fp_providers.rds"), compress = "gzip")
    haven::write_dta(joined, file.path(out, "lagos_women_manitoba_fp_providers.dta"), version = 15)
    back <- haven::read_dta(file.path(out, "lagos_women_manitoba_fp_providers.dta"))
    stopifnot(nrow(back) == nrow(raw), all(vapply(names(raw), function(k) {
        a <- raw[[k]]
        b <- back[[k]]
        if (is.character(a)) {
            a[is.na(a)] <- ""
            b[is.na(b)] <- ""
            identical(as.vector(a), as.vector(b))
        } else if (is.logical(a)) same(as.numeric(a), as.numeric(b)) else same(a, b)
    }, logical(1))))
    saveRDS(profile, file.path(out, "lagos_fp_provider_profiles.rds"))
    write.csv(profile, file.path(out, "lagos_fp_provider_profiles.csv"), row.names = FALSE, na = "")
    write.csv(coverage, file.path(out, "fp_provider_coverage.csv"), row.names = FALSE)
    write.csv(fac[c("fid", "assigned_join_area", "fp_provider", names(frequency))], file.path(out, "daily_provision_facility_audit.csv"),
        row.names = FALSE, na = "")
    jsonlite::write_json(audit, file.path(out, "dataset_verification.json"), auto_unbox = TRUE, pretty = TRUE)
    cat("Verified original women; reaggregated supply among", audit$linked_fp_providers, "FP facilities across 17 linked LGAs.\n")
}


# Build analysis --------------------------------------------------------------


calculate_descriptives_and_area_scores <- function(input_path, facility_path, output_dir, raw_facility_path) {
    input <- input_path
    facility_input <- facility_path
    out <- output_dir
    raw_facility <- raw_facility_path
    psu_var <- NULL
    strata_var <- NULL
    facility_sample <- "all"
    if (is.null(facility_sample))
        facility_sample <- "all"
    demand_sample <- "available"
    if (is.null(demand_sample))
        demand_sample <- "available"
    reference_dir <- ""
    if (is.null(reference_dir))
        reference_dir <- ""
    stopifnot(facility_sample == "all", demand_sample == "available", reference_dir == "")
    if (!is.null(strata_var) && is.null(psu_var))
        stop("--strata-var requires a confirmed --psu-var")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    hash_before <- tools::md5sum(c(input, facility_input))
    x <- readRDS(input)
    fac <- readRDS(facility_input)
    fac <- fac[fac$country %in% "lagos", ]
    nv <- function(k) as.numeric(x[[k]])
    key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
    wm <- function(v, w) {
        ok <- is.finite(v) & is.finite(w) & w > 0
        if (any(ok))
            survey_weighted_mean(v[ok], w[ok])
        else NA_real_
    }
    mean_valid <- function(v) {
        v <- as.numeric(v)
        if (any(is.finite(v)))
            mean(v[is.finite(v)])
        else NA_real_
    }
    bool <- function(v) {
        a <- tolower(as.character(v))
        ifelse(a %in% c("true", "1", "yes"), 1, ifelse(a %in% c("false", "0", "no"), 0, NA_real_))
    }
    flag <- function(k, pos, valid) {
        v <- nv(k)
        ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_)
    }
    any_concern <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(m)) >
        0, NA_real_, 0))
    D <- c("agency", "stigma", "health", "fertility", "confidence", "opposes")
    DL <- c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs",
        "Future-fertility concerns", "Low method knowledge", "Opposes family planning")
    domains <- lagos_domains()
    S <- domains$S
    SL <- domains$SL
    stopifnot(nrow(x) == 4555, !anyDuplicated(x$uid), all(x$link_has_supply))
    x$link_rurb <- as.character(x$settlement)
    d <- lagos_derive_women(x)
    pb2a <- as.numeric(x$pb2a)
    d$health <- ifelse(pb2a %in% 1:4, as.numeric(pb2a %in% 1:2), NA_real_)
    d$id <- as.character(x$uid)
    d$district <- d$lga
    d$positive <- is.finite(d$weight) & d$weight > 0
    d$psu <- d$geo
    d$fallback <- FALSE
    d$source_profile <- d$geo
    valid_weight <- is.finite(d$weight) & d$weight > 0
    stopifnot(all(valid_weight[d$need]), sum(d$need) == 2159)
    options(survey.lonely.psu = "adjust")
    design_confirmed <- !is.null(psu_var)
    if (design_confirmed) {
        stopifnot(psu_var %in% names(x))
        d$.psu <- as.character(x[[psu_var]])
        stopifnot(all(!is.na(d$.psu[valid_weight]) & nzchar(d$.psu[valid_weight])))
        strata_formula <- NULL
        if (!is.null(strata_var)) {
            stopifnot(strata_var %in% names(x))
            d$.stratum <- as.character(x[[strata_var]])
            stopifnot(all(!is.na(d$.stratum[valid_weight]) & nzchar(d$.stratum[valid_weight])))
            strata_formula <- ~.stratum
        }
        full <- survey::svydesign(ids = ~.psu, strata = strata_formula, weights = ~weight, data = d[valid_weight,
            ], nest = TRUE)
    }
    else {
        full <- survey::svydesign(ids = ~1, weights = ~weight, data = d[valid_weight, ])
    }
    need_design <- subset(full, need)
    estimate <- function(des, field) {
        v <- des$variables[[field]]
        p <- survey_weighted_mean(as.numeric(v), des$variables$weight)
        ci <- c(NA_real_, NA_real_)
        if (design_confirmed && p > 0 && p < 1 && survey::degf(des) > 0) {
            des$variables$.event <- as.numeric(v)
            ci <- as.numeric(confint(survey::svyciprop(~.event, des, method = "logit", df = survey::degf(des))))
        }
        list(n = sum(v), denominator = length(v), estimate = p, lo = ci[1], hi = ci[2])
    }
    bar_defs <- data.frame(field = c("user", "wanted_start", "other_start", "modern", "modern_other_start",
        "traditional", "traditional_modern_switch", "intender", "no_intent", "unresolved"), label = c("Current contraceptive users",
        "Using the method wanted at start*", "Not using the method wanted at start*", "Current modern-method users",
        "Wanted another method at start*", "Traditional methods (subset of users)", "Wants to switch to a modern method",
        "Nonusers with intention", "Nonusers without stated intention", "Use / intention unresolved"),
        indent = c(0, 1, 1, 0, 1, 0, 1, 0, 0, 0), y = c(0, 1, 2, 3.8, 4.8, 6.6, 7.6, 9.4, 11, 12.6),
        color = c("#0a7d89", "#579b90", "#ba8743", "#307e60", "#ba8743", "#8c8981", "#b57a32", "#446da4",
            "#89639a", "#9caab0"), stringsAsFactors = FALSE)
    bars <- lapply(seq_len(nrow(bar_defs)), function(i) c(as.list(bar_defs[i, ]), estimate(need_design,
        bar_defs$field[i])))
    if (tail(bars, 1)[[1]]$n == 0)
        bars <- head(bars, -1)
    dn <- d[d$need, ]
    areas <- sort(unique(dn$geo))
    rates <- lapply(areas, function(g) {
        a <- subset(need_design, geo == g)
        list(geo = g, n = nrow(a$variables), cpr = estimate(a, "user"), mcpr = estimate(a, "modern"),
            intender = estimate(a, "intender"), no_intent = estimate(a, "no_intent"))
    })
    fac$geo <- ifelse(!is.na(fac$geo_join_pipeline), key(fac$geo_join_pipeline), key(fac$source_geo_name))
    stopifnot(all(!is.na(fac$geo)), length(unique(fac$geo)) == 20)
    inventory <- fac
    r <- haven::read_dta(raw_facility, col_select = tidyselect::all_of(c("key", "q506", lagos_frequency_fields())))
    ri <- match(fac$fid, as.character(r$key))
    stopifnot(!anyNA(ri))
    frequency <- lagos_frequency(r[ri, ])
    fac <- cbind(fac, frequency)
    fp <- as.numeric(r$q506[ri])
    fac <- fac[fp %in% 1, ]
    fac$offering <- bool(fac$one_larc_3_sa_offer)
    fac$free <- as.numeric(fac$p_free_prod)
    fac$training <- as.numeric(fac$p_train)
    fac$larc_stock <- bool(fac$one_larc_stockin)
    fac$stockout_burden <- as.numeric(fac$p_any_stockout)
    stopifnot(nrow(fac) == 1040, sum(fac$geo %in% areas) == 882)
    savecsv0 <- function(z, name) write.csv(z, file.path(out, name), row.names = FALSE, na = "")
    savecsv0(fac[c("fid", "geo", names(frequency))], "daily_provision_facility_audit.csv")
    assigned_key <- key(fac$assigned_join_area)
    assigned_key[assigned_key == "IFAKOIJAIYE"] <- "IFAKOIJAYE"
    stopifnot(all(assigned_key == fac$geo))
    fac$complete_supply <- complete.cases(fac[S])
    fac$included <- if (facility_sample == "complete")
        fac$complete_supply
    else TRUE
    savecsv <- function(z, name, rows = FALSE) write.csv(z, file.path(out, name), row.names = rows, na = "")
    savecsv(data.frame(geo = areas, total_facilities = vapply(areas, function(g) sum(fac$geo == g), integer(1)),
        complete_facilities = vapply(areas, function(g) sum(fac$geo == g & fac$complete_supply), integer(1))),
        "facility_complete_case_coverage.csv")
    dn_complete <- complete.cases(dn[D])
    savecsv(do.call(rbind, lapply(c("LAGOS", areas), function(g) {
        ii <- if (g == "LAGOS")
            rep(TRUE, nrow(dn))
        else dn$geo == g
        data.frame(geo = g, women_in_need = sum(ii), complete_demand = sum(ii & dn_complete), excluded = sum(ii &
            !dn_complete), complete_weight_share = survey_weighted_mean(as.numeric(dn_complete[ii]),
            dn$weight[ii]))
    })), "demand_complete_case_coverage.csv")
    savecsv(do.call(rbind, lapply(D, function(k) data.frame(variable = k, missing = sum(!is.finite(dn[[k]])),
        total = nrow(dn), missing_pct = 100 * mean(!is.finite(dn[[k]])), weighted_missing_pct = 100 *
            wm(as.numeric(!is.finite(dn[[k]])), dn$weight)))), "demand_missingness.csv")
    savecsv(do.call(rbind, lapply(S, function(k) {
        ii <- fac$geo %in% areas
        data.frame(variable = k, valid = sum(is.finite(fac[[k]][ii])), missing = sum(!is.finite(fac[[k]][ii])),
            total = sum(ii), missing_pct = 100 * mean(!is.finite(fac[[k]][ii])))
    })), "supply_missingness.csv")
    savecsv(fac[c("fid", "geo", "source_geo_name", "assigned_join_area", "geo_join_pipeline")], "facility_geography_audit.csv")
    cells <- list()
    meta <- list()
    display_areas <- sort(unique(inventory$geo))
    for (g in c("LAGOS", display_areas)) {
        wi <- if (g == "LAGOS")
            rep(TRUE, nrow(dn))
        else dn$geo %in% g
        fi <- (if (g == "LAGOS")
            fac$geo %in% areas
        else fac$geo %in% g) & fac$included
        meta[[g]] <- list(geo = g, n_need = sum(wi), facility_n = sum(fi), linked = g == "LAGOS" || g %in%
            areas)
        for (j in seq_along(D)) {
            k <- D[j]
            ok <- wi & is.finite(dn[[k]]) & (demand_sample == "available" | dn_complete)
            cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Demand", variable = k, label = DL[j],
                value = if (any(ok))
                  wm(dn[[k]][ok], dn$weight[ok]) * 100
                else NA_real_, n_valid = sum(ok), n_total = sum(wi), numerator = sum(dn[[k]][ok] == 1))
        }
        for (j in seq_along(S)) {
            k <- S[j]
            ok <- fi & is.finite(fac[[k]])
            cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Supply", variable = k, label = SL[j],
                value = mean_valid(fac[[k]][fi]) * 100, n_valid = sum(ok), n_total = sum(fi), numerator = sum(fac[[k]][ok]))
        }
    }
    cells <- do.call(rbind, cells)
    savecsv <- function(z, name, rows = FALSE) write.csv(z, file.path(out, name), row.names = rows, na = "")
    savecsv(cells, "heatmap_values_and_denominators.csv")
    M <- list()
    transforms <- list()
    for (side in c("Demand", "Supply")) {
        cols <- if (side == "Demand")
            D
        else S
        original <- sapply(cols, function(k) {
            z <- cells[cells$side == side & cells$variable == k, ]
            z$value[match(areas, z$geo)]
        })
        m <- if (side == "Supply")
            as.matrix(100 * (1 - lagos_favorable_supply(as.data.frame(original/100))))
        else original
        rownames(m) <- areas
        reference <- complete.cases(m)
        means <- colMeans(m[reference, , drop = FALSE])
        sds <- apply(m[reference, , drop = FALSE], 2, sd)
        meds <- apply(m[reference, , drop = FALSE], 2, median)
        iqrs <- apply(m[reference, , drop = FALSE], 2, IQR)
        ranks <- matrix(NA_real_, nrow(m), ncol(m), dimnames = dimnames(m))
        ranks[reference, ] <- apply(m[reference, , drop = FALSE], 2, rank, ties.method = "average")
        stopifnot(all(sds > 0), all(iqrs > 0))
        M[[side]] <- list(raw = m, rank = ranks, robust = sweep(sweep(m, 2, meds, "-"), 2, iqrs, "/"),
            zscore = sweep(sweep(m, 2, means, "-"), 2, sds, "/"))
        for (k in cols) transforms[[length(transforms) + 1L]] <- data.frame(geo = areas, side = side,
            variable = k, original_pct = original[, k], aligned_pct = m[, k], higher_raw_is_worse = side ==
                "Supply" && k == "stockout_burden", reference_included = reference, reference_n = sum(reference),
            indicator_mean = means[k], indicator_SD = sds[k], indicator_median = meds[k], indicator_IQR = iqrs[k],
            component_rank = M[[side]]$rank[, k], zscore = M[[side]]$zscore[, k], robust = M[[side]]$robust[,
                k])
    }
    classify <- function(df, sf) ifelse(is.na(df) | is.na(sf), "unassessed", ifelse(df & sf, "both",
        ifelse(df, "demand", ifelse(sf, "supply", "neither"))))
    scores <- thresholds <- list()
    for (id in c("rank", "raw", "robust", "zscore")) {
        ds <- rowMeans(M$Demand[[id]])
        ss <- rowMeans(M$Supply[[id]])
        dc <- if (id == "zscore")
            0
        else median(ds, na.rm = TRUE)
        sc <- if (id == "zscore")
            0
        else median(ss, na.rm = TRUE)
        df <- ds > dc + 1e-10
        sf <- ss > sc + 1e-10
        scores[[id]] <- data.frame(method = id, geo = areas, demand_score = ds, supply_score = ss, demand_flag = df,
            supply_flag = sf, demand_priority_rank = rank(-ds, ties.method = "average", na.last = "keep"),
            supply_priority_rank = rank(-ss, ties.method = "average", na.last = "keep"), category = classify(df,
                sf))
        thresholds[[id]] <- data.frame(method = id, demand_cutoff = dc, supply_cutoff = sc, demand_rule = if (id ==
            "zscore")
            "strictly above final-score mean (zero)"
        else "strictly above final-score median", supply_rule = if (id == "zscore")
            "strictly above constraints-score mean (zero)"
        else "strictly above constraints-score median", demand_reference_n = sum(is.finite(ds)), supply_reference_n = sum(is.finite(ss)))
    }
    dc <- rowSums(sweep(M$Demand$raw, 2, apply(M$Demand$raw, 2, median, na.rm = TRUE), "-") > 1e-10)
    sc <- rowSums(sweep(M$Supply$raw, 2, apply(M$Supply$raw, 2, median, na.rm = TRUE), "-") > 1e-10)
    scores$median_count <- data.frame(method = "median_count", geo = areas, demand_score = dc, supply_score = sc,
        demand_flag = dc >= 4, supply_flag = sc >= 4, demand_priority_rank = rank(-dc, ties.method = "average",
            na.last = "keep"), supply_priority_rank = rank(-sc, ties.method = "average", na.last = "keep"),
        category = classify(dc >= 4, sc >= 4))
    thresholds$median_count <- data.frame(method = "median_count", demand_cutoff = 4, supply_cutoff = 4,
        demand_rule = "4 or more of six above component medians", supply_rule = "4 or more of six above constraint-component medians",
        demand_reference_n = sum(is.finite(dc)), supply_reference_n = sum(is.finite(sc)))
    scores <- do.call(rbind, scores)
    thresholds <- do.call(rbind, thresholds)
    trace <- do.call(rbind, transforms)
    exact <- cells[cells$geo %in% areas, c("geo", "side", "variable", "value")]
    exact$original_pct_exact <- sprintf("%.17g", exact$value)
    savecsv(exact[c("geo", "side", "variable", "original_pct_exact")], "scoring_inputs_exact.csv")
    savecsv(scores, "all_method_scores_and_classifications.csv")
    savecsv(thresholds, "classification_thresholds.csv")
    savecsv(trace, "component_score_trace.csv")
    planning <- lapply(areas, function(g) {
        q <- scores[scores$method == "zscore" & scores$geo == g, ]
        dt <- trace[trace$geo == g & trace$side == "Demand", ]
        st <- trace[trace$geo == g & trace$side == "Supply", ]
        dt <- dt[order(-dt$zscore, match(dt$variable, D)), ]
        st <- st[order(-st$zscore, match(st$variable, S)), ]
        list(geo = g, category = q$category, demand_flag = q$demand_flag, supply_flag = q$supply_flag,
            demand_score = q$demand_score, supply_score = q$supply_score, n = meta[[g]]$n_need, top_demand = head(dt$variable,
                3), top_supply = head(st$variable, 3))
    })
    coverage <- lapply(display_areas, function(g) list(geo = g, women = sum(d$geo %in% g), facilities = sum(inventory$geo %in%
        g), fp_facilities = sum(fac$geo %in% g)))
    bar_table <- do.call(rbind, lapply(bars, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
    savecsv(bar_table, "use_distribution_fpn2.csv")
    lga_rates <- do.call(rbind, lapply(rates, function(r) data.frame(geo = r$geo, n_need = r$n, cpr = r$cpr$estimate,
        mcpr = r$mcpr$estimate, intender = r$intender$estimate, no_intent = r$no_intent$estimate)))
    savecsv(lga_rates, "lga_use_rates_fpn2.csv")
    audit <- list(supply_score_orientation = "constraints", country = "Lagos", source_rows = nrow(x),
        positive_weight_rows = sum(valid_weight), need_n = nrow(dn), need_eligibility = "Original SURGO fpn2=1, unchanged; no additional need reconstruction.",
        women_weight = "weight", weighted_need_share = wm(as.numeric(d$need[valid_weight]), d$weight[valid_weight]),
        linked_lgas = length(areas), supply_lgas = length(display_areas), all_facility_records = nrow(inventory),
        linked_lga_facility_records = sum(inventory$geo %in% areas), fp_facility_records = nrow(fac),
        linked_fp_facilities = sum(fac$geo %in% areas), supply_aggregation = "Q506 Yes only; valid-response equal-facility means within LGA, combining Rural and Urban; no facility weights. Genuine zeros included, undefined excluded per measure.",
        free_exception = "No new zero recoding. The prior Ibeju-Lekki Urban exception was a Q506 No facility and is excluded under the FP-provider restriction.",
        health_definition = "PB2A only: 1/2 concern; 3/4 no concern; otherwise unresolved.", stockout_definition = "Original cleaned p_any_stockout unchanged. Heatmap shows burden; every score and PCA uses continuity=1-burden.",
        chart_denominator = "All 2159 women with original fpn2=1; final weight percentages, unweighted n. Overlapping subsets.",
        use_definition = "CU1/CU5 confirmed current use; CU4 and prompts classify modern/traditional; unresolved remains distinct.",
        design_fields_confirmed = design_confirmed, survey_variance = if (design_confirmed) "Confirmed runtime PSU/strata Taylor intervals" else "Final weight point estimates; official EA/PSU and strata unconfirmed, so overview intervals are withheld. No clusters inferred from household IDs or sampling probabilities.",
        four_category_labels = "Relative above-average concerns and below-average readiness; not causal binding constraints or an absolute standard.",
        scoring_reference = "17 linked LGAs equally weighted; each component centered on LGA mean and divided by sample SD. Mean six favorable supply z-scores; reverse stockout to continuity first. Demand>0 and readiness<0; 1e-10 ties unflagged.",
        main_method = "zscore", sources = list(joined_file = basename(input), facility_file = basename(facility_input)),
        source_md5 = as.list(unname(hash_before)), source_files_unchanged = identical(unname(hash_before),
            unname(tools::md5sum(c(input, facility_input)))))
    stopifnot(audit$source_files_unchanged)
    profiles <- data.frame(geo = areas, district = areas)
    for (k in S) profiles[[k]] <- vapply(areas, function(g) mean_valid(fac[[k]][fac$geo == g & fac$included]),
        numeric(1))
    profiles$n_facilities <- vapply(areas, function(g) sum(fac$geo == g & fac$included), integer(1))
    stopifnot(all(profiles$n_facilities > 0))
    sp <- profiles[match(d$geo, profiles$geo), S, drop = FALSE]
    rownames(sp) <- NULL
    map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
        daily_provision = "mean_daily_modern_provision", larc_stock = "share_stocked_1plus_larc", stockout_burden = "mean_facility_any_stockout")
    reconcile <- do.call(rbind, lapply(names(map), function(k) {
        a <- as.numeric(x[[map[k]]])
        b <- sp[[k]]
        data.frame(variable = k, max_abs_difference = max(abs(a - b), na.rm = TRUE), exception = FALSE,
            passed = if (facility_sample == "all") {
                ii <- rep(TRUE, nrow(d))
                max(abs(a[ii] - b[ii]), na.rm = TRUE) < 5.0001e-06
            }
            else TRUE)
    }))
    stopifnot(all(reconcile$passed))
    savecsv(reconcile, "join_supply_reconciliation.csv")
    audit$country <- "Lagos"
    audit$facility_sample <- facility_sample
    audit$demand_sample <- demand_sample
    audit$facility_records_used <- sum(fac$included & fac$geo %in% areas)
    audit$complete_facilities <- sum(fac$complete_supply & fac$geo %in% areas)
    audit$complete_demand_women <- sum(dn_complete)
    audit$supply_resolution <- "LGA only: each woman receives all included facilities averaged within her LGA"
    audit$counseling_included <- FALSE
    audit$positive_weight_rows <- sum(d$positive)
    ctx <- unique(cbind(d[c("geo", "district", "residence", "source_profile", "fallback")], sp))
    savecsv(profiles, "lga_supply_profiles.csv")
    savecsv(ctx, "assigned_supply_profiles.csv")
    saveRDS(list(women = d, supply = sp, profiles = profiles, assigned_profiles = ctx, facility_sample = facility_sample),
        file.path(out, "analysis_frame.rds"))
    payload <- list(audit = audit, demand_variables = D, demand_labels = DL, supply_variables = S, supply_labels = SL,
        bars = bars, rates = rates, heatmap = cells, area_metadata = unname(meta), scores = scores, thresholds = thresholds,
        component_trace = trace, planning = planning, coverage = coverage, profiles = profiles, assigned_profiles = ctx)
    jsonlite::write_json(payload, file.path(out, "report_data.json"), auto_unbox = TRUE, dataframe = "rows",
        pretty = FALSE, na = "null", digits = 12)
    jsonlite::write_json(audit, file.path(out, "analysis_audit.json"), auto_unbox = TRUE, pretty = TRUE,
        na = "null")
    cat("Built data:", nrow(dn), "women in need;", length(areas), "linked LGAs;", nrow(fac), "facility records.\n")
    print(bar_table[, c("field", "n", "estimate")], row.names = FALSE)
    print(thresholds, row.names = FALSE)
    print(table(scores$method, scores$category))
    payload <- export_risk_scores(payload, out)
}


# Fit models ------------------------------------------------------------------


fit_modern_use_models <- function(data_dir, output_dir, score_method, pca_score_file) {
    data_dir <- data_dir
    out <- output_dir
    score_method <- score_method
    pca_file <- pca_score_file
    stopifnot(score_method %in% c("zscore", "pca"))
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    frame <- readRDS(file.path(data_dir, "analysis_frame.rds"))
    d <- frame$women
    p <- jsonlite::fromJSON(file.path(data_dir, "report_data.json"))
    D <- p$demand_variables
    S <- p$supply_variables
    report_hash <- unname(tools::md5sum(file.path(data_dir, "report_data.json")))
    refs <- list()
    zz <- sapply(D, function(k) {
        ok <- d$need & is.finite(d[[k]])
        mu <- survey_weighted_mean(d[[k]][ok], d$weight[ok])
        sd <- sqrt(survey_weighted_mean((d[[k]][ok] - mu)^2, d$weight[ok]))
        stopifnot(sd > 0)
        refs[[k]] <<- data.frame(variable = k, weighted_mean = mu, weighted_SD = sd, n_valid_need = sum(ok))
        (d[[k]] - mu)/sd
    })
    d$demand_score <- rowMeans(zz)
    write.csv(do.call(rbind, refs), file.path(out, "individual_demand_score_reference.csv"), row.names = FALSE)
    d$supply_risk <- supply_constraint_score(frame$supply, p$component_trace, constraints_reference = identical(p$audit$supply_score_orientation,
        "constraints"))
    sref <- do.call(rbind, lapply(S, function(k) {
        q <- p$component_trace[p$component_trace$side == "Supply" & p$component_trace$variable == k,
            ]
        data.frame(variable = k, reference_mean_pct = unique(q$indicator_mean), reference_SD_pct = unique(q$indicator_SD),
            reference_LGAs = length(unique(q$geo)))
    }))
    write.csv(sref, file.path(out, "supply_score_reference.csv"), row.names = FALSE)
    if (score_method == "pca") {
        pp <- readRDS(pca_file)
        stopifnot(identical(pp$scores$row, d$row), pp$audit$report_md5 == report_hash)
        d$demand_score <- pp$scores$demand_score
        d$supply_risk <- pp$scores$supply_risk
    }
    resolution <- "lga"
    demand_resolution <- "individual"
    z <- p$scores[p$scores$method == "zscore", ]
    profiles <- unique(cbind(d[c("geo", "district", "source_profile", "fallback")], frame$supply, supply_risk = d$supply_risk))
    stopifnot(nrow(profiles) == 17, !anyDuplicated(profiles$geo))
    stopifnot(all(vapply(split(d$supply_risk, d$geo), function(v) length(unique(v)) == 1, logical(1))))
    write.csv(profiles, file.path(out, "model_supply_profiles.csv"), row.names = FALSE, na = "")
    controls <- c("age10", "age10_sq", "education", "partnered", "fertility_goal", "urban")
    specs <- list(score_demand = c(controls, "demand_score"), supply = c(controls, "supply_risk"), score_joint = c(controls,
        "demand_score", "supply_risk"), components_demand = c(controls, D), components_joint = c(controls,
        D, "supply_risk"))
    positive <- d$positive
    common_vars <- unique(c("y_modern", controls, D, "demand_score", "supply_risk"))
    d$eligible <- d$need & positive & complete.cases(d[common_vars])
    cc <- droplevels(d[d$eligible, ])
    stopifnot(all(cc$y_modern %in% 0:1), all(cc$need), length(unique(cc$geo)) == 17, all(cc$urban %in%
        0:1), length(unique(cc$partnered)) == 2)
    full_design <- surgo_design(d[positive, ])
    des <- subset(full_design, eligible)
    fit_survey_model <- function(fo, a) fit_survey_logistic(fo, a)
    coef_table <- function(f, V) {
        a <- as.data.frame(clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite"))
        lo <- a$beta - qt(0.975, a$df_Satt) * a$SE
        hi <- a$beta + qt(0.975, a$df_Satt) * a$SE
        data.frame(term = rownames(a), estimate = a$beta, se = a$SE, df = a$df_Satt, p = a$p_Satt, lo = lo,
            hi = hi, odds_ratio = exp(a$beta), or_lo = exp(lo), or_hi = exp(hi))
    }
    block_test <- function(f, V, fields) {
        ix <- match(fields, names(coef(f)))
        stopifnot(!anyNA(ix))
        K <- diag(length(coef(f)))[ix, , drop = FALSE]
        t <- clubSandwich::Wald_test(f, constraints = K, vcov = V, test = "HTZ")
        data.frame(F = t$Fstat, df_num = t$df_num, df_denom = t$df_denom, p = t$p_val)
    }
    vifs <- function(f, a) {
        X <- model.matrix(f)[, -1, drop = FALSE]
        do.call(rbind, lapply(seq_len(ncol(X)), function(j) {
            r <- lm.wfit(cbind(1, X[, -j, drop = FALSE]), X[, j], w = a$weight)
            total <- sum(a$weight * (X[, j] - survey_weighted_mean(X[, j], a$weight))^2)
            data.frame(term = colnames(X)[j], vif = 1/(sum(a$weight * r$residuals^2)/total))
        }))
    }
    fits <- coefficients <- samples <- tests <- diagnostics <- leaveout <- list()
    for (id in names(specs)) {
        fo <- reformulate(specs[[id]], "y_modern")
        f <- fit_survey_logistic(fo, cc, design = des)
        V <- geographic_cr2(f, cc$geo)
        a <- coef_table(f, V)
        a$model <- id
        coefficients[[id]] <- a
        vv <- vifs(f, cc)
        vv$model <- id
        diagnostics[[id]] <- vv
        for (block in c(if (id != "supply") "Demand", if (id %in% c("supply", "score_joint", "components_joint")) "Supply")) {
            fields <- if (block == "Supply")
                "supply_risk"
            else if (grepl("^score", id))
                "demand_score"
            else D
            t <- block_test(f, V, fields)
            t$model <- id
            t$block <- block
            tests[[length(tests) + 1L]] <- t
        }
        samples[[id]] <- data.frame(model = id, n = nrow(cc), events = sum(cc$y_modern), non_events = sum(cc$y_modern ==
            0), weighted_modern_use = survey_weighted_mean(cc$y_modern, cc$weight), districts = length(unique(cc$geo)),
            parameters = length(coef(f)), design_rank = f$rank, max_vif = max(vv$vif), minimum_df = min(a$df[a$term !=
                "(Intercept)"]), min_fitted = min(fitted(f)), max_fitted = max(fitted(f)), converged = f$converged,
            warnings = paste(attr(f, "fit_warnings"), collapse = " | "))
        fields <- intersect(c("demand_score", "supply_risk", D), names(coef(f)))
        for (g in sort(unique(cc$geo))) {
            ff <- fit_survey_model(fo, cc[cc$geo != g, ])
            leaveout[[length(leaveout) + 1L]] <- data.frame(model = id, omitted_LGA = g, term = fields,
                estimate = coef(ff)[fields], full_estimate = coef(f)[fields], row.names = NULL)
        }
        fits[[id]] <- list(fit = f, V = V, data = cc, formula = fo)
        cat(id, ": N =", nrow(cc), "; events =", sum(cc$y_modern), "; max VIF =", round(max(vv$vif),
            2), "\n")
    }
    bind <- function(a) do.call(rbind, a)
    coefficients <- bind(coefficients)
    samples <- bind(samples)
    tests <- bind(tests)
    tests$holm_p <- ave(tests$p, tests$model, FUN = function(v) p.adjust(v, "holm"))
    diagnostics <- bind(diagnostics)
    leaveout <- bind(leaveout)
    savecsv <- function(a, name) write.csv(a, file.path(out, name), row.names = FALSE, na = "")
    savecsv(coefficients, "model_coefficients.csv")
    savecsv(samples, "model_samples.csv")
    savecsv(tests, "model_block_tests.csv")
    savecsv(diagnostics, "model_vifs.csv")
    savecsv(leaveout, "leave_one_LGA_out.csv")
    missing <- data.frame(variable = common_vars, missing_in_need = vapply(d[d$need, common_vars], function(a) sum(is.na(a)),
        integer(1)))
    savecsv(missing, "model_missingness.csv")
    savecsv(z[, c("geo", "demand_score", "supply_score")], "model_LGA_scores.csv")
    sensitivity <- list()
    for (id in c("score_demand", "supply", "score_joint")) {
        available <- d$need & positive & complete.cases(d[all.vars(fits[[id]]$formula)])
        aa <- d[available, ]
        f <- fit_survey_model(fits[[id]]$formula, aa)
        V <- geographic_cr2(f, aa$geo)
        t <- coef_table(f, V)
        t$model <- id
        t$n <- nrow(aa)
        sensitivity[[id]] <- t
    }
    sensitivity <- bind(sensitivity)
    savecsv(sensitivity, "broader_score_sample_sensitivity.csv")
    Xmat <- function(f, a) model.matrix(delete.response(terms(f)), a, contrasts.arg = f$contrasts, xlev = f$xlevels)[,
        names(coef(f)), drop = FALSE]
    margin <- function(ob, changes = list()) {
        a <- ob$data
        for (k in names(changes)) a[[k]] <- changes[[k]]
        X <- Xmat(ob$fit, a)
        pp <- plogis(as.numeric(X %*% coef(ob$fit)))
        w <- a$weight/sum(a$weight)
        list(estimate = survey_weighted_mean(pp, a$weight), gradient = colSums(X * (w * pp * (1 - pp))))
    }
    ci <- function(ob, estimate, gradient, probability = FALSE) {
        g <- matrix(gradient, nrow = 1)
        lc <- as.data.frame(clubSandwich::linear_contrast(ob$fit, vcov = ob$V, contrasts = g, test = "Satterthwaite"))
        df <- as.numeric(lc[[4]][1])
        se <- sqrt(max(0, as.numeric(g %*% ob$V %*% t(g))))
        q <- qt(0.975, df)
        if (probability) {
            lo <- plogis(qlogis(estimate) - q * se/(estimate * (1 - estimate)))
            hi <- plogis(qlogis(estimate) + q * se/(estimate * (1 - estimate)))
        }
        else {
            lo <- estimate - q * se
            hi <- estimate + q * se
        }
        data.frame(estimate = estimate, se = se, df = df, lo = lo, hi = hi)
    }
    curves <- list()
    for (k in c("demand_score", "supply_risk")) {
        v <- if (k == "supply_risk")
            unique(cc[, c("geo", "residence", k)])[[k]]
        else unique(cc[[k]])
        grid <- seq(min(v), max(v), length.out = 41)
        for (id in c(if (k == "demand_score") "score_demand" else "supply", "score_joint")) for (value in grid) {
            ob <- fits[[id]]
            a <- margin(ob, setNames(list(value), k))
            t <- ci(ob, a$estimate, a$gradient, TRUE)
            t$model <- id
            t$term <- k
            t$value <- value
            curves[[length(curves) + 1L]] <- t
        }
    }
    curves <- bind(curves)
    savecsv(curves, "score_predicted_probabilities.csv")
    gaps <- list()
    max_prediction_error <- max_gradient_error <- 0
    for (k in D) {
        ob <- fits$components_joint
        low <- margin(ob, setNames(list(0), k))
        high <- margin(ob, setNames(list(1), k))
        grad <- high$gradient - low$gradient
        gap <- high$estimate - low$estimate
        t <- ci(ob, gap, grad)
        t$term <- k
        t$probability_absent <- low$estimate
        t$probability_present <- high$estimate
        gaps[[k]] <- t
        for (value in 0:1) {
            aa <- ob$data
            aa[[k]] <- value
            direct <- survey_weighted_mean(predict(ob$fit, newdata = aa, type = "response"), aa$weight)
            max_prediction_error <- max(max_prediction_error, abs(direct - if (value == 0) low$estimate else high$estimate))
        }
        a0 <- ob$data
        a1 <- ob$data
        a0[[k]] <- 0
        a1[[k]] <- 1
        X0 <- Xmat(ob$fit, a0)
        X1 <- Xmat(ob$fit, a1)
        b <- coef(ob$fit)
        w <- ob$data$weight/sum(ob$data$weight)
        for (j in unique(c(1, match(k, names(b))))) {
            up <- down <- b
            up[j] <- up[j] + 1e-06
            down[j] <- down[j] - 1e-06
            fun <- function(bb) sum(w * (plogis(as.numeric(X1 %*% bb)) - plogis(as.numeric(X0 %*% bb))))
            max_gradient_error <- max(max_gradient_error, abs((fun(up) - fun(down))/2e-06 - grad[j]))
        }
    }
    gaps <- bind(gaps)
    savecsv(gaps, "individual_demand_probability_gaps.csv")
    stopifnot(max_prediction_error < 1e-10, max_gradient_error < 1e-07)
    labels <- render_modern_use_plots(coefficients, curves, gaps, cc, p$demand_labels, D, out, score_method,
        demand_resolution, resolution, "lagos", supply_pca_mixed = score_method == "pca" && length(pp$audit$negative_supply_PC1_loadings) >
            0)
    influence <- do.call(rbind, lapply(split(leaveout, interaction(leaveout$model, leaveout$term, drop = TRUE)),
        function(a) data.frame(model = a$model[1], term = a$term[1], full_estimate = a$full_estimate[1],
            deletion_min = min(a$estimate), deletion_max = max(a$estimate), deletion_sign_changes = sum(sign(a$estimate) !=
                sign(a$full_estimate)))))
    savecsv(influence, "influence_summary.csv")
    saveRDS(fits, file.path(out, "fitted_models.rds"))
    audit <- list(regression_function = "survey::svyglm", design_ids = "~1", design_strata = "None",
        country = "Lagos", outcome = "Current modern contraceptive use only: confirmed modern=1; traditional-only or confirmed nonuse=0; unresolved status remains missing.",
        need_definition = "Original SURGO FPN2=1, unchanged. No reconstructed need flag or permanent-protection additions.",
        need_n = sum(d$need), common_n = nrow(cc), common_events = sum(cc$y_modern), score_method = score_method,
        supply_components = length(S), counseling_included = "counselor" %in% S, demand_resolution = "individual",
        supply_resolution = "LGA only: every woman receives the summary of all included facilities in her LGA",
        districts = length(unique(cc$geo)), survey_psus = length(unique(cc$psu)), demand_distinct_values = length(unique(cc$demand_score)),
        supply_distinct_values = length(unique(cc$supply_risk)), assigned_context_profiles = length(unique(cc$geo)),
        woman_residence_cells = nrow(unique(cc[c("geo", "residence")])), fallback_model_women = sum(cc$fallback),
        fallback_need_women = sum(d$fallback & d$need), women_residence_control = "settlement, retained regardless of facility source level",
        controls = controls, omitted_controls = "None of the agreed adjustment domains omitted; all model designs checked for full rank.",
        weights = "Original Lagos weight; common mean-one fitting rescaling. No facility weights.", inference = "Weighted logistic coefficients with LGA-clustered CR2 covariance, coefficient-specific Satterthwaite intervals, HTZ block tests and Holm adjustment within each joint model. No random effects or interactions.",
        design = "survey::svyglm with ids=~1, no strata or FPC, original SURGO weights; administrative-area CR2 covariance and Satterthwaite tests. Original sampling PSUs are not specified in this model.",
        score_definition = if (score_method == "zscore") "Demand: mean of six personal binary indicators standardized with item-specific weighted mean/population SD in the need group. Supply: negative mean of selected assigned LGA-wide favorable measures (stock continuity=1-burden) standardized against the fixed 17-LGA component references. Scores enter per one original average-z point." else "Separate weighted demand and supply PCAs; personal demand PC1 and negative assigned LGA-wide supply PC1. Mixed supply signs make it a pattern score, not a uniformly ordered risk index. Scores enter per one original PC1 unit.",
        supply_missing = "All 17 LGAs have complete aggregate supply inputs among Q506 Yes facilities. No zero recoding, geographic fallback or residence-specific supply.",
        excluded_need = sum(d$need) - nrow(cc), eligible_weight_share = survey_weighted_mean(as.numeric(d$eligible[d$need]),
            d$weight[d$need]), excluded_LGAs = setdiff(unique(d$geo[d$need]), unique(cc$geo)), uncertainty = "Intervals condition on estimated scores and do not propagate their construction uncertainty; full original sampling design is not available. Diagnostics are not a formal power analysis or causal identification.",
        probability_checks = list(maximum_direct_prediction_error = max_prediction_error, maximum_numeric_gradient_error = max_gradient_error),
        report_md5 = report_hash, source_hashes = p$audit$source_md5)
    payload <- list(audit = audit, coefficients = coefficients, samples = samples, tests = tests, vifs = diagnostics,
        curves = curves, gaps = gaps, influence = influence, labels = as.list(labels))
    jsonlite::write_json(payload, file.path(out, "model_results.json"), dataframe = "rows", auto_unbox = TRUE,
        digits = 12, na = "null")
    jsonlite::write_json(audit, file.path(out, "model_audit.json"), auto_unbox = TRUE, pretty = TRUE,
        digits = 12, na = "null")
    print(samples)
    print(tests)
    print(coefficients[coefficients$term %in% c("demand_score", "supply_risk"), ])
}


# Verify analysis -------------------------------------------------------------


validate_analysis <- function(data_dir, input_path, facility_path, raw_facility_path, raw_women_path, cleaned_facility_path) {
    data_dir <- data_dir
    input <- input_path
    facility_input <- facility_path
    raw_facility <- raw_facility_path
    raw_women <- raw_women_path
    cleaned <- cleaned_facility_path
    out <- file.path(data_dir, "checks")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    p <- jsonlite::fromJSON(file.path(data_dir, "report_data.json"))
    x <- readRDS(input)
    frame <- readRDS(file.path(data_dir, "analysis_frame.rds"))
    d <- frame$women
    D <- p$demand_variables
    S <- p$supply_variables
    checks <- list()
    verify <- function(name, ok) {
        if (!isTRUE(ok))
            stop("Verification failed: ", name)
        checks[[length(checks) + 1L]] <<- data.frame(check = name, passed = TRUE)
    }
    same <- function(a, b, tol = 1e-10) isTRUE(all.equal(as.vector(a), as.vector(b), check.attributes = FALSE,
        tolerance = tol))
    key <- function(v) {
        v <- toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
        v[v %in% "IFAKOIJAIYE"] <- "IFAKOIJAYE"
        v
    }
    valid_mean <- function(v) {
        v <- as.numeric(v)
        if (any(is.finite(v)))
            mean(v[is.finite(v)])
        else NA_real_
    }
    bool <- function(v) ifelse(tolower(v) %in% c("1", "true", "yes"), 1, ifelse(tolower(v) %in% c("0",
        "false", "no"), 0, NA_real_))
    raw <- readr::read_csv(raw_women, show_col_types = FALSE, guess_max = 20000, name_repair = "minimal",
        na = c("", "NA"))
    verify("Original 4555 women and IDs", nrow(x) == 4555 && identical(x$uid, raw$uid))
    for (k in names(raw)) verify(paste("Original SURGO field", k), same(x[[k]], raw[[k]], 0))
    verify("Original SURGO need status", identical(d$need, as.numeric(raw$fpn2) %in% 1) && sum(d$need) ==
        2159)
    verify("Original final woman weights", same(d$weight, raw$weight, 0))
    verify("Every bar uses all need women", all(p$bars$denominator == 2159))
    verify("Raw woman LGA is the join LGA", identical(key(raw$lga), d$geo) && identical(d$geo, key(x$link_geo_join)))
    agency <- ifelse(raw$hs7c %in% 1:2, as.numeric(raw$hs7c %in% 1), NA_real_)
    health <- ifelse(raw$pb2a %in% 1:4, as.numeric(raw$pb2a %in% 1:2), NA_real_)
    verify("Health uses PB2A only", same(d$health, health))
    verify("Agency uses general healthcare permission", same(d$agency, agency))
    for (k in c("stigma", "confidence")) {
        cols <- if (k == "stigma")
            c("pb7b", "pb7c")
        else c("mc5a", "mc5b")
        mat <- as.matrix(raw[cols])
        positive <- if (k == "stigma")
            mat %in% 1:2
        else mat %in% 3:4
        dim(positive) <- dim(mat)
        known <- mat %in% 1:4
        dim(known) <- dim(mat)
        expected <- ifelse(rowSums(positive) > 0, 1, ifelse(rowSums(known) == 2, 0, NA_real_))
        verify(paste("Raw demand domain", k), same(expected, d[[k]]))
    }
    mp <- as.matrix(raw[paste0("mp4_", letters[1:9])])
    sel <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
    none <- mp[, 9] %in% 1
    ok <- apply(mp, 1, function(v) all(v %in% 0:1)) & raw$mp4_99 %in% 0 & (sel | none) & !(sel & none)
    fert <- ifelse(ok, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) > 0), NA_real_)
    verify("Raw coherent fertility concerns", same(fert, d$fertility))
    verify("Raw opposition to FP", same(ifelse(raw$fp1 %in% 1:4, as.numeric(raw$fp1 %in% 3:4), NA_real_),
        d$opposes))
    f <- readRDS(facility_input)
    f <- f[f$country == "lagos", ]
    r <- haven::read_dta(raw_facility)
    ri <- match(f$fid, as.character(r$key))
    verify("Facility raw IDs one-to-one", !anyNA(ri) && !anyDuplicated(ri) && length(ri) == 1741)
    f$geo <- key(f$assigned_join_area)
    verify("Raw facility LGA assignments", identical(key(haven::as_factor(r$district_name[ri])), f$geo))
    fp <- as.numeric(r$q506[ri])
    verify("FP eligibility counts", sum(fp %in% 1) == 1040 && sum(fp %in% 2) == 465 && sum(is.na(fp)) ==
        236)
    areas <- sort(unique(d$geo[d$need]))
    verify("882 linked FP providers", sum(fp %in% 1 & f$geo %in% areas) == 882)
    exception <- f$geo == "IBEJULEKKI" & f$rurb %in% "Urban"
    verify("Previous zero exception excluded by FP eligibility", sum(exception) == 1 && fp[exception] ==
        2 && is.na(f$p_free_prod[exception]))
    q <- lapply(r[ri, grep("^q507a", names(r), value = TRUE)], as.numeric)
    collapse <- function(cols) vapply(seq_len(nrow(f)), function(i) {
        v <- vapply(cols, function(k) q[[k]][i], numeric(1))
        a <- v[v %in% 1:4]
        if (length(a))
            min(a)
        else if (all(v %in% 5))
            5
        else NA_real_
    }, numeric(1))
    m <- cbind(pills = q$q507ad, injectables = collapse(c("q507ag", "q507ah", "q507ai")), male_condoms = q$q507ae,
        female_condoms = q$q507af, ecp = q$q507ao, iud = collapse(c("q507aa", "q507ab")), implants = q$q507aj)
    den <- rowSums(m >= 1 & m <= 4, na.rm = TRUE)
    num <- rowSums(m == 1, na.rm = TRUE)
    daily <- ifelse(den > 0, num/den, NA_real_)
    daily_audit <- read.csv(file.path(data_dir, "daily_provision_facility_audit.csv"))
    verify("Independent daily-frequency formula", same(daily_audit$daily_provision, daily[match(daily_audit$fid,
        f$fid)]))
    f$offering <- bool(f$one_larc_3_sa_offer)
    f$free <- as.numeric(f$p_free_prod)
    f$training <- as.numeric(f$p_train)
    f$larc_stock <- bool(f$one_larc_stockin)
    f$stockout_burden <- as.numeric(f$p_any_stockout)
    f$daily_provision <- daily
    cl <- readr::read_csv(cleaned, show_col_types = FALSE)
    ci <- match(f$fid, as.character(cl$fid))
    verify("Stockout source coverage", !anyNA(ci))
    verify("Original cleaned stockout values preserved", same(f$stockout_burden, as.numeric(cl$p_any_stockout[ci])))
    inventory <- f
    f <- f[fp %in% 1, ]
    for (k in S) for (g in areas) {
        v <- f[[k]][f$geo == g]
        h <- p$heatmap[p$heatmap$side == "Supply" & p$heatmap$variable == k & p$heatmap$geo == g, ]
        verify(paste("FP-only facility mean", g, k), same(valid_mean(v) * 100, h$value))
        verify(paste("FP-only denominator", g, k), h$n_total == sum(f$geo == g) && h$n_valid == sum(is.finite(v)))
        verify(paste("Genuine zeros retained", g, k), same(h$numerator, sum(v[is.finite(v)])))
        verify(paste("LGA supply assigned to every resident", g, k), same(frame$supply[[k]][d$geo ==
            g], rep(valid_mean(v), sum(d$geo == g))))
    }
    for (k in D) for (g in areas) {
        ii <- d$need & d$geo == g & is.finite(d[[k]])
        h <- p$heatmap[p$heatmap$side == "Demand" & p$heatmap$variable == k & p$heatmap$geo == g, ]
        verify(paste("Need-only weighted concern", g, k), same(h$value, 100 * weighted.mean(d[[k]][ii],
            d$weight[ii])) && h$n_valid == sum(ii))
    }
    for (side in c("Demand", "Supply")) {
        fields <- if (side == "Demand")
            D
        else S
        mat <- sapply(fields, function(k) {
            a <- p$heatmap[p$heatmap$side == side & p$heatmap$variable == k, ]
            a$value[match(areas, a$geo)]
        })
        if (side == "Supply")
            mat[, "stockout_burden"] <- 100 - mat[, "stockout_burden"]
        zz <- scale(mat)
        expected <- rowMeans(zz)
        z <- p$scores[p$scores$method == "zscore", ]
        z <- z[match(areas, z$geo), ]
        observed <- if (side == "Demand")
            z$demand_score
        else -z$supply_score
        verify(paste("Independent mean z-score", side), same(expected, observed) && abs(mean(observed)) <
            1e-12)
        flag <- if (side == "Demand")
            expected > 1e-10
        else expected < -1e-10
        verify(paste("Zero-mean classification", side), identical(as.logical(flag), if (side == "Demand")
            z$demand_flag
        else z$supply_flag))
        for (k in fields) {
            t <- p$component_trace[p$component_trace$side == side & p$component_trace$variable == k,
                ]
            verify(paste("Aligned component z", side, k), same(if (side == "Supply")
                -t$zscore
            else t$zscore, zz[match(t$geo, areas), k]))
        }
    }
    cut <- p$thresholds[p$thresholds$method == "zscore", ]
    verify("Primary cutoffs zero", cut$demand_cutoff == 0 && cut$supply_cutoff == 0)
    z <- p$scores[p$scores$method == "zscore", ]
    verify("Classification categories", identical(z$category, ifelse(z$demand_flag & z$supply_flag, "both",
        ifelse(z$demand_flag, "demand", ifelse(z$supply_flag, "supply", "neither")))))
    previous <- NULL
    for (folder in c("models_zscore", "pca_appendix/models")) {
        models <- readRDS(file.path(data_dir, folder, "fitted_models.rds"))
        co <- read.csv(file.path(data_dir, folder, "model_coefficients.csv"))
        tt <- read.csv(file.path(data_dir, folder, "model_block_tests.csv"))
        for (id in names(models)) {
            ob <- models[[id]]
            aa <- ob$data
            fo <- ob$formula
            verify(paste(folder, id, "common women across all ten fits"), is.null(previous) || identical(previous,
                aa$row))
            previous <- aa$row
            verify(paste(folder, id, "modern use and original need"), all(aa$need) && all(aa$y_modern %in%
                0:1) && same(aa$y_modern, d$y_modern[aa$row]))
            verify(paste(folder, id, "agreed covariates"), all(c("age10", "age10_sq", "education", "partnered",
                "fertility_goal", "urban") %in% all.vars(fo)))
            verify(paste(folder, id, "no random effects or interactions"), !grepl("[|:*]", paste(deparse(fo),
                collapse = " ")))
            fit <- glm(fo, data = aa, weights = aa$weight/mean(aa$weight), family = quasibinomial(),
                control = glm.control(maxit = 100))
            verify(paste(folder, id, "weighted coefficients"), same(coef(fit), coef(ob$fit)))
            V <- clubSandwich::vcovCR(fit, cluster = aa$geo, type = "CR2", inverse_var = FALSE)
            verify(paste(folder, id, "LGA CR2 covariance"), same(as.vector(V), as.vector(ob$V)))
            for (block in tt$block[tt$model == id]) {
                fields <- if (block == "Supply")
                  "supply_risk"
                else if (grepl("^score", id))
                  "demand_score"
                else D
                K <- diag(length(coef(fit)))[match(fields, names(coef(fit))), , drop = FALSE]
                t <- clubSandwich::Wald_test(fit, constraints = K, vcov = V, test = "HTZ")
                saved <- tt[tt$model == id & tt$block == block, ]
                verify(paste(folder, id, block, "HTZ block test"), abs(t$p_val - saved$p) < 1e-10 &&
                  abs(t$df_denom - saved$df_denom) < 1e-09)
            }
        }
    }
    models <- readRDS(file.path(data_dir, "models_zscore/fitted_models.rds"))
    aa <- models$score_joint$data
    refs <- read.csv(file.path(data_dir, "models_zscore/individual_demand_score_reference.csv"))
    zz <- sapply(D, function(k) {
        v <- d[[k]]
        ii <- d$need & is.finite(v)
        mu <- weighted.mean(v[ii], d$weight[ii])
        sd <- sqrt(weighted.mean((v[ii] - mu)^2, d$weight[ii]))
        (v - mu)/sd
    })
    verify("Personal demand score from own six answers", same(aa$demand_score, rowMeans(zz)[aa$row]))
    context_score <- z$supply_score[match(aa$geo, z$geo)]
    verify("Supply risk is negative LGA readiness", same(aa$supply_risk, context_score))
    verify("17 distinct LGA supply profiles", length(unique(aa$supply_risk)) == 17)
    pca <- readRDS(file.path(data_dir, "pca_appendix/pca_scores.rds"))
    for (side in c("demand", "supply")) {
        fit <- pca[[paste0(side, "_fit")]]
        mat <- if (side == "demand")
            as.matrix(d[D])
        else as.matrix(frame$supply[S])
        if (side == "supply")
            mat[, "stockout_burden"] <- 1 - mat[, "stockout_burden"]
        ii <- d$need & complete.cases(mat)
        w <- d$weight[ii]/sum(d$weight[ii])
        mu <- colSums(mat[ii, , drop = FALSE] * w)
        sd <- sqrt(colSums(sweep(mat[ii, , drop = FALSE], 2, mu, "-")^2 * w))
        Z <- sweep(sweep(mat[ii, , drop = FALSE], 2, mu, "-"), 2, sd, "/")
        sv <- svd(Z * sqrt(w))
        loading <- sv$v[, 1]
        if (sum(loading * fit$rotation[, 1]) < 0)
            loading <- -loading
        verify(paste(side, "PCA weighted center"), same(mu, fit$center))
        verify(paste(side, "PCA weighted scale"), same(sd, fit$scale))
        verify(paste(side, "PCA independent weighted SVD"), same(loading, fit$rotation[, 1], 1e-08))
    }
    save <- do.call(rbind, checks)
    write.csv(save, file.path(out, "audit_checks.csv"), row.names = FALSE)
    jsonlite::write_json(list(all_passed = all(save$passed), check_count = nrow(save), source_rows = 4555,
        need_n = 2159, fp_facilities = 1040, linked_fp_facilities = 882, model_n = nrow(aa), models = 10,
        fp_providers_only = TRUE, health_PB2A_only = TRUE, primary_cutoffs_zero = TRUE, all_models_same_women = TRUE,
        no_random_effects = TRUE, limitations = "Implementation verification; not causal validation, full survey-design reconstruction or a formal power analysis."),
        file.path(out, "verification.json"), auto_unbox = TRUE, pretty = TRUE)
    cat("Passed", nrow(save), "independent checks.\n")
}


# Sensitivity analyses --------------------------------------------------------


# Alternative scores and final-score SD flags are saved with the area summaries.
# The following retains the existing PCA and regression sensitivities for this report.

calculate_pca_scores <- function(data_dir, output_dir) {
    data_dir <- data_dir
    out <- output_dir
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    report_file <- file.path(data_dir, "report_data.json")
    report <- jsonlite::fromJSON(report_file)
    report_hash <- unname(tools::md5sum(report_file))
    frame <- readRDS(file.path(data_dir, "analysis_frame.rds"))
    d <- frame$women
    D <- report$demand_variables
    S <- report$supply_variables
    eligible <- d$need
    dm <- as.matrix(d[D])
    sm0 <- frame$supply[S]
    sm0$stockout_burden <- 1 - sm0$stockout_burden
    sm <- as.matrix(sm0)
    di <- eligible & complete.cases(dm)
    si <- eligible & complete.cases(sm)
    weighted_pca <- function(m, weights, anchor = NULL) {
        w <- weights/sum(weights)
        center <- colSums(m * w)
        centered <- sweep(m, 2, center, "-")
        scale <- sqrt(colSums(centered^2 * w))
        stopifnot(all(scale > 0))
        Z <- sweep(centered, 2, scale, "/")
        R <- crossprod(Z, Z * w)
        ee <- eigen(R, symmetric = TRUE)
        rotation <- ee$vectors
        rownames(rotation) <- colnames(m)
        colnames(rotation) <- paste0("PC", 1:ncol(m))
        for (j in seq_len(ncol(rotation))) {
            orient <- if (j == 1 && !is.null(anchor))
                sum(rotation[, j] * anchor)
            else sum(rotation[, j])
            if (orient < 0)
                rotation[, j] <- -rotation[, j]
        }
        list(center = center, scale = scale, rotation = rotation, eigenvalues = ee$values, correlation = R,
            scores = Z %*% rotation)
    }
    project <- function(f, m) sweep(sweep(m, 2, f$center, "-"), 2, f$scale, "/") %*% f$rotation
    fd <- weighted_pca(dm[di, , drop = FALSE], d$weight[di])
    fs <- weighted_pca(sm[si, , drop = FALSE], d$weight[si])
    scores <- data.frame(row = d$row, demand_score = project(fd, dm)[, 1], supply_risk = -project(fs,
        sm)[, 1])
    stopifnot(all(is.finite(scores$demand_score[di])), all(is.finite(scores$supply_risk[si])))
    key <- d$source_profile
    profiles <- unique(data.frame(profile = key, geo = d$geo, sm, supply_PC1 = -scores$supply_risk, supply_risk = scores$supply_risk))
    stopifnot(!anyDuplicated(profiles$profile))
    profiles <- profiles[complete.cases(profiles[S]), ]
    profiles$women_in_need <- as.integer(table(factor(key[si], levels = profiles$profile)))
    profiles$SURGO_weight <- vapply(profiles$profile, function(k) sum(d$weight[si & key == k]), numeric(1))
    profiles$weight_share <- profiles$SURGO_weight/sum(profiles$SURGO_weight)
    collapsed <- weighted_pca(as.matrix(profiles[S]), profiles$SURGO_weight, fs$rotation[, 1])
    collapse_error <- max(abs(collapsed$rotation[, 1] - fs$rotation[, 1]), abs(collapsed$center - fs$center),
        abs(collapsed$scale - fs$scale))
    stopifnot(collapse_error < 1e-10)
    savecsv <- function(a, name) write.csv(a, file.path(out, name), row.names = FALSE, na = "")
    savecsv(profiles, "pca_matched_supply_profiles.csv")
    fits <- list(Demand = fd, Supply = fs)
    loads <- variance <- moments <- list()
    set.seed(20261106)
    B <- 2000L
    for (side in names(fits)) {
        f <- fits[[side]]
        fields <- if (side == "Demand")
            D
        else S
        for (j in seq_along(fields)) loads[[length(loads) + 1L]] <- data.frame(side = side, variable = fields,
            PC = j, loading = f$rotation[, j], squared_loading_share = f$rotation[, j]^2, variable_PC_correlation = f$rotation[,
                j] * sqrt(f$eigenvalues[j]))
        moments[[side]] <- data.frame(side = side, variable = fields, center = f$center, scale = f$scale)
        if (side == "Demand") {
            m <- dm[di, , drop = FALSE]
            ww <- d$weight[di]
            groups <- split(seq_len(nrow(m)), d$geo[di])
            null <- replicate(B, {
                perm <- m
                for (j in 1:6) for (ii in groups) perm[ii, j] <- sample(m[ii, j], length(ii), replace = FALSE)
                weighted_pca(perm, ww)$eigenvalues
            })
        }
        else {
            m <- as.matrix(profiles[S])
            ww <- profiles$SURGO_weight
            null <- replicate(B, {
                perm <- apply(m, 2, sample)
                weighted_pca(perm, ww)$eigenvalues
            })
        }
        cutoff <- apply(null, 1, quantile, probs = 0.95, type = 7, names = FALSE)
        variance[[side]] <- data.frame(side = side, PC = seq_along(fields), eigenvalue = f$eigenvalues,
            variance_pct = 100 * f$eigenvalues/sum(f$eigenvalues), cumulative_pct = 100 * cumsum(f$eigenvalues)/sum(f$eigenvalues),
            permutation_95 = cutoff, above_permutation_95 = f$eigenvalues > cutoff)
    }
    loads <- do.call(rbind, loads)
    variance <- do.call(rbind, variance)
    moments <- do.call(rbind, moments)
    savecsv(loads, "pca_loadings.csv")
    savecsv(variance, "pca_variance.csv")
    savecsv(moments, "pca_centers_scales.csv")
    savecsv(data.frame(variable = D, fd$correlation, check.names = FALSE), "pca_demand_correlations.csv")
    savecsv(data.frame(variable = S, fs$correlation, check.names = FALSE), "pca_supply_correlations.csv")
    stability <- list()
    for (g in sort(unique(d$geo[eligible]))) for (side in names(fits)) {
        m <- if (side == "Demand")
            dm
        else sm
        use <- if (side == "Demand")
            di
        else si
        fit <- fits[[side]]
        train <- use & d$geo != g
        f <- weighted_pca(m[train, , drop = FALSE], d$weight[train], fit$rotation[, 1])
        full_score <- project(fit, m[use, , drop = FALSE])[, 1]
        alt_score <- project(f, m[use, , drop = FALSE])[, 1]
        stability[[length(stability) + 1L]] <- data.frame(side = side, omitted_LGA = g, loading_cosine = sum(f$rotation[,
            1] * fit$rotation[, 1]), score_rank_correlation = cor(full_score, alt_score, method = "spearman"),
            pc1_variance_pct = 100 * f$eigenvalues[1]/sum(f$eigenvalues))
    }
    stability <- do.call(rbind, stability)
    savecsv(stability, "pca_leave_one_LGA_out.csv")
    pc1 <- loads[loads$PC == 1, ]
    audit <- list(method = "Separate final-weighted correlation PCAs on individual demand and assigned LGA-wide supply; PC1 prespecified as sensitivity.",
        need_n = sum(eligible), demand_PCA_n = sum(di), supply_PCA_women_n = sum(si), supply_profiles = nrow(profiles),
        demand_PC1_variance = variance$variance_pct[variance$side == "Demand" & variance$PC == 1], supply_PC1_variance = variance$variance_pct[variance$side ==
            "Supply" & variance$PC == 1], negative_demand_PC1_loadings = pc1$variable[pc1$side == "Demand" &
            pc1$loading < 0], negative_supply_PC1_loadings = pc1$variable[pc1$side == "Supply" & pc1$loading <
            0], supply_collapse_equivalence_error = collapse_error, report_md5 = report_hash, weighting = "SURGO weight on both sides. Supply PCA is equivalent to assigned LGA-wide profiles weighted by summed weights of represented women; these are not facility weights.",
        missingness = "Require all six demand and all selected supply indicators. No new zero recoding, partial scores or geographic fallback. Only Q506 Yes facilities contribute; no undefined facility value is zero-filled.",
        alignment = "Stockout burden becomes continuity=1-burden before PCA; all supply inputs favorable.",
        sign = "Demand PC1 oriented toward average concern; supply PC1 toward average favorable provision and negated for modeling. Mixed signs remain and are explicitly reported.",
        uncertainty = "Permutation and LGA-deletion checks assess exploratory construction stability, not validated survey-design inference. Regression intervals condition on estimated scores.",
        source_unchanged = report_hash == unname(tools::md5sum(report_file)))
    obj <- list(audit = audit, scores = scores, demand_fit = fd, supply_fit = fs, profiles = profiles,
        loadings = loads, variance = variance, moments = moments, stability = stability)
    saveRDS(obj, file.path(out, "pca_scores.rds"))
    jsonlite::write_json(obj[setdiff(names(obj), c("scores", "demand_fit", "supply_fit"))], file.path(out,
        "pca_results.json"), dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 12)
    jsonlite::write_json(audit, file.path(out, "pca_audit.json"), auto_unbox = TRUE, pretty = TRUE, digits = 12)
    labels <- setNames(c(report$demand_labels, report$supply_labels), c(D, S))
    labels["stockout_burden"] <- "Stock continuity (1 - burden)"
    base <- ggplot2::theme_minimal(base_family = "sans", base_size = 11) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold",
            size = 18, color = "#17374a"), plot.subtitle = ggplot2::element_text(size = 10, color = "#607985",
            lineheight = 1.2), plot.caption = ggplot2::element_text(hjust = 0, color = "#607985", size = 9),
        strip.background = ggplot2::element_rect(fill = "#edf4f6", color = NA), strip.text = ggplot2::element_text(face = "bold",
            color = "#17374a"), panel.spacing = grid::unit(1.5, "lines"), legend.position = "bottom",
        plot.margin = ggplot2::margin(14, 18, 12, 12))
    saveplot <- function(g, name, height) {
        ggplot2::ggsave(file.path(out, paste0(name, ".png")), g, width = 14, height = height, dpi = 170,
            bg = "white")
        ggplot2::ggsave(file.path(out, paste0(name, ".svg")), g, width = 14, height = height, device = svglite::svglite,
            bg = "white")
    }
    pc1$row <- factor(pc1$variable, levels = rev(c(D, S)))
    pc1$direction <- ifelse(pc1$loading < 0, "Negative coefficient", pc1$side)
    pc1$panel <- ifelse(pc1$side == "Demand", sprintf("Individual demand PC1 · %.1f%% of variation",
        audit$demand_PC1_variance), sprintf(if (length(audit$negative_supply_PC1_loadings) > 0)
        "Supply pattern PC1 · %.1f%% of variation"
    else "Favorable supply PC1 · %.1f%% of variation", audit$supply_PC1_variance))
    pc1$panel <- factor(pc1$panel, levels = c(sprintf("Individual demand PC1 · %.1f%% of variation",
        audit$demand_PC1_variance), sprintf(if (length(audit$negative_supply_PC1_loadings) > 0) "Supply pattern PC1 · %.1f%% of variation" else "Favorable supply PC1 · %.1f%% of variation",
        audit$supply_PC1_variance)))
    g <- ggplot2::ggplot(pc1, ggplot2::aes(loading, row, fill = direction)) + ggplot2::geom_vline(xintercept = 0,
        color = "#8da2ad") + ggplot2::geom_col(width = 0.55) + ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.2f",
        loading), hjust = ifelse(loading < 0, 1.15, -0.15)), size = 3.5) + ggplot2::facet_wrap(~panel,
        nrow = 1, scales = "free_y") + ggplot2::scale_y_discrete(labels = labels) + ggplot2::scale_fill_manual(values = c(Demand = "#087f8a",
        Supply = "#be6b23", `Negative coefficient` = "#8b99a3")) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
        ggplot2::labs(title = "What the PCA scores emphasize", subtitle = "Signed coefficients on standardized inputs; demand uses individual responses, supply uses matched LGA-wide exposures",
            x = "PC1 scoring coefficient (not an association with contraceptive use)", y = NULL, fill = NULL,
            caption = "Supply coefficients show PC1 before the model sign reversal; mixed signs indicate a supply pattern. Inspect signed coefficients: a negative value indicates a contrast within PC1.\nWeights describe variation in the inputs, not program importance or causal effects.") +
        base
    saveplot(g, "01_pca_loadings", 6)
    g <- ggplot2::ggplot(variance, ggplot2::aes(PC, eigenvalue, fill = side)) + ggplot2::geom_col(width = 0.55) +
        ggplot2::geom_line(ggplot2::aes(y = permutation_95), color = "#718997", linewidth = 0.7) + ggplot2::geom_point(ggplot2::aes(y = permutation_95),
        color = "#718997", size = 1.5) + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", variance_pct)),
        vjust = -0.4, size = 3.5) + ggplot2::facet_wrap(~side, nrow = 1) + ggplot2::scale_x_continuous(breaks = 1:6) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) + ggplot2::scale_fill_manual(values = c(Demand = "#087f8a",
        Supply = "#be6b23"), guide = "none") + ggplot2::labs(title = "How much of each side's variation does PCA retain?",
        subtitle = "Bars: observed eigenvalues and variance shares · gray line: exploratory permutation 95th percentile",
        x = "Principal component", y = "Eigenvalue", caption = "2,000 permutations per side. Null constructions differ by level; these are descriptive dimensionality diagnostics, not survey-design tests.") +
        base
    saveplot(g, "02_pca_variance", 5.5)
    print(audit)
    print(pc1[, c("side", "variable", "loading")])
    print(variance)
}


run_complete_case_sensitivities <- function(data_dir, output_dir) {
    lagos_sensitivity_controls <- c("age10", "age10_sq", "education", "partnered", "fertility_goal",
        "urban")
    lagos_response_probabilities <- function(base) {
        stopifnot(all(complete.cases(base[c("responder", "y_modern", lagos_sensitivity_controls, "geo",
            "weight")])))
        groups <- split(base$responder, base$geo)
        stopifnot(all(vapply(groups, function(x) any(x == 1), logical(1))))
        complete_areas <- names(groups)[vapply(groups, function(x) all(x == 1), logical(1))]
        partial <- !base$geo %in% complete_areas
        probability <- rep(1, nrow(base))
        fit <- NULL
        if (any(partial)) {
            a <- droplevels(base[partial, ])
            a$response_area <- factor(a$geo)
            formula <- reformulate(c("y_modern", lagos_sensitivity_controls, "response_area"), "responder")
            fit <- fit_survey_logistic(formula, a)
            probability[partial] <- as.numeric(predict(fit, newdata = a, type = "response"))
        }
        stopifnot(all(is.finite(probability)), all(probability > 0), all(probability <= 1))
        list(probability = probability, fit = fit, complete_areas = complete_areas)
    }
    lagos_jackknife_covariance <- function(estimates) {
        G <- nrow(estimates)
        centered <- sweep(estimates, 2, colMeans(estimates), "-")
        crossprod(centered) * (G - 1)/G
    }
    build_lagos_regression_sensitivity <- function(data_dir, out) {
        dir.create(out, recursive = TRUE, showWarnings = FALSE)
        frame <- readRDS(file.path(data_dir, "analysis_frame.rds"))
        d <- frame$women
        p <- jsonlite::fromJSON(file.path(data_dir, "report_data.json"))
        D <- p$demand_variables
        stopifnot(p$audit$need_n == 2159, p$audit$supply_score_orientation == "constraints")
        original <- readRDS(file.path(data_dir, "models_zscore/fitted_models.rds"))
        refs <- read.csv(file.path(data_dir, "models_zscore/individual_demand_score_reference.csv"))
        d$demand_score <- rowMeans(sapply(D, function(k) {
            r <- refs[refs$variable == k, ]
            (d[[k]] - r$weighted_mean)/r$weighted_SD
        }))
        d$supply_risk <- supply_constraint_score(frame$supply, p$component_trace, TRUE)
        controls <- lagos_sensitivity_controls
        common_vars <- c("y_modern", controls, D, "demand_score", "supply_risk")
        d$common <- d$need & d$positive & complete.cases(d[common_vars])
        d$base <- d$need & d$positive & complete.cases(d[c("y_modern", controls, "supply_risk")])
        stopifnot(identical(d$row[d$common], original$score_joint$data$row), sum(d$common) == 1896, sum(d$base) ==
            2132)
        stopifnot(all(vapply(split(d$supply_risk, d$geo), function(v) length(unique(v)) == 1, logical(1))))
        source_files <- c(file.path(data_dir, "analysis_frame.rds"), file.path(data_dir, "report_data.json"),
            file.path(data_dir, "models_zscore/fitted_models.rds"))
        source_hash <- unname(tools::md5sum(source_files))
        savecsv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
        wm <- function(x, w) {
            ii <- is.finite(x)
            if (any(ii))
                survey_weighted_mean(x[ii], w[ii])
            else NA_real_
        }
        full_design <- surgo_design(d[d$positive, ])
        cr2_table <- function(f, a) {
            V <- geographic_cr2(f, a$geo)
            ct <- as.data.frame(clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite"))
            data.frame(term = rownames(ct), estimate = ct$beta, se = ct$SE, df = ct$df_Satt, p = ct$p_Satt,
                odds_ratio = exp(ct$beta), or_lo = exp(ct$beta - qt(0.975, ct$df_Satt) * ct$SE), or_hi = exp(ct$beta +
                  qt(0.975, ct$df_Satt) * ct$SE))
        }
        groups <- list(Included = d$need & d$common, Excluded = d$need & !d$common)
        overview <- do.call(rbind, lapply(names(groups), function(k) {
            a <- d[groups[[k]], ]
            data.frame(group = k, n = nrow(a), survey_weight = sum(a$weight), known_outcomes = sum(is.finite(a$y_modern)),
                modern_use_pct = 100 * wm(a$y_modern, a$weight), mean_age = 30 + 10 * wm(a$age10, a$weight),
                urban_pct = 100 * wm(a$urban, a$weight), partnered_pct = 100 * wm(a$partnered, a$weight))
        }))
        categories <- do.call(rbind, lapply(c("education", "fertility_goal"), function(k) do.call(rbind,
            lapply(names(groups), function(g) {
                a <- d[groups[[g]], ]
                v <- as.character(a[[k]])
                v[is.na(v)] <- "Unresolved"
                do.call(rbind, lapply(sort(unique(v)), function(level) data.frame(variable = k, group = g,
                  level = level, n = sum(v == level), weighted_pct = 100 * survey_weighted_mean(as.numeric(v ==
                    level), a$weight))))
            }))))
        missingness <- do.call(rbind, lapply(c("y_modern", controls, D, "supply_risk"), function(k) {
            a <- d[d$need, ]
            data.frame(variable = k, n_missing = sum(is.na(a[[k]])), total = nrow(a), weighted_missing_pct = 100 *
                survey_weighted_mean(as.numeric(is.na(a[[k]])), a$weight))
        }))
        area_coverage <- do.call(rbind, lapply(sort(unique(d$geo[d$need])), function(g) {
            a <- d[d$need & d$geo == g, ]
            data.frame(geo = g, lga = a$district[1], need_n = nrow(a), common_n = sum(a$common), excluded_n = sum(!a$common),
                excluded_weighted_pct = 100 * survey_weighted_mean(as.numeric(!a$common), a$weight),
                broader_base_n = sum(a$base))
        }))
        savecsv(overview, "included_excluded_overview.csv")
        savecsv(categories, "included_excluded_categories.csv")
        savecsv(missingness, "exclusion_drivers.csv")
        savecsv(area_coverage, "exclusions_by_lga.csv")
        formulas <- c(list(supply = original$supply$formula), setNames(lapply(D, function(k) reformulate(c(controls,
            k, "supply_risk"), "y_modern")), paste0("single_", D)))
        coefficients <- samples <- fits <- list()
        for (id in names(formulas)) for (version in c("common", "available")) {
            formula <- formulas[[id]]
            eligible <- if (version == "common")
                d$common
            else d$need & d$positive & complete.cases(d[all.vars(formula)])
            fd <- full_design
            fd$variables$.sensitivity_eligible <- eligible[d$positive]
            des <- subset(fd, .sensitivity_eligible)
            a <- droplevels(d[eligible, ])
            fit <- fit_survey_logistic(formula, a, des)
            co <- cr2_table(fit, a)
            co$model <- id
            co$sample <- version
            coefficients[[paste(id, version)]] <- co
            samples[[paste(id, version)]] <- data.frame(model = id, sample = version, n = nrow(a), events = sum(a$y_modern),
                lgas = length(unique(a$geo)), weighted_modern_use = wm(a$y_modern, a$weight))
            fits[[paste(id, version)]] <- list(fit = fit, data = a, formula = formula)
            if (id == "supply" && version == "common")
                stopifnot(max(abs(coef(fit) - coef(original$supply$fit))) < 1e-09, max(abs(geographic_cr2(fit,
                  a$geo) - original$supply$V)) < 1e-09)
        }
        coefficients <- do.call(rbind, coefficients)
        samples <- do.call(rbind, samples)
        savecsv(coefficients, "available_case_coefficients.csv")
        savecsv(samples, "available_case_samples.csv")
        base <- droplevels(d[d$base, ])
        base$responder <- as.numeric(base$common)
        response <- lagos_response_probabilities(base)
        base$response_probability <- response$probability
        base$response_adjustment <- 1/base$response_probability
        joint_specs <- lapply(original[c("score_joint", "components_joint")], `[[`, "formula")
        fit_joint <- function(a, response_weighted, formula) {
            cc <- droplevels(a[a$responder == 1, ])
            if (response_weighted)
                cc$weight <- cc$weight * cc$response_adjustment
            fit_survey_logistic(formula, cc)
        }
        joint_fits <- list()
        joint_points <- list()
        replicates <- list()
        for (id in names(joint_specs)) for (version in c("survey_only", "response_weighted")) {
            key <- paste(id, version)
            f <- fit_joint(base, version == "response_weighted", joint_specs[[id]])
            joint_fits[[key]] <- f
            joint_points[[key]] <- coef(f)
            if (version == "survey_only")
                stopifnot(max(abs(coef(f) - coef(original[[id]]$fit))) < 1e-09)
        }
        areas <- sort(unique(base$geo))
        G <- length(areas)
        stopifnot(G == 17)
        for (g in areas) {
            a <- droplevels(base[base$geo != g, ])
            rr <- lagos_response_probabilities(a)
            a$response_probability <- rr$probability
            a$response_adjustment <- 1/rr$probability
            for (id in names(joint_specs)) for (version in c("survey_only", "response_weighted")) {
                key <- paste(id, version)
                f <- fit_joint(a, version == "response_weighted", joint_specs[[id]])
                stopifnot(identical(names(coef(f)), names(joint_points[[key]])))
                replicates[[paste(key, g)]] <- data.frame(model = id, weighting = version, omitted_lga = g,
                  term = names(coef(f)), estimate = unname(coef(f)))
            }
            cat("Completed sensitivity jackknife:", g, "\n")
        }
        replicates <- do.call(rbind, replicates)
        joint <- list()
        for (id in names(joint_specs)) for (version in c("survey_only", "response_weighted")) {
            key <- paste(id, version)
            b <- joint_points[[key]]
            r <- replicates[replicates$model == id & replicates$weighting == version, ]
            B <- sapply(names(b), function(term) r$estimate[match(paste(areas, term), paste(r$omitted_lga,
                r$term))])
            stopifnot(all(is.finite(B)))
            V <- lagos_jackknife_covariance(B)
            se <- sqrt(diag(V))
            df <- G - 1
            joint[[key]] <- data.frame(model = id, weighting = version, term = names(b), estimate = unname(b),
                se = se, df = df, p = 2 * pt(-abs(b/se), df), odds_ratio = exp(b), or_lo = exp(b - qt(0.975,
                  df) * se), or_hi = exp(b + qt(0.975, df) * se), n = sum(base$responder == 1))
        }
        joint <- do.call(rbind, joint)
        savecsv(joint, "response_weighted_joint_coefficients.csv")
        savecsv(replicates, "response_weighted_lga_jackknife.csv")
        balance_matrix <- cbind(modern_use = base$y_modern, age_years = 30 + 10 * base$age10, urban = base$urban,
            partnered = base$partnered, supply_risk = base$supply_risk, model.matrix(~education + fertility_goal +
                factor(geo) - 1, base))
        complete <- base$responder == 1
        balance <- do.call(rbind, lapply(colnames(balance_matrix), function(k) {
            v <- balance_matrix[, k]
            target <- wm(v, base$weight)
            sd <- sqrt(wm((v - target)^2, base$weight))
            before <- wm(v[complete], base$weight[complete])
            after <- wm(v[complete], base$weight[complete] * base$response_adjustment[complete])
            data.frame(variable = k, target_mean = target, common_mean = before, response_weighted_mean = after,
                standardized_difference_before = if (sd > 0)
                  (before - target)/sd
                else 0, standardized_difference_after = if (sd > 0)
                  (after - target)/sd
                else 0)
        }))
        savecsv(balance, "response_weighting_balance.csv")
        adjusted <- base$weight[complete] * base$response_adjustment[complete]
        kish <- function(w) sum(w)^2/sum(w^2)
        diagnostics <- list(target_n = nrow(base), complete_n = sum(complete), lgas = G, fully_observed_lgas = response$complete_areas,
            response_probability_min = min(base$response_probability), response_probability_max = max(base$response_probability),
            response_adjustment_min = min(base$response_adjustment[complete]), response_adjustment_median = median(base$response_adjustment[complete]),
            response_adjustment_p99 = unname(quantile(base$response_adjustment[complete], 0.99)), response_adjustment_max = max(base$response_adjustment[complete]),
            kish_effective_n_original = kish(base$weight[complete]), kish_effective_n_adjusted = kish(adjusted),
            maximum_absolute_balance_difference_before = max(abs(balance$standardized_difference_before)),
            maximum_absolute_balance_difference_after = max(abs(balance$standardized_difference_after)),
            response_model_formula = "Complete demand information ~ modern use + age + age squared + education + partnership + fertility goal + urban residence + LGA fixed indicators, estimated in LGAs with both complete and incomplete records; probability 1 in fully observed LGAs.",
            response_weight_definition = "Original SURGO weight / estimated probability of complete demand information. No trimming or imputation.",
            inference = "For response-weighted joint comparisons, delete-one-LGA jackknife refits the response and outcome models; variance=(G-1)/G times centered replicate crossproducts; t intervals use G-1=16 df. The survey-only joint comparison uses the same jackknife; primary report inference remains CR2/Satterthwaite.")
        audit <- list(country = "Lagos", supply_resolution = "LGA only", need_n = sum(d$need), common_n = sum(d$common),
            excluded_n = sum(d$need & !d$common), base_n = sum(d$base), regression_function = "survey::svyglm",
            design_ids = "~1", design_strata = "None", weights = "Original Lagos SURGO weight; response adjustments only in the named sensitivity fits",
            controls = controls, demand_references_fixed = TRUE, supply_references_fixed = TRUE, main_results_unchanged = TRUE,
            source_files_unchanged = identical(source_hash, unname(tools::md5sum(source_files))), uncertainty = "Response weighting requires the completeness model to account adequately for selection using observed information. It cannot establish robustness to selection on unrecorded concerns. Jackknife inference has only 17 geographic clusters and conditions on fixed score references.")
        stopifnot(audit$source_files_unchanged)
        payload <- list(audit = audit, overview = overview, categories = categories, missingness = missingness,
            area_coverage = area_coverage, available_coefficients = coefficients, available_samples = samples,
            joint_coefficients = joint, weight_diagnostics = diagnostics, balance = balance, demand_variables = D,
            demand_labels = p$demand_labels)
        jsonlite::write_json(payload, file.path(out, "sensitivity_results.json"), auto_unbox = TRUE,
            pretty = TRUE, dataframe = "rows", na = "null", digits = 12)
        saveRDS(list(data = d, base = base, response = response, available_fits = fits, joint_fits = joint_fits,
            replicates = replicates), file.path(out, "sensitivity_fits.rds"))
        cat("Built Lagos aggregate sensitivity checks with", sum(d$common), "common and", sum(d$base),
            "broader eligible women.\n")
        invisible(payload)
    }
    {
        build_lagos_regression_sensitivity(data_dir, output_dir)
    }
}


validate_complete_case_sensitivities <- function(data_dir, sensitivity_dir) {
    data_dir <- data_dir
    out <- sensitivity_dir
    frame <- readRDS(file.path(data_dir, "analysis_frame.rds"))
    d <- frame$women
    p <- jsonlite::fromJSON(file.path(out, "sensitivity_results.json"))
    ob <- readRDS(file.path(out, "sensitivity_fits.rds"))
    original <- readRDS(file.path(data_dir, "models_zscore/fitted_models.rds"))
    D <- p$demand_variables
    controls <- p$audit$controls
    checks <- list()
    check <- function(name, ok) {
        if (!isTRUE(ok))
            stop(name)
        checks[[length(checks) + 1L]] <<- data.frame(check = name, passed = TRUE)
    }
    same <- function(a, b, tol = 1e-08) isTRUE(all.equal(as.vector(a), as.vector(b), check.attributes = FALSE,
        tolerance = tol))
    check("Original survey variables preserved", all(vapply(names(d), function(k) same(d[[k]], ob$data[[k]],
        0), logical(1))))
    base <- d$need & d$positive & complete.cases(d[c("y_modern", controls)])
    check("Broader target independently selected", identical(d$row[base], ob$base$row) && sum(base) ==
        2132)
    common <- base & complete.cases(d[D])
    check("Common model sample independently selected", identical(d$row[common], original$score_joint$data$row) &&
        sum(common) == 1896)
    for (model in unique(p$available_samples$model)) for (version in c("common", "available")) {
        key <- paste(model, version)
        a <- ob$available_fits[[key]]
        needed <- all.vars(a$formula)
        ii <- if (version == "common")
            common
        else ob$data$need & ob$data$positive & complete.cases(ob$data[needed])
        check(paste(model, version, "exact rows"), identical(a$data$row, d$row[ii]))
        check(paste(model, version, "need and modern outcome"), all(a$data$need) && all(a$data$y_modern %in%
            0:1))
        check(paste(model, version, "unchanged survey weights"), same(a$data$weight, d$weight[ii], 0))
        check_data <- a$data
        check_data$.verification_weight <- check_data$weight/mean(check_data$weight)
        f <- glm(a$formula, data = check_data, weights = .verification_weight, family = quasibinomial(),
            control = glm.control(maxit = 100))
        check(paste(model, version, "independent weighted coefficients"), same(coef(f), coef(a$fit)))
        V <- clubSandwich::vcovCR(f, cluster = a$data$geo, type = "CR2", inverse_var = FALSE)
        ct <- as.data.frame(clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite"))
        co <- p$available_coefficients[p$available_coefficients$model == model & p$available_coefficients$sample ==
            version, ]
        co <- co[match(rownames(ct), co$term), ]
        check(paste(model, version, "LGA CR2 Satterthwaite intervals"), same(co$or_lo, exp(ct$beta -
            qt(0.975, ct$df_Satt) * ct$SE)) && same(co$or_hi, exp(ct$beta + qt(0.975, ct$df_Satt) * ct$SE)))
    }
    for (group in c("Included", "Excluded")) {
        ii <- if (group == "Included")
            common
        else d$need & !common
        r <- p$overview[p$overview$group == group, ]
        known <- ii & is.finite(d$y_modern)
        check(paste(group, "weighted modern use and known denominator"), same(r$modern_use_pct, 100 *
            weighted.mean(d$y_modern[known], d$weight[known])) && r$known_outcomes == sum(known))
        check(paste(group, "age units independently recovered"), same(r$mean_age, weighted.mean(30 +
            10 * d$age10[ii], d$weight[ii])))
    }
    response_probability <- function(a) {
        sums <- tapply(a$responder, a$geo, sum)
        counts <- table(a$geo)
        complete_areas <- names(sums)[sums == as.numeric(counts[names(sums)])]
        ii <- !a$geo %in% complete_areas
        probability <- rep(1, nrow(a))
        if (any(ii)) {
            b <- droplevels(a[ii, ])
            b$response_area <- factor(b$geo)
            b$.verification_weight <- b$weight/mean(b$weight)
            f <- glm(reformulate(c("y_modern", controls, "response_area"), "responder"), data = b, weights = .verification_weight,
                family = quasibinomial(), control = glm.control(maxit = 100))
            probability[ii] <- predict(f, type = "response")
        }
        probability
    }
    b <- ob$base
    probability <- response_probability(b)
    check("Independent response probabilities", same(probability, b$response_probability))
    check("Positive response probabilities and inverse adjustments", all(probability > 0 & probability <=
        1) && same(b$response_adjustment, 1/probability))
    check("Fully observed LGAs require no response adjustment", setequal(p$weight_diagnostics$fully_observed_lgas,
        c("ETIOSA", "IKEJA")) && all(b$response_adjustment[b$geo %in% c("ETIOSA", "IKEJA")] == 1))
    independent_joint <- function(a, weighted, formula) {
        probability <- response_probability(a)
        ii <- a$responder == 1
        w <- a$weight[ii]
        if (weighted)
            w <- w/probability[ii]
        check_data <- a[ii, ]
        check_data$.verification_weight <- w/mean(w)
        glm(formula, data = check_data, weights = .verification_weight, family = quasibinomial(), control = glm.control(maxit = 100))
    }
    for (model in c("score_joint", "components_joint")) for (version in c("survey_only", "response_weighted")) {
        fo <- original[[model]]$formula
        key <- paste(model, version)
        f <- independent_joint(b, version == "response_weighted", fo)
        check(paste(key, "independent full-fit coefficients"), same(coef(f), coef(ob$joint_fits[[key]])))
        areas <- sort(unique(b$geo))
        B <- matrix(NA_real_, length(areas), length(coef(f)), dimnames = list(areas, names(coef(f))))
        for (g in areas) {
            a <- droplevels(b[b$geo != g, ])
            rep <- independent_joint(a, version == "response_weighted", fo)
            B[g, ] <- coef(rep)
            saved <- ob$replicates[ob$replicates$model == model & ob$replicates$weighting == version &
                ob$replicates$omitted_lga == g, ]
            check(paste(key, g, "refit both stages"), same(coef(rep), saved$estimate[match(names(coef(rep)),
                saved$term)]))
        }
        V <- matrix(0, ncol(B), ncol(B))
        mu <- colMeans(B)
        for (j in seq_len(nrow(B))) V <- V + tcrossprod(B[j, ] - mu)
        V <- V * (length(areas) - 1)/length(areas)
        se <- sqrt(diag(V))
        co <- p$joint_coefficients[p$joint_coefficients$model == model & p$joint_coefficients$weighting ==
            version, ]
        co <- co[match(names(coef(f)), co$term), ]
        check(paste(key, "independent jackknife variance"), same(co$se, se))
        check(paste(key, "17-LGA t16 intervals"), all(co$df == 16) && same(co$or_lo, exp(coef(f) - qt(0.975,
            16) * se)) && same(co$or_hi, exp(coef(f) + qt(0.975, 16) * se)))
    }
    check("Score units fixed to main models", same(ob$data$demand_score[common], original$score_joint$data$demand_score) &&
        same(ob$data$supply_risk[common], original$score_joint$data$supply_risk))
    check("Whole-LGA supply assignment retained", all(vapply(split(ob$data$supply_risk, ob$data$geo),
        function(x) length(unique(x)) == 1, logical(1))))
    result <- list(all_passed = TRUE, check_count = length(checks), country = "Lagos", supply_resolution = "LGA only",
        need_n = 2159, main_n = 1896, supply_available_n = 2132, primary_results_unchanged = TRUE, limitations = "Implementation checks do not validate the untestable response-selection assumption or reconstruct the original sampling design.")
    write.csv(do.call(rbind, checks), file.path(out, "verification_checks.csv"), row.names = FALSE)
    jsonlite::write_json(result, file.path(out, "verification.json"), auto_unbox = TRUE, pretty = TRUE)
    cat("Passed", length(checks), "independent regression-sensitivity checks.\n")
}


plot_complete_case_sensitivities <- function(data_dir) {
    out <- data_dir
    p <- jsonlite::fromJSON(file.path(out, "sensitivity_results.json"))
    D <- p$demand_variables
    labels <- setNames(p$demand_labels, D)
    labels <- c(labels, supply_risk = "Supply risk score", demand_score = "Demand risk score")
    wrap <- function(x) vapply(x, function(v) paste(strwrap(v, 31), collapse = "\n"), character(1))
    library(ggplot2)
    theme_report <- theme_minimal(base_size = 12, base_family = "Arial") + theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), axis.title.y = element_blank(), axis.text = element_text(color = "#244657"),
        plot.title = element_text(face = "bold", color = "#17374a", size = 16), legend.position = "top",
        legend.title = element_blank(), strip.text.y = element_text(face = "bold", color = "#17374a"),
        plot.margin = margin(12, 18, 12, 12))
    save_plot <- function(plot, name, height) {
        file <- file.path(out, name)
        ggsave(file, plot, width = 10.2, height = height, device = svglite::svglite, bg = "white")
        ggsave(sub("[.]svg$", ".png", file), plot, width = 10.2, height = height, dpi = 144, bg = "white")
        text <- readLines(file, warn = FALSE)
        text <- text[!grepl("<!--.*Created with", text)]
        writeLines(text, file, useBytes = TRUE)
    }
    a <- p$available_coefficients
    a <- a[(a$model == "supply" & a$term == "supply_risk") | (startsWith(a$model, "single_") & a$term ==
        sub("^single_", "", a$model)), ]
    a$component <- factor(a$term, levels = rev(c("supply_risk", D)))
    a$sample <- factor(a$sample, levels = c("common", "available"), labels = c("Common sample", "Available cases"))
    pos <- position_dodge(width = 0.58)
    g <- ggplot(a, aes(x = odds_ratio, y = component, color = sample)) + geom_vline(xintercept = 1, linetype = "dashed",
        color = "#aabcc6", linewidth = 0.5) + geom_errorbar(aes(xmin = or_lo, xmax = or_hi), orientation = "y",
        width = 0.17, position = pos, linewidth = 0.6) + geom_point(position = pos, size = 2.7) + scale_x_log10() +
        scale_y_discrete(labels = function(x) wrap(labels[x])) + scale_color_manual(values = c(`Common sample` = "#74838d",
        `Available cases` = "#047f8b")) + labs(title = "Broader eligible samples give similar adjusted associations",
        x = "Adjusted odds ratio · 95% confidence interval") + theme_report
    save_plot(g, "available_case_comparison.svg", 6.3)
    b <- p$joint_coefficients
    b <- b[(b$model == "score_joint" & b$term %in% c("demand_score", "supply_risk")) | (b$model == "components_joint" &
        b$term %in% c(D, "supply_risk")), ]
    b$key <- paste(b$model, b$term)
    keys <- c(paste("score_joint", c("demand_score", "supply_risk")), paste("components_joint", c(D,
        "supply_risk")))
    b$key <- factor(b$key, levels = rev(keys))
    key_labels <- setNames(labels[sub("^.* ", "", keys)], keys)
    b$model_label <- factor(b$model, levels = c("score_joint", "components_joint"), labels = c("Demand + supply\nscores",
        "Six demand indicators\n+ supply score"))
    b$weighting <- factor(b$weighting, levels = c("survey_only", "response_weighted"), labels = c("Survey weights",
        "Survey + response weights"))
    g <- ggplot(b, aes(x = odds_ratio, y = key, color = weighting)) + geom_vline(xintercept = 1, linetype = "dashed",
        color = "#aabcc6", linewidth = 0.5) + geom_errorbar(aes(xmin = or_lo, xmax = or_hi), orientation = "y",
        width = 0.17, position = pos, linewidth = 0.6) + geom_point(position = pos, size = 2.7) + scale_x_log10() +
        scale_y_discrete(labels = function(x) wrap(key_labels[x])) + scale_color_manual(values = c(`Survey weights` = "#74838d",
        `Survey + response weights` = "#047f8b")) + facet_grid(model_label ~ ., scales = "free_y", space = "free_y") +
        labs(title = "Joint estimates remain similar after response adjustment", x = "Adjusted odds ratio · 95% LGA-jackknife confidence interval") +
        theme_report + theme(strip.text.y = element_text(angle = 0, size = 10), strip.placement = "outside")
    save_plot(g, "response_weighting_comparison.svg", 7.7)
    cat("Saved two aggregate sensitivity coefficient figures.\n")
}


# Execute the full report -------------------------------------------------------
build_lagos_aggregate <- function(root = .report_root, join_dir = "", cache_dir = "data/cache",
    work_dir = "output/build/aggregate/lagos", python = "", input_zip = "",
    output_dir = work_dir, input_dir = "", html = TRUE) {
  root <- normalizePath(root, mustWork = TRUE)
  work_dir <- portable_path(work_dir, root); output_dir <- portable_path(output_dir, root)
  cache_dir <- portable_path(cache_dir, root)
  needed <- if (html) required_packages else setdiff(required_packages, c("sf", "rmarkdown", "knitr"))
  setup_analysis_packages(needed, locked_environment, library_dir,
    install_missing = install_missing_packages, check_versions = check_package_versions)
  pandoc <- cli_value("--pandoc-dir", Sys.getenv("RSTUDIO_PANDOC", ""))
  if (nzchar(pandoc)) Sys.setenv(RSTUDIO_PANDOC = pandoc)
  if (html) {
    select_python(python)
    if (!rmarkdown::pandoc_available()) stop("Pandoc is required for HTML. Use RStudio or supply --pandoc-dir / RSTUDIO_PANDOC.", call. = FALSE)
    resource_root <- prepare_report_resources(root, work_dir, report_resources, resource_url)
    assets_dir <- file.path(resource_root, "support/assets")
  }
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  message("Report output: ", output_dir)
  if (nzchar(input_zip)) {
    input_zip <- portable_path(input_zip, root)
    verify_input_bundle(input_zip, cache_dir)
    input_dir <- cache_dir
  } else if (nzchar(input_dir)) input_dir <- portable_path(input_dir, root) else input_dir <- cache_dir
  if (!dir.exists(input_dir)) stop("Supply the authorized input ZIP or extracted input directory.")
  if (nzchar(join_dir)) {
    join_dir <- portable_path(join_dir, root)
    admin <- if (file.exists(file.path(join_dir, "admin_reference/audit/facility_linkage_inputs.rds"))) file.path(join_dir, "admin_reference") else join_dir
    if (nzchar(input_zip)) {
      record_file <- file.path(admin, "run_parameters.json")
      if (!file.exists(record_file)) stop("The supplied verified join lacks its input-bundle record.")
      record <- jsonlite::read_json(record_file, simplifyVector = TRUE)
      if (!identical(record$input_sha256, digest::digest(file = input_zip, algo = "sha256")) ||
          !identical(record$geography_version, "both")) stop("The supplied join does not match the input ZIP/geography version.")
    }
    linked <- list(woman_path = file.path(admin, "checked_geography/lagos_women_manitoba.rds"),
      facility_path = file.path(admin, "audit/facility_linkage_inputs.rds"),
      raw_women_path = file.path(input_dir, country_config[["lagos"]]$woman_file),
      raw_facility_path = file.path(input_dir, "inputs/uom", country_config[["lagos"]]$facility, country_config[["lagos"]]$raw_name),
      dictionary_path = file.path(input_dir, "exports/merged_supply_demand_dictionary.csv"),
      cleaned_facility_path = file.path(input_dir, "uom_pipeline/uom_summary_facility.csv"))
    stopifnot(file.exists(linked$woman_path), file.exists(linked$facility_path))
  } else linked <- read_and_link_country("lagos", input_dir, file.path(work_dir, "linkage"))
  prep_dir <- file.path(work_dir, "inputs")
  data_dir <- file.path(work_dir, "Lagos_analysis_aggregate_files")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  prepare_fp_facility_profiles(linked$woman_path, linked$facility_path, linked$raw_facility_path,
    linked$raw_women_path, linked$dictionary_path, prep_dir)
  analytic_input <- file.path(prep_dir, "lagos_women_manitoba_fp_providers.rds")
  calculate_descriptives_and_area_scores(input_path = analytic_input, facility_path = linked$facility_path, output_dir = data_dir, raw_facility_path = linked$raw_facility_path)
  fit_modern_use_models(data_dir = data_dir, output_dir = file.path(data_dir, "models_zscore"), score_method = "zscore", pca_score_file = "")
  pca_dir <- file.path(data_dir, "pca_appendix")
  calculate_pca_scores(data_dir = data_dir, output_dir = pca_dir)
  fit_modern_use_models(data_dir = data_dir, output_dir = file.path(pca_dir, "models"), score_method = "pca", pca_score_file = file.path(pca_dir, "pca_scores.rds"))
  sensitivity_dir <- file.path(data_dir, "regression_sensitivity")
  run_complete_case_sensitivities(data_dir = data_dir, output_dir = sensitivity_dir)
  validate_complete_case_sensitivities(data_dir = data_dir, sensitivity_dir = sensitivity_dir)
  plot_complete_case_sensitivities(data_dir = sensitivity_dir)
  validate_analysis(data_dir = data_dir, input_path = analytic_input, facility_path = linked$facility_path, raw_facility_path = linked$raw_facility_path, raw_women_path = linked$raw_women_path, cleaned_facility_path = linked$cleaned_facility_path)
  result <- if (html) render_report("lagos", "aggregate", "Lagos_analysis_aggregate.html", "Lagos updated LGA analysis | SURGO × Manitoba",
    data_dir, work_dir, output_dir, resource_root, python) else list(data_dir = data_dir)
  jsonlite::write_json(list(country = "Lagos", aggregation = "aggregate", html = html,
    report = if (html) "Lagos_analysis_aggregate.html" else NULL,
    result_tables = "report_data.json, heatmap_values_and_denominators.csv, all_method_scores_and_classifications.csv",
    model_results = "models_zscore/model_coefficients.csv",
    private_outputs = "Working joins and fitted model objects contain confidential data; do not upload."),
    file.path(output_dir, "RESULTS.json"), auto_unbox = TRUE, pretty = TRUE)
  message("Completed Lagos_analysis_aggregate; results: ", output_dir)
  result
}

if (run_analysis) {
  report_result <- build_lagos_aggregate(root = .report_root, join_dir = cli_value("--join-dir"), cache_dir = cache_dir,
    work_dir = work_dir, python = cli_value("--python", Sys.getenv("SURGO_MANITOBA_PYTHON", "")),
    input_zip = input_zip, output_dir = output_dir, input_dir = input_dir, html = render_html)
}
