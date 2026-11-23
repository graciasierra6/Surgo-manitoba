#!/usr/bin/env Rscript
# Senegal | Administrative-area × Rural/Urban analysis
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
#   Rscript rural_urban/senegal/Senegal_rural_urban_updated.R --input-zip INPUT.zip
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
#     inputs/surgo/senegal/surgo_senegal_women.dta
#   Original facility survey:
#     inputs/uom/senegal/uom_senegal_facility_pooled.dta
#   Checked health-district mapping:
#     lookups/senegal_health_district_department_evidence.csv
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
#   Facility residence proxy: inputs/SenegalURlookup.xlsx
#
# All recodes, joins, facility aggregation, scores, models, implemented
# sensitivities and independent numerical checks are included in this file.
# Main SURGO analyses use women in need of FP; coverage retains all source women.
# SURGO source weight: WEIGHT -> internal weight; sampling design ids=~1,
# no strata/FPC. Main regression standard errors cluster by département (CR2).
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
  gsub("~+~", " ", sub("^--file=", "", .entry[1]), fixed = TRUE) else "Senegal_rural_urban_updated.R"
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
    "support/renderers/rural_urban/senegal_render.py",
    "support/renderers/rural_urban/senegal_verify.py",
    "support/assets/boundaries/senegal_departments.geojson"),
  sha256 = c("a1614c7c1467e4adff8653c04cb60a87f8e491ce566f0d38eb690b854f8b6294",
    "2aa4da8b3257aa8ab1bda1dcbe7f3d65b03932b27ce3ea7dbd93f4b6b7ad8448",
    "dfaef69903df000b7e37fd54095f2ab892ec9d90803f54a09b3675f18b745219",
    "d752996b2c61b404660e5c6b3fb7db73edf76f510fbf15ff823be3f64c0e4eaf",
    "3382f02f1a965acb63a44dbbf0c28321923529dfce02efa79a976581bf4de0c4",
    "f5fa87f2e2866e9f2270430dadf68e3307e1c874007140f6b421efe098bacb6d",
    "bfccf55d9023446a521f10a3e30ae5e98e500e71f95b889cf80efebc9952f96a",
    "628825fb573573b6ab9417eef388ddb1355c9b23d4ebf716143e39064ba30e00",
    "b4a2e75f1623685a3aae6fa8e8d951a261f30a9b1aa159095a04ce11096f8711",
    "cfe5f628225ddc4f1498658a29e6990ec0d0f5cdb317aaf9d3e586b579c59712"),
  stringsAsFactors = FALSE)

.report_root <- locate_project(script_dir)
output_dir <- file.path("output", "rebuilt", "rural_urban", "senegal")
work_dir <- file.path("output", "build", "rural_urban", "senegal")
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
  cat("Run: Rscript rural_urban/senegal/Senegal_rural_urban_updated.R --input-zip FILE\n",
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

# Initial residence linkage preserves original survey fields. The subsequent
# assign_residence_supply() step recomputes all final means among FP providers
# and applies the documented whole-area fallback only for zero-provider cells.
prepare_residence_input <- function(country, linked, input_dir, out) {
  fac <- readRDS(linked$facility_path)
  w <- read_women(linked$raw_women_path)
  field <- if (country == "lagos") "settlement" else "FV_MILIEU"
  residence <- rurb_value(w[[field]])
  fc_residence <- rurb_value(fac$rurb)
  dictionary <- read_chars(linked$dictionary_path)
  dictionary <- dictionary[dictionary$table_name == "merged_supply_demand_department", ]
  fac$.context <- rurb_key(fac$geo_join_audited, fc_residence)
  profiles <- aggregate_supply(fac, dictionary, ".context")
  keys <- unique(data.frame(geo_join = fac$.context, area = fac$geo_join_audited, rurb_join = fc_residence))
  keys <- keys[!is.na(keys$geo_join), ]
  profiles <- left_join_safe(profiles, keys, "geo_join", "Area/residence names")
  profiles$geo_join <- profiles$area
  profiles$area <- NULL
  area <- toupper(trimws(lab(w[[country_config[[country]]$woman_geo]])))
  index <- rurb_match(area, residence, profiles)
  added <- profiles[index, setdiff(names(profiles), c("country", "join_level", "geo_join", "rurb_join")), drop = FALSE]
  rownames(added) <- NULL
  joined <- dplyr::bind_cols(w, added)
  joined$link_source_row <- seq_len(nrow(w)); joined$link_country <- country
  joined$link_geo_level <- country_config[[country]]$join_level; joined$link_geo_join <- area
  joined$link_rurb <- residence; joined$link_geo_mode <- "audited_geography"
  joined$link_rurb_basis <- if (country == "senegal") "health_district_lookup_proxy" else "recorded_facility_location"
  joined$link_woman_rurb_basis <- "recorded_woman_residence"
  joined$link_rurb_proxy <- country == "senegal"
  joined$link_aggregation <- "administrative_area_urban_rural"
  joined$link_has_supply <- !is.na(index)
  joined$link_has_admin_supply <- norm(area) %in% norm(fac$geo_join_audited) & !is.na(norm(area))
  facility_area <- norm(fac$geo_join_audited)
  woman_area <- norm(area)
  unique_areas <- unique(woman_area)
  for (pair in list(c("link_n_admin_fac", "all"), c("link_n_admin_rural", "Rural"),
                    c("link_n_admin_urban", "Urban"), c("link_n_admin_unknown", "missing"))) {
    counts <- vapply(unique_areas, function(g) {
      same <- !is.na(facility_area) & facility_area == g
      included <- switch(pair[2], all = rep(TRUE, nrow(fac)), missing = is.na(fc_residence), fc_residence %in% pair[2])
      as.integer(sum(same & included, na.rm = TRUE))
    }, integer(1))
    joined[[pair[1]]] <- counts[match(woman_area, unique_areas)]
  }
  joined$link_issue <- rurb_issue(area, residence, joined$link_has_supply,
    joined$link_n_admin_fac, joined$link_n_admin_rural, joined$link_n_admin_urban)
  joined$link_status <- ifelse(is.na(norm(area)), "missing_woman_geography",
    ifelse(is.na(residence), "missing_woman_residence", ifelse(!is.na(index), "matched", "no_facility_area_residence_match")))
  joined$n_facilities[is.na(joined$n_facilities)] <- 0L
  joined$link_single_facility <- joined$link_has_supply & joined$n_facilities == 1L
  weight <- num(w[[country_config[[country]]$weight]])
  joined$link_valid_weight <- is.finite(weight) & weight > 0
  stopifnot(nrow(joined) == nrow(w), all(vapply(names(w), function(k) identical(w[[k]], joined[[k]]), logical(1))))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out, paste0(country, "_women_manitoba_urban_rural.rds"))
  saveRDS(joined, path, compress = "gzip")
  path
}


# 3. Country recodes and facility measures ------------------------------------


senegal_key <- function(v) {
    v <- iconv(as.character(v), to = "ASCII//TRANSLIT")
    v <- toupper(gsub("[^A-Za-z0-9]", "", v))
    v[is.na(v) | v == ""] <- NA_character_
    v
}

senegal_domains <- function() list(D = c("agency", "stigma", "health", "fertility", "confidence", "opposes"),
    DL = c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs",
        "Future-fertility concerns", "Low method knowledge", "Opposes family planning"), S = c("offering",
        "free", "training", "counselor", "larc_stock", "popular_stock"), SL = c("Short + long-acting offering",
        "Free-product provision", "Trained-provider share", "FP counselor availability", "At least one LARC in stock",
        "Most-popular product in stock"))

senegal_supply <- function(x) {
    map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
        counselor = "share_fp_counselor", larc_stock = "share_stocked_1plus_larc")
    a <- as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
    yes <- as.numeric(x$n_mp_in_stock_true)
    no <- as.numeric(x$n_mp_in_stock_false)
    a$popular_stock <- ifelse(yes + no > 0, yes/(yes + no), NA_real_)
    a
}

senegal_definitions <- function(x) {
    nv <- function(k) {
        if (!k %in% names(x))
            stop("Missing Senegal field: ", k)
        as.numeric(x[[k]])
    }
    flag <- function(k, pos, valid) {
        v <- nv(k)
        ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_)
    }
    any_flag <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1, ifelse(rowSums(is.na(m)) > 0,
        NA_real_, 0))
    stopifnot(!anyDuplicated(x$IDW))
    d <- data.frame(row = seq_len(nrow(x)), id = as.character(x$IDW), geo = senegal_key(x$link_geo_join),
        district = as.character(x$link_geo_join), weight = nv("WEIGHT"), consent = nv("IRB1") %in% 1,
        psu = as.character(x$RB0H), residence = as.character(x$link_rurb))
    d$positive <- d$consent & is.finite(d$weight) & d$weight > 0
    d$need_code <- nv("FPN2")
    d$need <- d$positive & d$need_code %in% 1
    d$agency <- flag("HS7C", 1, 1:2)
    d$stigma <- any_flag(cbind(flag("PB7B", 1:2, 1:4), flag("PB7C", 1:2, 1:4)))
    d$health <- flag("PB2A", 1:2, 1:4)
    mp <- sapply(paste0("MP4_", letters[1:9]), nv)
    selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
    none <- mp[, 9] %in% 1
    coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & nv("MP4_99") %in% 0 & (selected | none) &
        !(selected & none)
    d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) >
        0), NA_real_)
    d$confidence <- any_flag(cbind(flag("MC5A", 3:4, 1:4), flag("MC5B", 3:4, 1:4)))
    d$opposes <- flag("FP1", 3:4, 1:4)
    prompts <- sapply(paste0("CU5", LETTERS[1:4]), nv)
    current <- ifelse(nv("CU1") %in% c(1, 3) | rowSums(prompts == 1, na.rm = TRUE) > 0, 1, ifelse(nv("CU1") %in%
        2 & rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
    d$user <- current %in% 1
    modern <- rowSums(sapply(paste0("CU4_", 1:8), function(k) nv(k) %in% 1)) > 0 | nv("CU5C") %in% 1 |
        nv("CU5D") %in% 1 | (nv("CU4_11") %in% 1 & nv("CU4_lam") %in% 1)
    traditional <- rowSums(sapply(paste0("CU4_", c(9, 10, 12:15)), function(k) nv(k) %in% 1)) > 0 | nv("CU5A") %in%
        1 | nv("CU5B") %in% 1
    d$modern <- d$user & modern
    d$traditional <- d$user & !modern & traditional
    d$y_modern <- ifelse(current %in% 0, 0, ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
    d$intender <- current %in% 0 & nv("ITU1") %in% 1:2
    d$no_intent <- current %in% 0 & nv("ITU1") %in% c(3, 4, 99)
    d$unresolved <- !(d$user | d$intender | d$no_intent)
    d$wanted_start <- d$user & nv("CUB8") %in% 1
    d$other_start <- d$user & nv("CUB8") %in% 2
    d$modern_other_start <- d$modern & nv("CUB8") %in% 2
    future_modern <- rowSums(sapply(paste0("ITS2_", 1:8), function(k) nv(k) %in% 1)) > 0
    d$traditional_modern_switch <- d$traditional & nv("ITC2") %in% 1:2 & future_modern
    age <- nv("RB2")
    d$age10 <- ifelse(age >= 15 & age <= 49, (age - 30)/10, NA_real_)
    d$age10_sq <- d$age10^2
    edu <- ifelse(nv("RB3") %in% 1:4, "Primary or less", ifelse(nv("RB3") %in% 5:6, "Middle / secondary",
        ifelse(nv("RB3") %in% 7:8, "Tertiary / professional", NA_character_)))
    d$education <- factor(edu, levels = c("Primary or less", "Middle / secondary", "Tertiary / professional"))
    d$partnered <- flag("R1", 1:3, 1:4)
    goal <- rep("Uncertain or unstated", nrow(x))
    goal[nv("SF2") %in% 4] <- "After marriage"
    goal[nv("SF2") %in% 3] <- "After 3 years"
    goal[nv("SF2") %in% 2] <- "In 1-3 years"
    goal[nv("SF1A") %in% 2 | nv("SF1B") %in% 2 | nv("SF1C") %in% 2] <- "No more children"
    d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children",
        "After marriage", "Uncertain or unstated"))
    d$urban <- ifelse(nv("FV_MILIEU") %in% 1:2, as.numeric(nv("FV_MILIEU") == 2), NA_real_)
    stopifnot(all(d$residence == ifelse(d$urban == 1, "Urban", "Rural")), !anyNA(d$psu[d$positive]),
        all(nzchar(d$psu[d$positive])), all(vapply(split(d$geo[d$positive], d$psu[d$positive]), function(v) length(unique(v)) ==
            1, logical(1))))
    d
}


base_senegal_domains <- senegal_domains

senegal_domains <- function() {
    a <- base_senegal_domains()
    a$S <- c("offering", "free", "training", "daily_provision", "larc_stock", "stockout_burden")
    a$SL <- c("Short + long-acting offering", "Free-product provision", "Trained-provider share", "Daily provision of offered modern methods",
        "At least one LARC in stock", "Stockout burden")
    a
}

senegal_supply <- function(x) {
    map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
        daily_provision = "mean_daily_modern_provision", larc_stock = "share_stocked_1plus_larc", stockout_burden = "mean_facility_any_stockout")
    missing <- setdiff(unname(map), names(x))
    if (length(missing))
        stop("Missing updated supply fields: ", paste(missing, collapse = ", "))
    as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
}

senegal_favorable_supply <- function(x) {
    x <- as.data.frame(x)
    if ("stockout_burden" %in% names(x))
        x$stockout_burden <- 1 - x$stockout_burden
    x
}

senegal_frequency <- function(raw) {
    fields <- paste0("s5_7", letters[1:7])
    stopifnot(all(fields %in% names(raw)))
    m <- vapply(raw[fields], as.numeric, numeric(nrow(raw)))
    valid <- m >= 1 & m <= 5
    offered <- m >= 1 & m <= 4
    n_offered <- rowSums(offered, na.rm = TRUE)
    n_daily <- rowSums(m == 1, na.rm = TRUE)
    n_weekly <- rowSums(m >= 1 & m <= 2, na.rm = TRUE)
    daily <- ifelse(n_offered > 0, n_daily/n_offered, NA_real_)
    weekly <- ifelse(n_offered > 0, n_weekly/n_offered, NA_real_)
    data.frame(daily_provision = daily, at_least_weekly_provision = weekly, frequency_daily_categories = n_daily,
        frequency_offered_categories = n_offered, frequency_valid_categories = rowSums(valid, na.rm = TRUE),
        frequency_unknown_categories = 7L - rowSums(valid, na.rm = TRUE))
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
    hashes <- unname(tools::md5sum(inputs))
    all_fac <- readRDS(facility_input)
    fac <- all_fac[all_fac$country == "senegal", ]
    r <- haven::read_dta(raw_facility, col_select = tidyselect::all_of(c("fid", "s5_6", "s7_2", paste0("s5_7",
        letters[1:7]))))
    assert_unique(data.frame(fid = lab(r$fid)), "fid", "Raw Senegal facilities")
    ri <- match(fac$fid, lab(r$fid))
    stopifnot(!anyNA(ri), nrow(fac) == nrow(r))
    fp <- num(r$s5_6[ri])
    counsel <- num(r$s7_2[ri])
    expected <- ifelse(counsel %in% 1, 1, ifelse(counsel %in% c(0, 2), 0, NA_real_))
    stopifnot(isTRUE(all.equal(expected, fac$counselor_available, check.attributes = FALSE)))
    fac$counselor_available <- expected
    fac$fp_provider <- fp %in% 1
    frequency <- senegal_frequency(r[ri, ])
    fac <- cbind(fac, frequency)
    eligible <- fac[fac$fp_provider, ]
    dictionary <- read_chars(dictionary_file)
    dictionary <- dictionary[dictionary$table_name == "merged_supply_demand_department", ]
    profile <- aggregate_supply(eligible, dictionary, "assigned_join_area")
    for (k in c("mean_daily_modern_provision", "mean_weekly_modern_provision")) profile[[k]] <- NA_real_
    profile$n_daily_modern_valid <- profile$n_frequency_incomplete <- 0L
    for (i in seq_len(nrow(profile))) {
        z <- eligible[eligible$assigned_join_area == profile$geo_join[i], ]
        vv <- z$daily_provision
        profile$mean_daily_modern_provision[i] <- if (any(is.finite(vv)))
            mean(vv[is.finite(vv)])
        else NA_real_
        vv <- z$at_least_weekly_provision
        profile$mean_weekly_modern_provision[i] <- if (any(is.finite(vv)))
            mean(vv[is.finite(vv)])
        else NA_real_
        profile$n_daily_modern_valid[i] <- sum(is.finite(z$daily_provision))
        profile$n_frequency_incomplete[i] <- sum(z$frequency_unknown_categories > 0)
    }
    stopifnot(nrow(profile) == 46, sum(profile$n_facilities) == sum(fp %in% 1))
    original <- readRDS(input)
    joined <- original
    ix <- match(norm(original$link_geo_join), norm(profile$geo_join))
    stopifnot(!anyNA(ix), !anyDuplicated(norm(profile$geo_join)))
    replace <- setdiff(names(profile), c("country", "join_level", "geo_join"))
    for (k in replace) joined[[k]] <- profile[[k]][ix]
    joined$link_has_supply <- !is.na(ix)
    joined$link_geo_mode <- "audited_fp_providers"
    joined$link_fp_provider_only <- TRUE
    raw <- haven::read_dta(raw_women)
    same <- function(a, b) isTRUE(all.equal(as.vector(a), as.vector(b), check.attributes = FALSE, tolerance = 0))
    preserved <- vapply(names(raw), function(k) same(raw[[k]], joined[[k]]), logical(1))
    stopifnot(nrow(joined) == nrow(raw), identical(original$IDW, joined$IDW), all(preserved))
    areas <- unique(norm(original$link_geo_join))
    linked <- norm(fac$assigned_join_area) %in% areas
    coverage <- do.call(rbind, lapply(sort(unique(fac$assigned_join_area)), function(g) {
        z <- fac[fac$assigned_join_area == g, ]
        selected <- z$fp_provider
        data.frame(departement = g, all_records = nrow(z), fp_yes = sum(selected), counselor_yes = sum(selected &
            z$counselor_available %in% 1), counselor_no = sum(selected & z$counselor_available %in% 0),
            counselor_unknown = sum(selected & is.na(z$counselor_available)))
    }))
    audit <- list(source_women = nrow(raw), original_survey_columns = length(preserved), original_fields_preserved = all(preserved),
        source_facilities = nrow(fac), fp_yes = sum(fp %in% 1), fp_no = sum(fp %in% c(0, 2)), fp_unknown = sum(!fp %in%
            c(0, 1, 2)), linked_source_facilities = sum(linked), linked_fp_providers = sum(linked & fac$fp_provider),
        counselor_question = attr(r$s7_2, "label"), counselor_yes = sum(linked & fac$fp_provider & expected %in%
            1), counselor_no = sum(linked & fac$fp_provider & expected %in% 0), counselor_unknown = sum(linked &
            fac$fp_provider & is.na(expected)), counselor_definition = "s7_2: 1 Yes; 0/2 No; all other values unknown. Availability is not a measure of training, receipt or quality.",
        daily_provision_definition = "s5_7a-g: count Daily (1) / count Daily, Weekly, Biweekly or Monthly (1-4); code 5 Not at all excluded from the offered-method denominator; unknown responses excluded, not zero. Mean valid facility ratios among s5_6=1.",
        daily_valid_linked = sum(linked & fac$fp_provider & is.finite(fac$daily_provision)), daily_undefined_linked = sum(linked &
            fac$fp_provider & !is.finite(fac$daily_provision)), stockout_definition = "Original cleaned UoM p_any_stockout retained without recoding; valid facility means among s5_6=1. Higher raw values are worse; scoring reverses direction.",
        all_supply_columns_reaggregated = replace, linked_departements = length(unique(ix)), source_files_unchanged = identical(hashes,
            unname(tools::md5sum(inputs))))
    stopifnot(audit$source_files_unchanged, audit$fp_yes == 1941, audit$linked_fp_providers == 1568,
        audit$counselor_yes == 135, audit$counselor_no == 50, audit$linked_departements == 33)
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    saveRDS(joined, file.path(out, "senegal_women_manitoba_fp_providers.rds"), compress = "gzip")
    haven::write_dta(joined, file.path(out, "senegal_women_manitoba_fp_providers.dta"), version = 15)
    back <- haven::read_dta(file.path(out, "senegal_women_manitoba_fp_providers.dta"))
    stopifnot(nrow(back) == nrow(raw), all(vapply(names(raw), function(k) {
        a <- raw[[k]]
        b <- back[[k]]
        if (is.character(a)) {
            a[is.na(a)] <- ""
            b[is.na(b)] <- ""
            identical(a, b)
        } else same(a, b)
    }, logical(1))))
    saveRDS(profile, file.path(out, "senegal_fp_provider_profiles.rds"))
    write.csv(profile, file.path(out, "senegal_fp_provider_profiles.csv"), row.names = FALSE, na = "")
    write.csv(coverage, file.path(out, "fp_provider_counselor_coverage.csv"), row.names = FALSE, na = "")
    write.csv(fac[c("fid", "assigned_join_area", "fp_provider", names(frequency))], file.path(out, "daily_provision_facility_audit.csv"),
        row.names = FALSE, na = "")
    jsonlite::write_json(audit, file.path(out, "dataset_verification.json"), auto_unbox = TRUE, pretty = TRUE)
    cat("Verified all original women and survey columns; reaggregated all supply columns among FP providers.\n")
    cat(audit$linked_fp_providers, "eligible facilities in", audit$linked_departements, "linked departements.\n")
}


# Build analysis --------------------------------------------------------------


calculate_descriptives_and_area_scores <- function(input_path, facility_path, raw_women_path, raw_facility_path, department_lookup_path, output_dir) {
    input <- input_path
    facility_file <- facility_path
    raw_file <- raw_women_path
    raw_facility <- raw_facility_path
    department_lookup <- department_lookup_path
    out <- output_dir
    facility_sample <- "all"
    reference_dir <- ""
    demand_sample <- "available"
    stopifnot(facility_sample == "all", demand_sample == "available")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    inputs <- c(input, facility_file, raw_file, raw_facility, department_lookup)
    hash <- unname(tools::md5sum(inputs))
    savecsv <- function(v, name) write.csv(v, file.path(out, name), row.names = FALSE, na = "")
    key <- senegal_key
    lab <- function(v) if (inherits(v, "haven_labelled"))
        as.character(haven::as_factor(v))
    else as.character(v)
    valid_mean <- function(v) if (any(is.finite(v)))
        mean(v[is.finite(v)])
    else NA_real_
    wm <- function(v, w) {
        ok <- is.finite(v) & is.finite(w) & w > 0
        if (any(ok))
            survey_weighted_mean(v[ok], w[ok])
        else NA_real_
    }
    bool <- function(v) ifelse(tolower(v) %in% c("true", "1"), 1, ifelse(tolower(v) %in% c("false", "0"),
        0, NA_real_))
    x0 <- readRDS(input)
    admin <- x0
    x <- x0
    stopifnot(nrow(x) == 7169, !anyDuplicated(x$IDW), all(x$link_has_supply))
    x$link_rurb <- ifelse(as.numeric(x$FV_MILIEU) == 1, "Rural", ifelse(as.numeric(x$FV_MILIEU) == 2,
        "Urban", NA_character_))
    fallback <- rep(FALSE, nrow(x))
    x$analysis_supply_fallback <- fallback
    x$analysis_supply_match_level <- "Department"
    x$analysis_supply_source_key <- key(x$link_geo_join)
    x$analysis_has_supply <- x$link_has_supply
    d <- senegal_definitions(x)
    pb2a <- as.numeric(x$PB2A)
    d$health <- ifelse(pb2a %in% 1:4, as.numeric(pb2a %in% 1:2), NA_real_)
    domains <- senegal_domains()
    D <- domains$D
    DL <- domains$DL
    S <- domains$S
    SL <- domains$SL
    sp <- senegal_supply(x)[S]
    d$fallback <- fallback
    d$source_profile <- x$analysis_supply_source_key
    profiles <- unique(cbind(geo = key(admin$link_geo_join), district = as.character(admin$link_geo_join),
        senegal_supply(admin)[S], n_facilities = as.numeric(admin$n_facilities)))
    stopifnot(!anyDuplicated(profiles$geo), nrow(profiles) == 33, sum(d$positive) == 7158, sum(d$need) ==
        1905)
    ctx <- unique(cbind(d[c("geo", "district", "residence", "fallback", "source_profile")], sp, n_facilities = as.numeric(x$n_facilities)))
    stopifnot(!anyDuplicated(paste(ctx$geo, ctx$residence)), nrow(ctx) == 49)
    savecsv(ctx, "assigned_supply_profiles.csv")
    fac <- readRDS(facility_file)
    fac <- fac[fac$country == "senegal", ]
    fac$geo <- key(fac$assigned_join_area)
    areas <- sort(profiles$geo)
    names_map <- setNames(profiles$district, profiles$geo)
    mapped <- fac$geo %in% areas
    r <- haven::read_dta(raw_facility, col_select = tidyselect::all_of(c("fid", "s1_3", "s5_6", paste0("s5_7",
        letters[1:7]))))
    ri <- match(fac$fid, lab(r$fid))
    stopifnot(!anyNA(ri))
    e <- read.csv(department_lookup, check.names = FALSE, stringsAsFactors = FALSE)
    ei <- match(key(lab(r$s1_3)[ri]), key(e[["Health district"]]))
    stopifnot(!anyNA(ei))
    ed <- trimws(sub(" —.*$", "", e[["Department-checked"]][ei]))
    ed <- trimws(sub(" \\(.*$", "", ed))
    stopifnot(all(key(ed) == fac$geo))
    inventory <- fac
    fp_code <- as.numeric(r$s5_6[ri])
    frequency <- senegal_frequency(r[ri, ])
    fac <- cbind(fac, frequency)
    fac <- fac[fp_code %in% 1, ]
    mapped <- fac$geo %in% areas
    stopifnot(nrow(fac) == 1941, sum(mapped) == 1568)
    fac$offering <- bool(fac$one_larc_3_sa_offer)
    fac$free <- as.numeric(fac$p_free_prod)
    fac$training <- as.numeric(fac$p_train)
    fac$larc_stock <- bool(fac$one_larc_stockin)
    fac$stockout_burden <- as.numeric(fac$p_any_stockout)
    stopifnot(all(bool(fac$three_larc_stockin) == 0), nrow(inventory) == 2043)
    stopifnot(all(vapply(areas, function(g) sum(fac$geo == g) > 0, logical(1))))
    savecsv(fac[c("fid", "geo", names(frequency))], "daily_provision_facility_audit.csv")
    savecsv(do.call(rbind, lapply(areas, function(g) {
        ff <- fac[fac$geo == g, ]
        data.frame(geo = g, department = names_map[[g]], fp_facilities = nrow(ff), daily_valid = sum(is.finite(ff$daily_provision)),
            daily_undefined = sum(!is.finite(ff$daily_provision)), all_seven_frequency_responses = sum(ff$frequency_valid_categories ==
                7), stockout_valid = sum(is.finite(ff$stockout_burden)), mean_daily_provision = valid_mean(ff$daily_provision),
            mean_at_least_weekly_provision = valid_mean(ff$at_least_weekly_provision))
    })), "service_regularity_coverage.csv")
    for (k in S) stopifnot(all(vapply(split(sp[[k]], d$geo), function(v) length(unique(v)) == 1, logical(1))))
    savecsv(ctx, "assigned_supply_profiles.csv")
    reconcile <- list()
    check_profile <- function(ff, pp, scope, area, residence) {
        stopifnot(nrow(ff) == pp$n_facilities)
        for (k in S) {
            actual <- valid_mean(ff[[k]])
            expected <- pp[[k]]
            passed <- if (is.na(actual) || is.na(expected))
                is.na(actual) && is.na(expected)
            else abs(actual - expected) <= 5.00001e-06
            reconcile[[length(reconcile) + 1L]] <<- data.frame(scope = scope, geo = area, residence = residence,
                variable = k, facility_recalculation = actual, joined_value = expected, n_valid = sum(is.finite(ff[[k]])),
                n_facilities = nrow(ff), passed = passed)
        }
    }
    for (g in areas) check_profile(fac[fac$geo == g, ], profiles[profiles$geo == g, ], "Department",
        g, "All")
    for (j in seq_len(nrow(ctx))) {
        q <- ctx[j, ]
        fi <- fac$geo == q$geo
        check_profile(fac[fi, ], q, "Department assigned to woman", q$geo, q$residence)
    }
    reconcile <- do.call(rbind, reconcile)
    stopifnot(all(reconcile$passed))
    savecsv(reconcile, "supply_input_reconciliation.csv")
    trace <- data.frame(row = d$row, IDW = d$id, department = d$district, residence = d$residence, in_need = d$need,
        exact_profile_available = !fallback, department_fallback = fallback, supply_source_key = d$source_profile,
        assigned_facilities = profiles$n_facilities[match(d$geo, profiles$geo)])
    saveRDS(trace, file.path(out, "woman_assignment_audit.rds"))
    fallback_table <- do.call(rbind, lapply(areas, function(g) {
        ff <- fac[fac$geo == g, ]
        w <- d$geo == g
        data.frame(geo = g, department = names_map[[g]], source_women = sum(w), rural_women = sum(w &
            d$residence == "Rural"), urban_women = sum(w & d$residence == "Urban"), need_women = sum(w &
            d$need), rural_need = sum(w & d$need & d$residence == "Rural"), urban_need = sum(w & d$need &
            d$residence == "Urban"), fallback_women = sum(w & fallback), fallback_need = sum(w & fallback &
            d$need), facility_records = nrow(ff), rural_classified_facilities = sum(ff$rurb == "Rural",
            na.rm = TRUE), urban_classified_facilities = sum(ff$rurb == "Urban", na.rm = TRUE), unknown_residence_facilities = sum(is.na(ff$rurb)))
    }))
    savecsv(fallback_table, "department_residence_assignment_summary.csv")
    options(survey.lonely.psu = "adjust")
    full <- surgo_design(d[d$positive, ])
    need_design <- subset(full, need)
    estimate <- function(des, k) {
        v <- as.numeric(des$variables[[k]])
        p <- wm(v, des$variables$weight)
        ci <- c(NA_real_, NA_real_)
        if (is.finite(p) && p > 0 && p < 1) {
            des$variables$.event <- v
            ci <- as.numeric(confint(survey::svyciprop(~.event, des, method = "logit", df = survey::degf(des),
                na.rm = TRUE)))
        }
        list(n = sum(v == 1, na.rm = TRUE), denominator = length(v), estimate = p, lo = ci[1], hi = ci[2])
    }
    bar_defs <- data.frame(field = c("user", "wanted_start", "other_start", "modern", "modern_other_start",
        "traditional", "traditional_modern_switch", "intender", "no_intent", "unresolved"), label = c("Current contraceptive users",
        "Using the method wanted at start*", "Not using the method wanted at start*", "Current modern-method users",
        "Wanted another method at start*", "Traditional methods (subset of users)", "Wants to switch to a modern method",
        "Nonusers with intention", "Nonusers without stated intention", "Use / intention unresolved"),
        indent = c(0, 1, 1, 0, 1, 0, 1, 0, 0, 0), color = c("#0a7d89", "#579b90", "#ba8743", "#307e60",
            "#ba8743", "#8c8981", "#b57a32", "#446da4", "#89639a", "#9caab0"))
    bars <- lapply(seq_len(nrow(bar_defs)), function(i) c(as.list(bar_defs[i, ]), estimate(need_design,
        bar_defs$field[i])))
    rates <- lapply(areas, function(g) {
        a <- subset(need_design, geo == g)
        list(geo = g, name = names_map[[g]], n = nrow(a$variables), cpr = estimate(a, "user"), mcpr = estimate(a,
            "modern"), intender = estimate(a, "intender"), no_intent = estimate(a, "no_intent"))
    })
    dn <- d[d$need, ]
    cells <- list()
    coverage <- list()
    dn_complete <- complete.cases(dn[D]) & apply(dn[D], 1, function(v) all(is.finite(v)))
    stopifnot(identical(d$health, ifelse(as.numeric(x$PB2A) %in% 1:4, as.numeric(as.numeric(x$PB2A) %in%
        1:2), NA_real_)))
    savecsv(do.call(rbind, lapply(c("SENEGAL", areas), function(g) {
        ii <- if (g == "SENEGAL")
            rep(TRUE, nrow(dn))
        else dn$geo == g
        data.frame(geo = g, department = if (g == "SENEGAL")
            "Senegal"
        else names_map[[g]], women_in_need = sum(ii), complete_demand = sum(ii & dn_complete), excluded = sum(ii &
            !dn_complete), complete_weight_share = survey_weighted_mean(as.numeric(dn_complete[ii]),
            dn$weight[ii]))
    })), "demand_complete_case_coverage.csv")
    for (g in c("SENEGAL", areas)) {
        wi <- if (g == "SENEGAL")
            rep(TRUE, nrow(dn))
        else dn$geo %in% g
        fi <- mapped & (if (g == "SENEGAL")
            rep(TRUE, nrow(fac))
        else fac$geo %in% g)
        coverage[[g]] <- list(geo = g, name = if (g == "SENEGAL") "Senegal" else names_map[[g]], women = sum(d$positive &
            if (g == "SENEGAL") rep(TRUE, nrow(d)) else d$geo %in% g), all_source_women = if (g == "SENEGAL") nrow(d) else sum(d$geo %in%
            g), n_need = sum(wi), facilities = sum(inventory$geo %in% areas & (if (g == "SENEGAL") rep(TRUE,
            nrow(inventory)) else inventory$geo %in% g)), fp_facilities = sum(fi))
        for (j in seq_along(D)) {
            k <- D[j]
            ok <- wi & is.finite(dn[[k]]) & (demand_sample == "available" | dn_complete)
            cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Demand", variable = k, label = DL[j],
                value = 100 * wm(dn[[k]][ok], dn$weight[ok]), n_valid = sum(ok), n_total = sum(wi), numerator = sum(dn[[k]][ok] ==
                  1))
        }
        for (j in seq_along(S)) {
            k <- S[j]
            v <- if (g == "SENEGAL")
                valid_mean(fac[[k]][fi])
            else profiles[[k]][match(g, profiles$geo)]
            cells[[length(cells) + 1L]] <- data.frame(geo = g, side = "Supply", variable = k, label = SL[j],
                value = 100 * v, n_valid = sum(fi & is.finite(fac[[k]])), n_total = sum(fi), numerator = sum(fac[[k]][fi],
                  na.rm = TRUE))
        }
    }
    for (g in setdiff(sort(unique(inventory$geo)), areas)) {
        coverage[[g]] <- list(geo = g, name = inventory$assigned_join_area[match(g, inventory$geo)],
            women = 0, all_source_women = 0, n_need = 0, facilities = sum(inventory$geo == g), fp_facilities = sum(fac$geo ==
                g))
    }
    cells <- do.call(rbind, cells)
    savecsv(cells, "heatmap_values_and_denominators.csv")
    primary_trace <- if (nzchar(reference_dir))
        read.csv(file.path(reference_dir, "component_score_trace.csv"), stringsAsFactors = FALSE)
    else NULL
    primary_thresholds <- if (nzchar(reference_dir))
        read.csv(file.path(reference_dir, "classification_thresholds.csv"), stringsAsFactors = FALSE)
    else NULL
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
            as.matrix(100 * (1 - senegal_favorable_supply(as.data.frame(original/100))))
        else original
        rownames(m) <- areas
        means <- colMeans(m, na.rm = TRUE)
        sds <- apply(m, 2, sd, na.rm = TRUE)
        meds <- apply(m, 2, median, na.rm = TRUE)
        iqrs <- apply(m, 2, IQR, na.rm = TRUE)
        stopifnot(all(sds > 0), all(iqrs > 0))
        M[[side]] <- list(raw = m, rank = apply(m, 2, rank, ties.method = "average", na.last = "keep"),
            robust = sweep(sweep(m, 2, meds, "-"), 2, iqrs, "/"), zscore = sweep(sweep(m, 2, means, "-"),
                2, sds, "/"))
        for (k in cols) transforms[[length(transforms) + 1L]] <- data.frame(geo = areas, side = side,
            variable = k, original_pct = original[, k], aligned_pct = m[, k], higher_raw_is_worse = side ==
                "Supply" && k == "stockout_burden", indicator_mean = means[k], indicator_SD = sds[k],
            indicator_median = meds[k], indicator_IQR = iqrs[k], component_rank = M[[side]]$rank[, k],
            zscore = M[[side]]$zscore[, k], robust = M[[side]]$robust[, k])
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
        demand_flag = dc > length(D)/2, supply_flag = sc > length(S)/2, demand_priority_rank = rank(-dc,
            ties.method = "average", na.last = "keep"), supply_priority_rank = rank(-sc, ties.method = "average",
            na.last = "keep"), category = classify(dc > length(D)/2, sc > length(S)/2))
    thresholds$median_count <- data.frame(method = "median_count", demand_cutoff = floor(length(D)/2) +
        1, supply_cutoff = floor(length(S)/2) + 1, demand_rule = "4 or more of six above component medians",
        supply_rule = paste(floor(length(S)/2) + 1, "or more of", length(S), "above constraint-component medians"),
        demand_reference_n = sum(is.finite(dc)), supply_reference_n = sum(is.finite(sc)))
    scores <- do.call(rbind, scores)
    thresholds <- do.call(rbind, thresholds)
    trace <- do.call(rbind, transforms)
    if (!is.null(primary_trace)) {
        for (side in c("Demand", "Supply")) {
            fields <- if (side == "Demand")
                D
            else S
            for (k in fields) {
                rr <- primary_trace[primary_trace$side == side & primary_trace$variable == k, ]
                ii <- trace$side == side & trace$variable == k
                stopifnot(nrow(rr) == length(areas))
                trace$indicator_mean[ii] <- unique(rr$indicator_mean)
                trace$indicator_SD[ii] <- unique(rr$indicator_SD)
                trace$zscore[ii] <- (trace$aligned_pct[ii] - unique(rr$indicator_mean))/unique(rr$indicator_SD)
            }
            value <- vapply(areas, function(g) mean(trace$zscore[trace$side == side & trace$geo == g]),
                numeric(1))
            cut <- primary_thresholds[primary_thresholds$method == "zscore", if (side == "Demand")
                "demand_cutoff"
            else "supply_cutoff"]
            ix <- which(scores$method == "zscore")
            if (side == "Demand") {
                scores$demand_score[ix] <- value[match(scores$geo[ix], areas)]
                scores$demand_flag[ix] <- scores$demand_score[ix] > cut + 1e-10
                scores$demand_priority_rank[ix] <- rank(-scores$demand_score[ix], ties.method = "average")
                thresholds$demand_cutoff[thresholds$method == "zscore"] <- cut
            }
            else {
                scores$supply_score[ix] <- value[match(scores$geo[ix], areas)]
                scores$supply_flag[ix] <- scores$supply_score[ix] > cut + 1e-10
                scores$supply_priority_rank[ix] <- rank(-scores$supply_score[ix], ties.method = "average")
                thresholds$supply_cutoff[thresholds$method == "zscore"] <- cut
            }
        }
        scores$category[ix] <- classify(scores$demand_flag[ix], scores$supply_flag[ix])
    }
    planning <- lapply(areas, function(g) {
        q <- scores[scores$method == "zscore" & scores$geo == g, ]
        dt <- trace[trace$geo == g & trace$side == "Demand", ]
        st <- trace[trace$geo == g & trace$side == "Supply", ]
        list(geo = g, name = names_map[[g]], category = q$category, demand_flag = q$demand_flag, supply_flag = q$supply_flag,
            demand_score = q$demand_score, supply_score = q$supply_score, top_demand = head(dt$variable[order(-dt$zscore,
                match(dt$variable, D), na.last = TRUE)], 3), top_supply = if (all(is.finite(st$zscore))) head(st$variable[order(-st$zscore,
                match(st$variable, S))], 3) else character(), n_need = sum(dn$geo == g), facility_count = profiles$n_facilities[match(g,
                profiles$geo)])
    })
    missing <- do.call(rbind, lapply(areas, function(g) do.call(rbind, lapply(D, function(k) {
        a <- dn[dn$geo == g, ]
        data.frame(geo = g, variable = k, n = nrow(a), valid = sum(is.finite(a[[k]])), missing = sum(!is.finite(a[[k]])),
            weighted_pct = 100 * wm(a[[k]], a$weight))
    }))))
    savecsv(missing, "demand_missingness_by_department.csv")
    savecsv(scores, "all_method_scores_and_classifications.csv")
    savecsv(thresholds, "classification_thresholds.csv")
    savecsv(trace, "component_score_trace.csv")
    savecsv(do.call(rbind, lapply(bars, as.data.frame)), "use_distribution.csv")
    savecsv(do.call(rbind, lapply(rates, function(r) data.frame(geo = r$geo, name = r$name, n_need = r$n,
        cpr = r$cpr$estimate, mcpr = r$mcpr$estimate))), "department_use_rates.csv")
    savecsv(profiles, "department_supply_profiles.csv")
    comparison <- data.frame(geo = areas, name = names_map[areas])
    for (id in c("rank", "raw", "robust", "median_count", "zscore")) comparison[[id]] <- scores$category[scores$method ==
        id][match(areas, scores$geo[scores$method == id])]
    savecsv(comparison, "classification_comparison.csv")
    plan_table <- do.call(rbind, lapply(planning, function(q) data.frame(department = q$name, category = q$category,
        demand_flag = q$demand_flag, supply_flag = q$supply_flag, top_demand = paste(q$top_demand, collapse = "; "),
        top_supply = paste(q$top_supply, collapse = "; "), n_need = q$n_need, facility_count = q$facility_count)))
    savecsv(plan_table, "planning_priorities.csv")
    audit <- list(supply_score_orientation = "constraints", country = "Senegal", source_rows = nrow(x),
        consenting_positive_weight = sum(d$positive), need_n = sum(d$need), need_variable = "Original SURGO FPN2 = 1, retained unchanged. No reconstructed need_n, sterilization or hysterectomy additions.",
        need_qualification = "Use the supplied current-need flag as recorded; do not substitute the broader segmentation population or DHS unmet-need algorithm.",
        weight_field = "WEIGHT", psu_field = "RB0H: recorded District de recensement (census district)",
        survey_design = "Final WEIGHT and recorded RB0H census districts, nested in departments and residence. Full consenting positive-weight frame before FPN2 domain selection. Original sampling strata/FPC not documented; no reconstructed strata claimed.",
        survey_psus = length(unique(d$psu[d$positive])), districts = length(areas), departments = length(areas),
        facility_records = nrow(inventory), mapped_facility_records = sum(inventory$geo %in% areas),
        facility_only_records = sum(!inventory$geo %in% areas), unmapped_facility_records = 0, eligible_facility_records = nrow(fac),
        eligible_mapped_facilities = sum(mapped), daily_valid_linked = sum(mapped & is.finite(fac$daily_provision)),
        daily_undefined_linked = sum(mapped & !is.finite(fac$daily_provision)), stockout_valid_linked = sum(mapped &
            is.finite(fac$stockout_burden)), health_definition = "PB2A only; 1/2 concern, 3/4 no concern, other responses unresolved",
        facility_departments = length(unique(fac$geo)), geography_version = "Completed, documented health-district -> department crosswalk used consistently in both input joins. Outside-SURGO departments retained for coverage.",
        supply_resolution = "Department-wide facility summaries assigned to every woman in her own department; no Rural/Urban breakdown or fallback.",
        facility_sample = facility_sample, facility_records_used = sum(mapped), counseling_included = FALSE,
        stockout_cleaned_definition_retained = TRUE, daily_provision_included = TRUE, demand_sample = demand_sample,
        complete_demand_women = sum(dn_complete), primary_reference_fixed = nzchar(reference_dir), fallback_source_women = 0,
        fallback_need_women = 0, fallback_departments = 0, assigned_context_profiles = length(areas),
        facility_residence_missing = sum(is.na(fac$rurb)), supply_unavailable = "The cleaned three-LARC stock indicator is constant FALSE in Senegal. The existing at-least-one-LARC stock indicator is retained.",
        supply_missing = "Valid zeros and FALSE values are included. Daily provision excludes unknown frequency responses from its numerator/denominator and is undefined if no category is recorded offered; undefined facility ratios do not enter the mean. All 33 departments have six finite component means. No partial-index averaging or geographic fallback.",
        supply_denominators = "Restrict raw s5_6 to 1 (FP services on-site), then average valid facility values without facility weights. Stockout burden retains original cleaned p_any_stockout. Daily provision uses s5_7a-g: count Daily / count Daily, Weekly, Biweekly or Monthly; Not at all is excluded from the offered denominator. Unknown responses are not zeros. Boolean summaries retain upstream five-decimal rounding.",
        supply_alignment = "Heatmaps retain raw stockout burden (higher worse). Before scoring and PCA, convert burden to continuity = 1 - burden, so all six supply inputs are favorable. Average component z-scores for readiness; supply risk is its negative. This equals averaging six risk-aligned z-scores.",
        residence_limitation = "Rural/Urban remains a woman-level covariate. Facility residence lookup labels do not determine supply assignment in this report.",
        scoring = paste("Six demand domains and", length(S), "direction-aligned supply domains. Average-z primary with zero (mean) thresholds: demand > 0, supply readiness < 0. Each of 33 linked departments contributes equally to component normalization. Other comparison methods retain their documented rules."),
        weighted_need_share = wm(as.numeric(d$need[d$positive]), d$weight[d$positive]), use_definition = "CU1 yes/sometimes plus CU5 prompts. Modern: CU4 1-8, prompted condom/ECP or explicitly contraceptive LAM. Traditional-only and confirmed nonuse are zero for modern-use models; unresolved status remains missing.",
        controls = "Age and age squared, Senegal-specific education, partnership, fertility goal and woman's Rural/Urban status. No random effects or interactions.",
        source_md5 = as.list(hash), source_names = as.list(basename(inputs)), source_files_unchanged = identical(hash,
            unname(tools::md5sum(inputs))))
    stopifnot(audit$source_files_unchanged, sum(vapply(bars, function(b) if (b$field %in% c("user", "intender",
        "no_intent", "unresolved")) b$n else 0, numeric(1))) == sum(d$need))
    payload <- list(audit = audit, demand_variables = D, demand_labels = DL, supply_variables = S, supply_labels = SL,
        bars = bars, rates = rates, heatmap = cells, coverage = unname(coverage), scores = scores, thresholds = thresholds,
        component_trace = trace, planning = planning, profiles = profiles, assignment_summary = fallback_table,
        assigned_profiles = ctx)
    jsonlite::write_json(payload, file.path(out, "report_data.json"), dataframe = "rows", auto_unbox = TRUE,
        na = "null", digits = 12)
    jsonlite::write_json(audit, file.path(out, "analysis_audit.json"), auto_unbox = TRUE, pretty = TRUE,
        na = "null")
    saveRDS(list(women = d, supply = sp, profiles = profiles, assigned_profiles = ctx, facility_sample = facility_sample),
        file.path(out, "analysis_frame.rds"))
    cat("Built", sum(d$need), "women in need across", length(areas), "departments; source fallback",
        sum(fallback), "need fallback", sum(fallback & d$need), "\n")
    print(table(scores$method, scores$category))
    print(data.frame(field = bar_defs$field, n = vapply(bars, `[[`, numeric(1), "n"), pct = 100 * vapply(bars,
        `[[`, numeric(1), "estimate")))
    payload <- export_risk_scores(payload, out)
}


# Assign residence supply -----------------------------------------------------


assign_residence_supply <- function(input_path, facility_path, raw_facility_path, dictionary_path, data_dir, output_dir, residence_lookup_path) {
    input <- input_path
    facility_input <- facility_path
    raw_facility <- raw_facility_path
    dictionary_file <- dictionary_path
    data_dir <- data_dir
    out <- output_dir
    residence_lookup <- residence_lookup_path
    key <- senegal_key
    mean_valid <- function(v) {
        v <- as.numeric(v)
        if (any(is.finite(v)))
            mean(v[is.finite(v)])
        else NA_real_
    }
    source_hash <- unname(tools::md5sum(c(input, facility_input, raw_facility, residence_lookup)))
    x <- readRDS(input)
    frame <- readRDS(file.path(data_dir, "analysis_frame.rds"))
    d <- frame$women
    stopifnot(nrow(x) == 7169, identical(as.character(x$IDW), d$id), identical(key(x$link_geo_join),
        d$geo), identical(as.character(x$link_rurb), ifelse(as.numeric(x$FV_MILIEU) == 1, "Rural", ifelse(as.numeric(x$FV_MILIEU) ==
        2, "Urban", NA_character_))), all(x$link_rurb %in% c("Rural", "Urban")))
    f <- readRDS(facility_input)
    f <- f[f$country == "senegal", ]
    r <- haven::read_dta(raw_facility, col_select = tidyselect::all_of(c("fid", "s5_6", "s1_3", paste0("s5_7",
        letters[1:7]))))
    ri <- match(f$fid, as.character(r$fid))
    stopifnot(!anyNA(ri), !anyDuplicated(ri), nrow(f) == 2043)
    f$geo <- key(f$assigned_join_area)
    f$residence <- as.character(f$rurb)
    lut <- readxl::read_xlsx(residence_lookup, range = "A1:B79")
    stopifnot(!anyDuplicated(key(lut$s1_3)))
    li <- match(key(as.character(haven::as_factor(r$s1_3[ri]))), key(lut$s1_3))
    rv <- as.numeric(lut$urbanrural[li])
    raw_residence <- ifelse(rv %in% 1, "Rural", ifelse(rv %in% 2, "Urban", NA_character_))
    stopifnot(identical(tolower(raw_residence), tolower(f$residence)))
    f$context <- paste(f$geo, f$residence, sep = "|")
    fp <- as.numeric(r$s5_6[ri]) %in% 1
    f <- cbind(f, senegal_frequency(r[ri, ]))
    eligible <- f[fp, ]
    stopifnot(nrow(eligible) == 1941)
    dictionary <- read_chars(dictionary_file)
    dictionary <- dictionary[dictionary$table_name == "merged_supply_demand_department", ]
    mapping <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
        daily_provision = "mean_daily_modern_provision", larc_stock = "share_stocked_1plus_larc", stockout_burden = "mean_facility_any_stockout")
    aggregate_profiles <- function(group) {
        a <- aggregate_supply(eligible, dictionary, group)
        a$mean_daily_modern_provision <- NA_real_
        a$n_daily_modern_valid <- 0L
        for (i in seq_len(nrow(a))) {
            z <- eligible[eligible[[group]] == a$geo_join[i], ]
            a$share_offer_larc_and_3_sa[i] <- mean_valid(bool(z$one_larc_3_sa_offer))
            a$share_stocked_1plus_larc[i] <- mean_valid(bool(z$one_larc_stockin))
            a$mean_p_free_prod[i] <- mean_valid(z$p_free_prod)
            a$mean_p_train[i] <- mean_valid(z$p_train)
            a$mean_facility_any_stockout[i] <- mean_valid(z$p_any_stockout)
            a$mean_daily_modern_provision[i] <- mean_valid(z$daily_provision)
            a$n_daily_modern_valid[i] <- sum(is.finite(z$daily_provision))
        }
        a
    }
    profiles <- aggregate_profiles("context")
    admin_profiles <- aggregate_profiles("geo")
    wk <- paste(d$geo, as.character(x$link_rurb), sep = "|")
    wi <- match(wk, profiles$geo_join)
    ai <- match(d$geo, admin_profiles$geo_join)
    fallback <- is.na(wi) & !is.na(ai)
    joined <- x
    replace <- setdiff(names(profiles), c("country", "join_level", "geo_join"))
    for (k in replace) {
        joined[[k]] <- profiles[[k]][wi]
        joined[[k]][fallback] <- admin_profiles[[k]][ai[fallback]]
    }
    count_fields <- replace[grepl("^n_", replace)]
    for (k in count_fields) joined[[k]][is.na(wi) & !fallback] <- 0
    assigned_profile <- ifelse(fallback, paste(d$geo, "Departement", sep = "|"), wk)
    joined$link_has_supply <- !is.na(wi) | fallback
    joined$link_fp_provider_only <- TRUE
    joined$link_has_exact_fp_supply <- !is.na(wi)
    joined$link_supply_fallback <- fallback
    joined$link_requested_supply_profile <- wk
    joined$link_supply_profile <- assigned_profile
    joined$link_supply_resolution <- ifelse(fallback, "departement-wide fallback", "Departement-residence")
    joined$link_geo_mode <- "audited_fp_providers_residence_with_empty_cell_lga_fallback"
    joined$link_status <- ifelse(fallback, "departement-wide FP-provider fallback: zero exact-cell FP providers",
        ifelse(is.na(wi), "No eligible FP profile at either resolution", "Matched exact Departement-residence FP profile"))
    S <- names(mapping)
    sp <- as.data.frame(lapply(mapping, function(k) as.numeric(joined[[k]])))
    names(sp) <- S
    d$residence <- as.character(x$link_rurb)
    d$source_profile <- assigned_profile
    d$fallback <- fallback
    d$has_exact_fp_supply <- !is.na(wi)
    d$has_fp_supply <- joined$link_has_supply
    observed <- unique(data.frame(context = wk, geo = d$geo, residence = d$residence, stringsAsFactors = FALSE))
    observed <- observed[order(observed$geo, observed$residence), ]
    observed$women <- vapply(observed$context, function(k) sum(wk == k), integer(1))
    observed$women_in_need <- vapply(observed$context, function(k) sum(wk == k & d$need), integer(1))
    observed$source_facilities <- vapply(observed$context, function(k) sum(f$context == k), integer(1))
    observed$fp_facilities <- vapply(observed$context, function(k) sum(eligible$context == k), integer(1))
    observed$fallback <- vapply(observed$context, function(k) any(fallback[wk == k]), logical(1))
    observed$assigned_profile <- vapply(observed$context, function(k) unique(assigned_profile[wk == k]),
        character(1))
    observed$assignment_resolution <- ifelse(observed$fallback, "departement-wide fallback", "Exact residence")
    observed$assigned_fp_facilities <- vapply(observed$context, function(k) as.integer(joined$n_facilities[match(k,
        wk)]), integer(1))
    for (k in S) {
        v <- switch(k, offering = bool(eligible$one_larc_3_sa_offer), free = as.numeric(eligible$p_free_prod),
            training = as.numeric(eligible$p_train), daily_provision = eligible$daily_provision, larc_stock = bool(eligible$one_larc_stockin),
            stockout_burden = as.numeric(eligible$p_any_stockout))
        observed[[k]] <- vapply(observed$context, function(a) sp[[k]][match(a, wk)], numeric(1))
        observed[[paste0(k, "_valid")]] <- vapply(seq_len(nrow(observed)), function(i) {
            use <- if (observed$fallback[i])
                eligible$geo == observed$geo[i]
            else eligible$context == observed$context[i]
            sum(use & is.finite(v))
        }, integer(1))
    }
    observed$complete_supply <- complete.cases(observed[S])
    stopifnot(nrow(observed) == 49, sum(observed$complete_supply) == 49, sum(fallback) == 1815, sum(fallback &
        d$need) == 522, all(d$has_fp_supply), length(unique(d$geo[fallback])) == 16, all(d$residence[fallback] ==
        "Rural"), all(is.finite(as.matrix(sp))), identical(x$IDW, joined$IDW), nrow(joined) == 7169)
    for (k in intersect(names(x), names(joined))) {
        if (k %in% replace || grepl("^link_", k))
            next
        stopifnot(isTRUE(all.equal(x[[k]], joined[[k]], check.attributes = FALSE, tolerance = 0)))
    }
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    saveRDS(joined, file.path(out, "senegal_women_manitoba_rural_urban_fp.rds"), compress = "gzip")
    saveRDS(profiles, file.path(out, "senegal_rural_urban_fp_profiles.rds"), compress = "gzip")
    saveRDS(admin_profiles, file.path(out, "senegal_fp_fallback_profiles.rds"), compress = "gzip")
    write.csv(observed, file.path(data_dir, "residence_supply_profiles_and_coverage.csv"), row.names = FALSE,
        na = "")
    write.csv(profiles, file.path(out, "senegal_rural_urban_fp_profiles.csv"), row.names = FALSE, na = "")
    frame$women <- d
    frame$supply <- sp
    frame$residence_profiles <- observed
    frame$assigned_profiles <- observed
    frame$supply_resolution <- "urban_rural"
    saveRDS(frame, file.path(data_dir, "analysis_frame.rds"))
    p <- jsonlite::fromJSON(file.path(data_dir, "report_data.json"), simplifyVector = FALSE)
    p$audit$supply_resolution <- "Departement-Rural/Urban FP-provider profiles, with flagged departement-wide fallback only when the exact cell has zero FP providers; departement-wide planning summaries"
    p$audit$model_supply_aggregation <- "s5_6 Yes, valid-response equal-facility means within residence cells; zero-provider cells receive the same departement's FP-provider means. No zero filling or component fallback when eligible exact-cell providers exist."
    p$audit$observed_residence_cells <- 49L
    p$audit$complete_residence_profiles <- 49L
    p$audit$exact_residence_profiles <- 33L
    p$audit$fallback_residence_profiles <- 16L
    p$audit$unmatched_source_women <- 0L
    p$audit$unmatched_need_women <- 0L
    p$audit$fallback_source_women <- 1815L
    p$audit$fallback_need_women <- 522L
    p$audit$fallback_departments <- 16L
    p$audit$assigned_context_profiles <- 49L
    p$audit$fallback_area <- "Sixteen rural departement cells have zero eligible matched FP providers under the health-district residence proxy; use flagged departement-wide averages."
    p$audit$planning_resolution <- "All s5_6 Yes facilities in each LGA, combining Rural and Urban, unchanged from current aggregate report."
    p$audit$residence_limitation <- "Facility Rural/Urban is a health-district lookup proxy, not a site-level residence measurement. Nine facility records have unknown proxy residence. Zero rural matches do not establish no rural services."
    p$audit$exact_residence_source_md5 <- as.list(source_hash)
    for (i in seq_along(p$assignment_summary)) {
        g <- p$assignment_summary[[i]]$geo
        ii <- d$geo == g
        p$assignment_summary[[i]]$fallback_women <- sum(ii & fallback)
        p$assignment_summary[[i]]$fallback_need <- sum(ii & fallback & d$need)
    }
    write.csv(do.call(rbind, lapply(p$assignment_summary, as.data.frame)), file.path(data_dir, "department_residence_assignment_summary.csv"),
        row.names = FALSE, na = "")
    write.csv(observed, file.path(data_dir, "assigned_supply_profiles.csv"), row.names = FALSE, na = "")
    saveRDS(data.frame(row = d$row, IDW = d$id, department = d$district, residence = d$residence, in_need = d$need,
        exact_profile_available = d$has_exact_fp_supply, department_fallback = fallback, supply_source_key = d$source_profile,
        assigned_facilities = joined$n_facilities), file.path(data_dir, "woman_assignment_audit.rds"))
    jsonlite::write_json(p, file.path(data_dir, "report_data.json"), auto_unbox = TRUE, pretty = FALSE,
        na = "null", null = "null", digits = 12)
    jsonlite::write_json(p$audit, file.path(data_dir, "analysis_audit.json"), auto_unbox = TRUE, pretty = TRUE,
        na = "null", null = "null")
    stopifnot(identical(source_hash, unname(tools::md5sum(c(input, facility_input, raw_facility, residence_lookup)))))
    cat("Residence join: 7169 women retained; 49 complete assigned profiles (33 exact, 16 departement fallbacks); 522 need-group women use flagged fallbacks.\n")
}


# Prepare map -----------------------------------------------------------------


prepare_display_map <- function(boundary_path, data_dir, assets_dir) {
    boundary <- boundary_path
    data_dir <- data_dir
    p <- jsonlite::fromJSON(file.path(data_dir, "report_data.json"))
    b <- sf::st_read(boundary, quiet = TRUE)
    stopifnot(nrow(b) == 46)
    b$geo <- senegal_key(b$adm2_name)
    aliases <- c(NIORODURIP = "NIORO", MEDINAYOROFOULA = "MEDINAYOROFOULAH", RANEROU = "RANEROUFERLO")
    ii <- match(b$geo, names(aliases))
    b$geo[!is.na(ii)] <- aliases[ii[!is.na(ii)]]
    coverage <- p$coverage[p$coverage$geo != "SENEGAL", ]
    stopifnot(!anyDuplicated(b$geo), setequal(b$geo, coverage$geo), length(intersect(b$geo, p$profiles$geo)) ==
        33)
    write.csv(data.frame(boundary_name = b$adm2_name, join_geo = b$geo, join_name = coverage$name[match(b$geo,
        coverage$geo)], boundary_pcode = b$adm2_pcode, exact_normalized_name = senegal_key(b$adm2_name) ==
        b$geo), file.path(data_dir, "map_name_crosswalk.csv"), row.names = FALSE)
    utm <- sf::st_transform(sf::st_make_valid(b), 32628)
    utm <- sf::st_simplify(utm, dTolerance = 120, preserveTopology = TRUE)
    ct <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(utm)))
    features <- lapply(seq_len(nrow(utm)), function(i) {
        g <- sf::st_geometry(utm)[[i]]
        rings <- if (inherits(g, "POLYGON"))
            unclass(g)
        else unlist(unclass(g), recursive = FALSE)
        rings <- lapply(rings, function(z) unname(as.matrix(z)))
        list(geo = utm$geo[i], name = utm$adm2_name[i], rings = unname(rings), center = unname(ct[i,
            1:2]), inset = utm$geo[i] %in% c("DAKAR", "GUEDIAWAYE", "PIKINE", "KEURMASSAR", "RUFISQUE"))
    })
    jsonlite::write_json(list(crs = "EPSG:32628", features = features), file.path(data_dir, "map_layout.json"),
        auto_unbox = TRUE, digits = 10)
    dir.create(file.path(data_dir, "assets"), showWarnings = FALSE)
    file.copy(boundary, file.path(data_dir, "assets/senegal_departments.geojson"), overwrite = TRUE)
    jsonlite::write_json(list(source = "https://data.humdata.org/dataset/cod-ab-sen", source_name = "Government of Senegal / OCHA administrative boundaries",
        snapshot_md5 = unname(tools::md5sum(boundary)), departments = 46, display_simplification_metres = 120,
        qualification = "Boundary snapshot is display geography; no facility point-in-polygon assignment or GPS inference."),
        file.path(data_dir, "assets/boundary_metadata.json"), auto_unbox = TRUE, pretty = TRUE)
    cat("Verified all 46 department polygons: 33 both-source and 13 facility-only departments.\n")
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
    favorable_supply <- senegal_favorable_supply(frame$supply)
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
            reference_departments = length(unique(q$geo)))
    }))
    write.csv(sref, file.path(out, "supply_score_reference.csv"), row.names = FALSE)
    if (score_method == "pca") {
        pp <- readRDS(pca_file)
        stopifnot(identical(pp$scores$row, d$row), pp$audit$report_md5 == report_hash)
        d$demand_score <- pp$scores$demand_score
        d$supply_risk <- pp$scores$supply_risk
    }
    resolution <- "urban_rural_fallback"
    demand_resolution <- "individual"
    z <- p$scores[p$scores$method == "zscore", ]
    profiles <- unique(cbind(d[c("geo", "district", "source_profile", "fallback")], frame$supply, supply_risk = d$supply_risk))
    stopifnot(nrow(profiles) == 49, !anyDuplicated(profiles$source_profile))
    stopifnot(all(vapply(split(d$supply_risk, d$source_profile), function(v) length(unique(v)) == 1,
        logical(1))))
    write.csv(profiles, file.path(out, "model_supply_profiles.csv"), row.names = FALSE, na = "")
    controls <- c("age10", "age10_sq", "education", "partnered", "fertility_goal", "urban")
    specs <- list(score_demand = c(controls, "demand_score"), supply = c(controls, "supply_risk"), score_joint = c(controls,
        "demand_score", "supply_risk"), components_demand = c(controls, D), components_joint = c(controls,
        D, "supply_risk"))
    positive <- d$positive
    common_vars <- unique(c("y_modern", controls, D, "demand_score", "supply_risk"))
    d$eligible <- d$need & positive & complete.cases(d[common_vars])
    cc <- droplevels(d[d$eligible, ])
    stopifnot(all(cc$y_modern %in% 0:1), all(cc$need), length(unique(cc$geo)) >= 20, all(cc$urban %in%
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
            leaveout[[length(leaveout) + 1L]] <- data.frame(model = id, omitted_department = g, term = fields,
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
    savecsv(leaveout, "leave_one_department_out.csv")
    missing <- data.frame(variable = common_vars, missing_in_need = vapply(d[d$need, common_vars], function(a) sum(is.na(a)),
        integer(1)))
    savecsv(missing, "model_missingness.csv")
    savecsv(z[, c("geo", "demand_score", "supply_score")], "model_department_scores.csv")
    available <- d$need & positive & complete.cases(d[c("y_modern", controls, "demand_score", "supply_risk")])
    sensitivity <- list()
    for (id in c("score_demand", "supply", "score_joint")) {
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
        demand_resolution, resolution, "senegal", supply_pca_mixed = score_method == "pca" && length(pp$audit$negative_supply_PC1_loadings) >
            0)
    influence <- do.call(rbind, lapply(split(leaveout, interaction(leaveout$model, leaveout$term, drop = TRUE)),
        function(a) data.frame(model = a$model[1], term = a$term[1], full_estimate = a$full_estimate[1],
            deletion_min = min(a$estimate), deletion_max = max(a$estimate), deletion_sign_changes = sum(sign(a$estimate) !=
                sign(a$full_estimate)))))
    savecsv(influence, "influence_summary.csv")
    saveRDS(fits, file.path(out, "fitted_models.rds"))
    audit <- list(regression_function = "survey::svyglm", design_ids = "~1", design_strata = "None",
        country = "Senegal", outcome = "Current modern contraceptive use only: confirmed modern=1; traditional-only or confirmed nonuse=0; unresolved status remains missing.",
        need_definition = "Original SURGO FPN2=1, unchanged. No reconstructed need flag or permanent-protection additions.",
        need_n = sum(d$need), common_n = nrow(cc), common_events = sum(cc$y_modern), score_method = score_method,
        supply_components = length(S), counseling_included = FALSE, daily_provision_included = TRUE,
        stockout_burden_included = TRUE, demand_resolution = "individual", supply_resolution = "Matched departement-residence FP-provider profiles, with flagged departement-wide fallback only for zero-provider residence cells",
        districts = length(unique(cc$geo)), survey_psus = length(unique(cc$psu)), demand_distinct_values = length(unique(cc$demand_score)),
        supply_distinct_values = length(unique(cc$supply_risk)), assigned_context_profiles = length(unique(cc$source_profile)),
        observed_residence_cells = 49L, exact_residence_profiles = 33L, fallback_residence_profiles = 16L,
        woman_residence_cells = nrow(unique(cc[c("geo", "residence")])), fallback_model_women = sum(cc$fallback),
        fallback_need_women = sum(d$fallback & d$need), women_residence_control = "FV_MILIEU, retained regardless of facility source level",
        controls = controls, omitted_controls = "None of the agreed adjustment domains omitted; all model designs checked for full rank.",
        weights = "Original Senegal WEIGHT; common mean-one fitting rescaling. No facility weights.",
        inference = "Weighted logistic coefficients with department-clustered CR2 covariance, coefficient-specific Satterthwaite intervals, HTZ block tests and Holm adjustment within each joint model. No random effects or interactions.",
        design = "survey::svyglm with ids=~1, no strata or FPC, original SURGO weights; administrative-area CR2 covariance and Satterthwaite tests. Original sampling PSUs are not specified in this model.",
        score_definition = if (score_method == "zscore") "Demand: mean of six personal binary indicators standardized with item-specific weighted mean/population SD in the need group. Supply: transform burden to continuity = 1 - burden, then negative mean of six favorable assigned departement-residence or flagged fallback measures standardized against fixed 33-department component references. Scores enter per one original average-z point." else "Separate weighted demand and supply PCAs; personal demand PC1 and negative assigned residence/fallback supply PC1. Mixed supply signs make it a pattern score, not a uniformly ordered risk index. Scores enter per one original PC1 unit.",
        supply_missing = "All 49 observed residence cells have complete assigned inputs: 33 exact, 16 flagged departement-wide fallbacks. Fallback affects 1815 source women, 522 in need. Facility residence is a health-district lookup proxy. No missing indicator is coded zero and no component fallback is made when an exact eligible profile exists.",
        excluded_need = sum(d$need) - nrow(cc), eligible_weight_share = survey_weighted_mean(as.numeric(d$eligible[d$need]),
            d$weight[d$need]), excluded_departments = setdiff(unique(d$geo[d$need]), unique(cc$geo)),
        uncertainty = "Intervals condition on estimated scores and do not propagate their construction uncertainty; full original sampling design is not available. Diagnostics are not a formal power analysis or causal identification.",
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


validate_analysis <- function(input_path, raw_women_path, facility_path, raw_facility_path, data_dir, cleaned_facility_path, residence_lookup_path) {
    input <- input_path
    admin_input <- input
    raw_file <- raw_women_path
    facility_file <- facility_path
    raw_facility <- raw_facility_path
    data_dir <- data_dir
    cleaned_facility <- cleaned_facility_path
    residence_lookup <- residence_lookup_path
    library(haven)
    library(jsonlite)
    library(clubSandwich)
    out <- file.path(data_dir, "checks")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    x <- readRDS(input)
    admin <- readRDS(admin_input)
    raw <- read_dta(raw_file)
    hybrid <- x
    frame <- readRDS(file.path(data_dir, "analysis_frame.rds"))
    d <- frame$women
    p <- fromJSON(file.path(data_dir, "report_data.json"))
    D <- p$demand_variables
    S <- p$supply_variables
    checks <- list()
    add <- function(name, ok, detail = "") {
        if (!isTRUE(ok))
            stop("Verification failed: ", name, " ", detail)
        checks[[length(checks) + 1L]] <<- data.frame(check = name, passed = TRUE, detail = as.character(detail))
    }
    same <- function(a, b) isTRUE(all.equal(as.vector(a), as.vector(b), check.attributes = FALSE, tolerance = 1e-12))
    add("All original women and IDs retained", nrow(raw) == 7169 && nrow(hybrid) == 7169 && identical(raw$IDW,
        hybrid$IDW))
    raw_same <- vapply(names(raw), function(k) same(raw[[k]], hybrid[[k]]), logical(1))
    add("Every raw SURGO field unchanged", all(raw_same), length(raw_same))
    num <- function(k) as.numeric(raw[[k]])
    valid <- num("IRB1") == 1 & is.finite(num("WEIGHT")) & num("WEIGHT") > 0
    need <- valid & num("FPN2") %in% 1
    add("Original SURGO need flag, not a reconstructed flag", identical(need, d$need) && sum(need) ==
        1905)
    add("Consent and positive weights", identical(valid, d$positive) && sum(valid) == 7158)
    add("Original final survey weights", identical(d$weight, num("WEIGHT")))
    add("Recorded census district retained", identical(d$psu, as.character(raw$RB0H)))
    key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", iconv(as.character(v), to = "ASCII//TRANSLIT")))
    add("Department matches original woman label", identical(d$geo, key(as.character(haven::as_factor(raw$RB0C)))))
    add("Woman residence matches original FV_MILIEU", identical(d$urban, as.numeric(num("FV_MILIEU") ==
        2)))
    add("Census districts nest in departments", all(vapply(split(d$geo[valid], d$psu[valid]), function(v) length(unique(v)) ==
        1, logical(1))))
    add("Census districts nest in residence", all(vapply(split(d$urban[valid], d$psu[valid]), function(v) length(unique(v)) ==
        1, logical(1))))
    fallback <- d$fallback
    add("Every source woman has assigned FP supply", all(x$link_has_supply) && all(d$has_fp_supply))
    add("Fallback flagged for 1815 source and 522 need women", sum(fallback) == 1815 && sum(fallback &
        need) == 522 && length(unique(d$geo[fallback])) == 16)
    add("Requested residence remains unchanged", identical(d$residence, ifelse(num("FV_MILIEU") == 1,
        "Rural", "Urban")))
    add("49 complete assigned residence groups", nrow(frame$residence_profiles) == 49 && all(frame$residence_profiles$complete_supply))
    reconciliation <- read.csv(file.path(data_dir, "supply_input_reconciliation.csv"))
    add("Every baseline department planning component reconciled", nrow(reconciliation) == 492 && all(reconciliation$passed))
    add("33 department profiles with daily provision and stockout burden", nrow(frame$profiles) == 33 &&
        all(complete.cases(frame$profiles[S])) && identical(S, c("offering", "free", "training", "daily_provision",
        "larc_stock", "stockout_burden")))
    fac <- readRDS(facility_file)
    fac <- fac[fac$country == "senegal", ]
    fac$geo <- key(fac$assigned_join_area)
    cleaned <- read.csv(cleaned_facility, stringsAsFactors = FALSE, check.names = FALSE)
    cleaned <- cleaned[tolower(cleaned$loc) == "senegal", ]
    ci <- match(fac$fid, cleaned$fid)
    add("Every stockout burden equals original cleaned UoM export", nrow(cleaned) == 2043 && !anyDuplicated(cleaned$fid) &&
        !anyNA(ci) && same(as.numeric(fac$p_any_stockout), as.numeric(cleaned$p_any_stockout[ci])))
    raw_fp <- read_dta(raw_facility, col_select = all_of(c("fid", "s1_3", "s5_6", paste0("s5_7", letters[1:7]))))
    ii <- match(fac$fid, as.character(haven::as_factor(raw_fp$fid)))
    add("Every facility linked to original eligibility response", !anyNA(ii))
    fp <- as.numeric(raw_fp$s5_6[ii])
    lut <- readxl::read_xlsx(residence_lookup, range = "A1:B79")
    li <- match(key(as.character(haven::as_factor(raw_fp$s1_3[ii]))), key(lut$s1_3))
    rv <- as.numeric(lut$urbanrural[li])
    proxy <- ifelse(rv %in% 1, "Rural", ifelse(rv %in% 2, "Urban", NA_character_))
    add("Facility residence independently matches health-district lookup", identical(proxy, as.character(fac$rurb)))
    add("Nine unknown proxy-residence records retained as unknown", sum(is.na(proxy)) == 9)
    freq <- as.matrix(data.frame(lapply(raw_fp[ii, paste0("s5_7", letters[1:7])], as.numeric)))
    frequency_daily <- apply(freq, 1, function(v) {
        offered <- v[!is.na(v) & v >= 1 & v <= 4]
        if (length(offered))
            sum(offered == 1)/length(offered)
        else NA_real_
    })
    fac$daily_provision <- frequency_daily
    add("Independent daily numerator and offered-method denominator", all(frequency_daily[is.finite(frequency_daily)] >=
        0 & frequency_daily[is.finite(frequency_daily)] <= 1))
    inventory <- fac
    fac <- fac[fp %in% 1, ]
    add("FP-provider restriction is Yes only", nrow(fac) == 1941 && !anyNA(fp[fp %in% 1]))
    boolean <- function(v) ifelse(tolower(v) %in% c("true", "1"), 1, ifelse(tolower(v) %in% c("false",
        "0"), 0, NA_real_))
    fac$offering <- boolean(fac$one_larc_3_sa_offer)
    fac$free <- as.numeric(fac$p_free_prod)
    fac$training <- as.numeric(fac$p_train)
    fac$larc_stock <- boolean(fac$one_larc_stockin)
    fac$stockout_burden <- as.numeric(fac$p_any_stockout)
    add("Known FP-provider inventory", sum(fac$geo %in% unique(d$geo)) == 1568)
    linked <- fac$geo %in% unique(d$geo)
    add("Stockout burden exactly original cleaned values", all(is.finite(fac$stockout_burden[linked])) &&
        sum(linked) == 1568)
    add("Daily usable versus undefined denominators", sum(linked & is.finite(fac$daily_provision)) ==
        1560 && sum(linked & is.na(fac$daily_provision)) == 8)
    add("No counseling or popular-stock component in main analysis", !any(c("counselor", "popular_stock") %in%
        S))
    for (g in unique(d$geo)) {
        ff <- fac[fac$geo == g, ]
        add(paste("Department has facility evidence", g), nrow(ff) > 0, nrow(ff))
        for (k in S) {
            v <- ff[[k]]
            v <- v[is.finite(v)]
            expected <- mean(v)
            pp <- p$heatmap[p$heatmap$side == "Supply" & p$heatmap$geo == g & p$heatmap$variable == k,
                ]
            add(paste("Planning FP-only department mean", g, k), abs(expected - pp$value/100) <= 5.00001e-06 &&
                pp$n_valid == length(v))
            for (res in unique(d$residence[d$geo == g])) {
                use <- ff$rurb %in% res
                fallback_expected <- sum(use) == 0
                selected <- if (fallback_expected)
                  ff
                else ff[use, ]
                vals <- selected[[k]]
                expected <- mean(vals[is.finite(vals)])
                rows <- d$geo == g & d$residence == res
                add(paste("Residence or authorized department mean", g, res, k), all(abs(frame$supply[[k]][rows] -
                  expected) < 1e-12))
                add(paste("Fallback only with zero exact FP providers", g, res, k), all(d$fallback[rows] ==
                  fallback_expected))
            }
        }
    }
    add("All 46 facility departments kept for coverage", length(unique(inventory$geo)) == 46 && nrow(inventory) ==
        2043)
    add("Mbacke fallback equals available urban profile", all(vapply(frame$supply[d$geo == "MBACKE",
        S], function(v) length(unique(v)) == 1, logical(1))))
    add("Three-LARC indicator is constant, not secretly modeled", all(boolean(fac$three_larc_stockin) ==
        0))
    item <- function(k, pos, valid) {
        v <- num(k)
        ifelse(v %in% valid, as.integer(v %in% pos), NA_real_)
    }
    combine <- function(mat) apply(mat, 1, function(v) if (any(v == 1, na.rm = TRUE))
        1
    else if (anyNA(v))
        NA_real_
    else 0)
    ind <- list(agency = item("HS7C", 1, 1:2), stigma = combine(cbind(item("PB7B", 1:2, 1:4), item("PB7C",
        1:2, 1:4))), health = item("PB2A", 1:2, 1:4), confidence = combine(cbind(item("MC5A", 3:4, 1:4),
        item("MC5B", 3:4, 1:4))), opposes = item("FP1", 3:4, 1:4))
    mp <- sapply(paste0("MP4_", letters[1:9]), num)
    ind$fertility <- vapply(seq_len(nrow(mp)), function(i) {
        v <- mp[i, ]
        if (!all(v %in% 0:1) || num("MP4_99")[i] != 0 || !any(v == 1) || v[9] == 1 && any(v[1:8] == 1))
            return(NA_real_)
        as.numeric(any(v[1:6] == 1))
    }, numeric(1))
    for (k in D) add(paste("Independent demand coding", k), same(ind[[k]], d[[k]]))
    unknown <- do.call(rbind, lapply(D, function(k) data.frame(variable = k, n_unresolved = sum(need &
        is.na(d[[k]])), weighted_pct = 100 * weighted.mean(is.na(d[[k]])[need], d$weight[need]))))
    write.csv(unknown, file.path(out, "demand_unresolved_summary.csv"), row.names = FALSE)
    item_unknown <- do.call(rbind, lapply(c("HS7C", "PB7B", "PB7C", "PB2A", "PB2B", "PB2C", "MP4_99",
        "MC5A", "MC5B", "FP1"), function(k) {
        v <- num(k)
        codes <- if (k == "MP4_99")
            1
        else if (k == "FP1")
            c(5, 99)
        else 99
        un <- v %in% codes
        data.frame(variable = k, unresolved_codes = paste(codes, collapse = ";"), n = sum(need & un),
            blank_n = sum(need & is.na(v)), weighted_pct = 100 * weighted.mean(un[need], d$weight[need]))
    }))
    write.csv(item_unknown, file.path(out, "unresolved_item_responses.csv"), row.names = FALSE)
    prompt <- as.matrix(raw[paste0("CU5", LETTERS[1:4])])
    current <- ifelse(num("CU1") %in% c(1, 3) | rowSums(prompt == 1, na.rm = TRUE) > 0, 1, ifelse(num("CU1") %in%
        2 & rowSums(prompt == 2, na.rm = TRUE) == 4, 0, NA_real_))
    modern <- rowSums(as.matrix(raw[paste0("CU4_", 1:8)]) == 1, na.rm = TRUE) > 0 | num("CU5C") %in%
        1 | num("CU5D") %in% 1 | (num("CU4_11") %in% 1 & num("CU4_lam") %in% 1)
    traditional <- rowSums(as.matrix(raw[paste0("CU4_", c(9, 10, 12:15))]) == 1, na.rm = TRUE) > 0 |
        num("CU5A") %in% 1 | num("CU5B") %in% 1
    y <- ifelse(current %in% 0, 0, ifelse(current %in% 1 & modern, 1, ifelse(current %in% 1 & traditional,
        0, NA_real_)))
    add("Independent modern-use outcome", same(y, d$y_modern))
    for (i in seq_len(nrow(p$bars))) {
        b <- p$bars[i, ]
        v <- d[[b$field]][need]
        w <- d$weight[need]
        add(paste("Weighted need-group bar", b$field), abs(sum(v * w)/sum(w) - b$estimate) < 1e-10 &&
            sum(v) == b$n && b$denominator == 1905)
    }
    for (g in unique(d$geo)) for (k in D) {
        r <- p$heatmap[p$heatmap$geo == g & p$heatmap$variable == k & p$heatmap$side == "Demand", ]
        ok <- need & d$geo == g & is.finite(ind[[k]])
        add(paste("Need-group heatmap", g, k), abs(100 * weighted.mean(ind[[k]][ok], d$weight[ok]) -
            r$value) < 1e-09 && sum(ok) == r$n_valid)
    }
    wcorr <- function(a, b, w) {
        ok <- is.finite(a) & is.finite(b) & is.finite(w) & w > 0
        a <- a[ok]
        b <- b[ok]
        w <- w[ok]/sum(w[ok])
        a <- a - sum(a * w)
        b <- b - sum(b * w)
        sum(w * a * b)/sqrt(sum(w * a^2) * sum(w * b^2))
    }
    corr <- list()
    for (side in c("Demand", "Supply")) {
        fields <- if (side == "Demand")
            D
        else S
        mat <- if (side == "Demand")
            d[need, fields]
        else frame$profiles[fields]
        ww <- if (side == "Demand")
            d$weight[need]
        else rep(1, nrow(mat))
        for (j in seq_along(fields)) for (k in seq_along(fields)) corr[[length(corr) + 1L]] <- data.frame(side = side,
            item1 = fields[j], item2 = fields[k], correlation = wcorr(mat[[j]], mat[[k]], ww), n_pair = sum(complete.cases(mat[c(j,
                k)])))
    }
    write.csv(do.call(rbind, corr), file.path(out, "component_correlations.csv"), row.names = FALSE)
    z <- sapply(D, function(k) {
        v <- ind[[k]]
        ok <- need & is.finite(v)
        mu <- weighted.mean(v[ok], d$weight[ok])
        sd <- sqrt(weighted.mean((v[ok] - mu)^2, d$weight[ok]))
        (v - mu)/sd
    })
    personal <- rowSums(z)/6
    favorable_supply <- frame$supply[S]
    favorable_supply$stockout_burden <- 1 - favorable_supply$stockout_burden
    zs <- sapply(S, function(k) {
        r <- p$component_trace[p$component_trace$side == "Supply" & p$component_trace$variable == k,
            ]
        (100 * (1 - favorable_supply[[k]]) - unique(r$indicator_mean))/unique(r$indicator_SD)
    })
    supply <- rowSums(zs)/length(S)
    risk_raw <- frame$profiles[S]
    for (k in setdiff(S, "stockout_burden")) risk_raw[[k]] <- 1 - risk_raw[[k]]
    risk_check <- rowMeans(scale(risk_raw))
    reported <- p$scores[p$scores$method == "zscore", ]
    add("Risk-first and readiness-first standardization equivalent", max(abs(risk_check - reported$supply_score[match(frame$profiles$geo,
        reported$geo)])) < 1e-10)
    add("Stockout high-is-worse sign applied before scoring", all(p$component_trace$higher_raw_is_worse[p$component_trace$variable ==
        "stockout_burden"]) && max(abs(p$component_trace$aligned_pct[p$component_trace$variable == "stockout_burden"] -
        p$component_trace$original_pct[p$component_trace$variable == "stockout_burden"])) < 1e-10)
    pv <- readRDS(file.path(data_dir, "pca_appendix/pca_scores.rds"))
    max_svd <- 0
    for (side in c("demand", "supply")) {
        f <- pv[[paste0(side, "_fit")]]
        mat <- if (side == "demand")
            as.matrix(d[D])
        else as.matrix(favorable_supply)
        use <- need & complete.cases(mat)
        w <- d$weight[use]/sum(d$weight[use])
        z0 <- sweep(sweep(mat[use, , drop = FALSE], 2, f$center, "-"), 2, f$scale, "/")
        sv <- svd(z0 * sqrt(w))
        err <- max(abs(sv$d^2 - f$eigenvalues))
        max_svd <- max(max_svd, err)
        add(paste("Independent weighted SVD", side), err < 1e-10, err)
        score <- drop(sweep(sweep(mat, 2, f$center, "-"), 2, f$scale, "/") %*% f$rotation[, 1])
        if (side == "supply")
            score <- -score
        saved <- if (side == "demand")
            pv$scores$demand_score
        else pv$scores$supply_risk
        add(paste("PCA score for every woman", side), same(score, saved))
    }
    previous <- NULL
    for (method in c("zscore", "pca")) {
        folder <- if (method == "zscore")
            file.path(data_dir, "models_zscore")
        else file.path(data_dir, "pca_appendix/models")
        fits <- readRDS(file.path(folder, "fitted_models.rds"))
        for (id in names(fits)) {
            ob <- fits[[id]]
            dd <- ob$data
            ix <- dd$row
            expectedD <- if (method == "zscore")
                personal[ix]
            else pv$scores$demand_score[ix]
            expectedS <- if (method == "zscore")
                supply[ix]
            else pv$scores$supply_risk[ix]
            add(paste(method, id, "personal demand assignment"), max(abs(dd$demand_score - expectedD)) <
                1e-12)
            add(paste(method, id, "residence-or-fallback supply assignment"), max(abs(dd$supply_risk -
                expectedS)) < 1e-12)
            add(paste(method, id, "need-only common sample and modern outcome"), nrow(dd) == sum(need &
                complete.cases(d[c("y_modern", "age10", "age10_sq", "education", "partnered", "fertility_goal",
                  "urban", D)])) && sum(dd$y_modern) == sum(d$y_modern[dd$row]) && all(dd$need) && all(dd$y_modern %in%
                0:1))
            add(paste(method, id, "agreed covariates including residence"), all(c("age10", "age10_sq",
                "education", "partnered", "fertility_goal", "urban") %in% all.vars(ob$formula)))
            add(paste(method, id, "no random effects or interactions"), !grepl("[|:*]", paste(deparse(ob$formula),
                collapse = " ")))
            add(paste(method, id, "same women in all ten specifications"), is.null(previous) || identical(previous,
                ix))
            previous <- ix
            dd$independent_w <- dd$weight/mean(dd$weight)
            ff <- glm(ob$formula, data = dd, weights = independent_w, family = quasibinomial(), control = glm.control(maxit = 100))
            add(paste(method, id, "independent coefficient reproduction"), max(abs(coef(ff) - coef(ob$fit))) <
                1e-10)
            V <- clubSandwich::vcovCR(ff, cluster = dd$geo, type = "CR2", inverse_var = FALSE)
            add(paste(method, id, "department CR2 reproduction"), max(abs(V - ob$V)) < 1e-10)
        }
    }
    zm <- readRDS(file.path(data_dir, "models_zscore/fitted_models.rds"))
    pm <- readRDS(file.path(data_dir, "pca_appendix/models/fitted_models.rds"))
    add("Demand-indicator-only fits identical across score methods", identical(coef(zm$components_demand$fit),
        coef(pm$components_demand$fit)))
    add("49 assigned groups and 39 numeric scores across 33 departments", length(unique(zm$score_joint$data$supply_risk)) ==
        39 && length(unique(zm$score_joint$data$geo)) == 33 && length(unique(zm$score_joint$data$source_profile)) ==
        49 && all(vapply(split(zm$score_joint$data$supply_risk, zm$score_joint$data$source_profile),
        function(v) length(unique(v)) == 1, logical(1))))
    areas <- sort(unique(d$geo))
    hm <- p$heatmap
    for (side in c("Demand", "Supply")) {
        fields <- if (side == "Demand")
            D
        else S
        mat <- sapply(fields, function(k) {
            rr <- hm[hm$side == side & hm$variable == k, ]
            rr$value[match(areas, rr$geo)]
        })
        if (side == "Supply")
            mat[, "stockout_burden"] <- 100 - mat[, "stockout_burden"]
        if (side == "Supply" && isTRUE(p$audit$primary_reference_fixed)) {
            tt <- p$component_trace[p$component_trace$side == side, ]
            zz <- sapply(seq_along(fields), function(j) {
                rr <- tt[tt$variable == fields[j], ]
                (mat[, j] - unique(rr$indicator_mean))/unique(rr$indicator_SD)
            })
        }
        else zz <- scale(mat)
        expected <- rowMeans(zz)
        rr <- p$scores[p$scores$method == "zscore", ]
        rr <- rr[match(areas, rr$geo), ]
        observed <- if (side == "Demand")
            rr$demand_score
        else -rr$supply_score
        add(paste("Independent department z-score", side), max(abs(expected - observed)) < 1e-09)
        threshold <- p$thresholds[p$thresholds$method == "zscore", if (side == "Demand")
            "demand_cutoff"
        else "supply_cutoff"]
        add(paste("Final score is centered and primary threshold is zero", side), abs(mean(expected)) <
            1e-12 && threshold == 0)
        flag <- if (side == "Demand")
            expected > threshold + 1e-10
        else expected < threshold - 1e-10
        add(paste("Independent zero-mean classification", side), identical(flag, if (side == "Demand")
            rr$demand_flag
        else rr$supply_flag))
    }
    model_rows <- zm$score_joint$data$row
    remaining <- need
    flow <- list(data.frame(step = "Original SURGO FPN2 = 1", excluded_at_step = 0, remaining = sum(remaining)))
    for (step in c("Demand indicators", "Modern-use outcome", "Partnership", "Other covariates")) {
        fields <- switch(step, `Demand indicators` = D, `Modern-use outcome` = "y_modern", Partnership = "partnered",
            `Other covariates` = c("age10", "age10_sq", "education", "fertility_goal", "urban"))
        remove <- remaining & !complete.cases(d[fields])
        remaining <- remaining & !remove
        flow[[length(flow) + 1L]] <- data.frame(step = step, excluded_at_step = sum(remove), remaining = sum(remaining))
    }
    add("Complete-case sample flow reproduced", identical(which(remaining), model_rows))
    write.csv(do.call(rbind, flow), file.path(out, "model_sample_flow.csv"), row.names = FALSE)
    samples <- do.call(rbind, lapply(c(TRUE, FALSE), function(in_model) {
        ok <- need & (d$row %in% model_rows) == in_model
        wmean <- function(v) {
            valid <- ok & is.finite(v)
            weighted.mean(v[valid], d$weight[valid])
        }
        data.frame(group = if (in_model)
            "Included in common models"
        else "Excluded from common models", n = sum(ok), weighted_share_of_need = sum(d$weight[ok])/sum(d$weight[need]),
            modern_use_among_known = wmean(d$y_modern), urban_share = wmean(d$urban), fallback_share = wmean(as.numeric(d$fallback)))
    }))
    write.csv(samples, file.path(out, "model_sample_comparison.csv"), row.names = FALSE)
    save <- do.call(rbind, checks)
    write.csv(save, file.path(out, "audit_checks.csv"), row.names = FALSE)
    write_json(list(check_count = nrow(save), all_passed = all(save$passed), source_rows = nrow(x), need_n = sum(need),
        model_n = nrow(zm$score_joint$data), models = 10, source_raw_columns_verified = length(raw_same),
        facility_component_checks = nrow(reconciliation), woman_supply_value_checks = nrow(d) * length(S),
        weighted_SVD_max_error = max_svd, all_models_same_women = TRUE, all_model_outcomes_modern = TRUE,
        no_random_effects = TRUE, fp_providers_only = TRUE, residence_fallback_verified = TRUE, health_district_residence_proxy = TRUE,
        health_PB2A_only = TRUE, primary_threshold_zero = TRUE), file.path(out, "verification.json"),
        auto_unbox = TRUE, pretty = TRUE, digits = 12)
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
    sm <- as.matrix(senegal_favorable_supply(frame$supply[S]))
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
        stability[[length(stability) + 1L]] <- data.frame(side = side, omitted_department = g, loading_cosine = sum(f$rotation[,
            1] * fit$rotation[, 1]), score_rank_correlation = cor(full_score, alt_score, method = "spearman"),
            pc1_variance_pct = 100 * f$eigenvalues[1]/sum(f$eigenvalues))
    }
    stability <- do.call(rbind, stability)
    savecsv(stability, "pca_leave_one_department_out.csv")
    pc1 <- loads[loads$PC == 1, ]
    audit <- list(method = "Separate final-weighted correlation PCAs on individual demand and assigned residence/fallback supply; PC1 prespecified as sensitivity.",
        need_n = sum(eligible), demand_PCA_n = sum(di), supply_PCA_women_n = sum(si), supply_profiles = nrow(profiles),
        demand_PC1_variance = variance$variance_pct[variance$side == "Demand" & variance$PC == 1], supply_PC1_variance = variance$variance_pct[variance$side ==
            "Supply" & variance$PC == 1], negative_demand_PC1_loadings = pc1$variable[pc1$side == "Demand" &
            pc1$loading < 0], negative_supply_PC1_loadings = pc1$variable[pc1$side == "Supply" & pc1$loading <
            0], supply_collapse_equivalence_error = collapse_error, report_md5 = report_hash, weighting = "SURGO WEIGHT on both sides. Supply PCA is equivalent to assigned residence/fallback profiles weighted by summed weights of represented women; these are not facility weights.",
        missingness = "Require all six demand and all selected supply indicators. No zero coding or partial scores. Flagged departement-wide fallback is used only when an exact residence cell has zero FP providers.",
        alignment = "Before PCA, raw stockout burden is replaced by continuity = 1 - burden; all six supply columns have favorable direction.",
        sign = "Demand PC1 oriented toward average concern; supply PC1 toward average favorable provision and negated for modeling. Mixed signs remain and are explicitly reported.",
        uncertainty = "Permutation and department-deletion checks assess exploratory construction stability, not validated survey-design inference. Regression intervals condition on estimated scores.",
        source_unchanged = report_hash == unname(tools::md5sum(report_file)))
    obj <- list(audit = audit, scores = scores, demand_fit = fd, supply_fit = fs, profiles = profiles,
        loadings = loads, variance = variance, moments = moments, stability = stability)
    saveRDS(obj, file.path(out, "pca_scores.rds"))
    jsonlite::write_json(obj[setdiff(names(obj), c("scores", "demand_fit", "supply_fit"))], file.path(out,
        "pca_results.json"), dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 12)
    jsonlite::write_json(audit, file.path(out, "pca_audit.json"), auto_unbox = TRUE, pretty = TRUE, digits = 12)
    labels <- setNames(c(report$demand_labels, report$supply_labels), c(D, S))
    labels["stockout_burden"] <- "Stock continuity (1 - stockout burden)"
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
        ggplot2::labs(title = "What the PCA scores emphasize", subtitle = "Signed coefficients on standardized inputs; demand uses individual responses, supply uses matched department-wide exposures",
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


# Execute the full report -------------------------------------------------------
build_senegal_rural_urban <- function(root = .report_root, join_dir = "", cache_dir = "data/cache",
    work_dir = "output/build/rural_urban/senegal", python = "", input_zip = "",
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
    linked <- list(woman_path = file.path(admin, "checked_geography/senegal_women_manitoba.rds"),
      facility_path = file.path(admin, "audit/facility_linkage_inputs.rds"),
      raw_women_path = file.path(input_dir, country_config[["senegal"]]$woman_file),
      raw_facility_path = file.path(input_dir, "inputs/uom", country_config[["senegal"]]$facility, country_config[["senegal"]]$raw_name),
      dictionary_path = file.path(input_dir, "exports/merged_supply_demand_dictionary.csv"),
      cleaned_facility_path = file.path(input_dir, "uom_pipeline/uom_summary_facility.csv"))
    stopifnot(file.exists(linked$woman_path), file.exists(linked$facility_path))
  } else linked <- read_and_link_country("senegal", input_dir, file.path(work_dir, "linkage"))
  prep_dir <- file.path(work_dir, "inputs")
  data_dir <- file.path(work_dir, "Senegal_rural_urban_updated_files")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  prepare_fp_facility_profiles(linked$woman_path, linked$facility_path, linked$raw_facility_path,
    linked$raw_women_path, linked$dictionary_path, prep_dir)
  analytic_input <- file.path(prep_dir, "senegal_women_manitoba_fp_providers.rds")
  calculate_descriptives_and_area_scores(input_path = analytic_input, facility_path = linked$facility_path, raw_women_path = linked$raw_women_path, raw_facility_path = linked$raw_facility_path, department_lookup_path = file.path(input_dir, "lookups/senegal_health_district_department_evidence.csv"), output_dir = data_dir)
  if (html) prepare_display_map(boundary_path = file.path(assets_dir, "boundaries/senegal_departments.geojson"), data_dir = data_dir, assets_dir = assets_dir)
  residence_input <- if (nzchar(join_dir)) file.path(join_dir, "checked_geography/senegal_women_manitoba_urban_rural.rds") else
    prepare_residence_input("senegal", linked, input_dir, file.path(work_dir, "residence_linkage"))
  assign_residence_supply(input_path = residence_input, facility_path = linked$facility_path, raw_facility_path = linked$raw_facility_path, dictionary_path = linked$dictionary_path, data_dir = data_dir, output_dir = prep_dir, residence_lookup_path = file.path(input_dir, "inputs/SenegalURlookup.xlsx"))
  analytic_input <- file.path(prep_dir, "senegal_women_manitoba_rural_urban_fp.rds")
  fit_modern_use_models(data_dir = data_dir, output_dir = file.path(data_dir, "models_zscore"), score_method = "zscore", pca_score_file = "")
  pca_dir <- file.path(data_dir, "pca_appendix")
  calculate_pca_scores(data_dir = data_dir, output_dir = pca_dir)
  fit_modern_use_models(data_dir = data_dir, output_dir = file.path(pca_dir, "models"), score_method = "pca", pca_score_file = file.path(pca_dir, "pca_scores.rds"))
  validate_analysis(input_path = analytic_input, raw_women_path = linked$raw_women_path, facility_path = linked$facility_path, raw_facility_path = linked$raw_facility_path, data_dir = data_dir, cleaned_facility_path = linked$cleaned_facility_path, residence_lookup_path = file.path(input_dir, "inputs/SenegalURlookup.xlsx"))
  result <- if (html) render_report("senegal", "rural_urban", "Senegal_rural_urban_updated.html", "Senegal Rural/Urban analysis | SURGO × Manitoba",
    data_dir, work_dir, output_dir, resource_root, python) else list(data_dir = data_dir)
  jsonlite::write_json(list(country = "Senegal", aggregation = "rural_urban", html = html,
    report = if (html) "Senegal_rural_urban_updated.html" else NULL,
    result_tables = "report_data.json, heatmap_values_and_denominators.csv, all_method_scores_and_classifications.csv",
    model_results = "models_zscore/model_coefficients.csv",
    private_outputs = "Working joins and fitted model objects contain confidential data; do not upload."),
    file.path(output_dir, "RESULTS.json"), auto_unbox = TRUE, pretty = TRUE)
  message("Completed Senegal_rural_urban_updated; results: ", output_dir)
  result
}

if (run_analysis) {
  report_result <- build_senegal_rural_urban(root = .report_root, join_dir = cli_value("--join-dir"), cache_dir = cache_dir,
    work_dir = work_dir, python = cli_value("--python", Sys.getenv("SURGO_MANITOBA_PYTHON", "")),
    input_zip = input_zip, output_dir = output_dir, input_dir = input_dir, html = render_html)
}
