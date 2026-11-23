# Restore the recorded R environment
#
# Required R packages (restore once: Rscript setup.R):
#   renv, haven, dplyr, readr, readxl, jsonlite, digest, sf, survey, clubSandwich, ggplot2,
#   svglite, tidyselect, rmarkdown, knitr, base64enc.
# Exact package versions: renv.lock. Project dependencies: DESCRIPTION.
# setup.R uses renv to restore the recorded project package environment.
# stats, utils, tools, grid, graphics and grDevices are bundled with R where used.
#
# Software metadata (no survey or facility datasets are read):
#   DESCRIPTION
#   renv.lock
#
# Run once from the project root: Rscript setup.R. Restores the package versions recorded in
#   renv.lock; --latest intentionally selects a different environment.
# This installer creates the project package library and checks versions. Python 3/Pandoc and any
#   spatial system dependencies must be installed separately.
# Full filename inventory: docs/DATASET_FILENAMES.md; roles/checksums: docs/INPUTS.md.
# Input locations and credentials are runtime parameters, never saved personal paths.

args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- normalizePath(dirname(script), mustWork = TRUE)
library <- file.path(root, "renv", "library", paste0("R-", getRversion()), R.version$platform)
dir.create(library, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(library) || file.access(library, 2L) != 0L) stop("Run setup from a writable project folder.")
.libPaths(c(library, .libPaths()))
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", lib = library, repos = "https://cloud.r-project.org")
}
if ("--latest" %in% args) {
  install.packages(c("rmarkdown", "knitr", "haven", "dplyr", "readr", "readxl", "jsonlite", "digest",
    "sf", "survey", "clubSandwich", "ggplot2", "svglite", "base64enc", "tidyselect"), lib = library, repos = "https://cloud.r-project.org")
} else {
  # Reuse only already-installed versions that exactly match the lockfile.
  # This is a software cache; missing versions are still restored from CRAN.
  lock <- renv::lockfile_read(file.path(root, "renv.lock"))
  matching <- names(lock$Packages)[vapply(names(lock$Packages), function(k)
    tryCatch(utils::packageVersion(k) == package_version(lock$Packages[[k]]$Version),
             error = function(e) FALSE), logical(1))]
  if (length(matching)) renv::hydrate(project = root, library = library,
    packages = matching, sources = .libPaths(), prompt = FALSE)
  renv::restore(project = root, library = library,
    lockfile = file.path(root, "renv.lock"), prompt = FALSE)
  .libPaths(c(library, .libPaths()))
  wrong <- names(lock$Packages)[!vapply(names(lock$Packages), function(k)
    tryCatch(utils::packageVersion(k) == package_version(lock$Packages[[k]]$Version),
             error = function(e) FALSE), logical(1))]
  if (length(wrong)) stop("Dependency versions still differ from the lockfile: ", paste(wrong, collapse = ", "))
  cat("Restored and verified ", length(lock$Packages), " locked package versions in the project environment.\n", sep = "")
}
