# Restore the recorded R dependency versions, or install current CRAN versions.
args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org")
library <- file.path(root, "renv", "library", paste0("R-", getRversion()), R.version$platform)
dir.create(library, recursive = TRUE, showWarnings = FALSE)
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
