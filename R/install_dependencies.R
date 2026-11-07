# Restore the recorded R dependency versions, or install current CRAN versions.
args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org")
if ("--latest" %in% args) {
  library <- renv::paths$library(project = root)
  dir.create(library, recursive = TRUE, showWarnings = FALSE)
  install.packages(c("rmarkdown", "knitr", "haven", "dplyr", "readr", "readxl", "jsonlite", "digest",
    "sf", "survey", "clubSandwich", "ggplot2", "svglite"), lib = library, repos = "https://cloud.r-project.org")
} else {
  renv::restore(project = root, lockfile = file.path(root, "renv.lock"), prompt = FALSE)
}
