# Interface checks: no confidential inputs or report refits are required.
# Packages: jsonlite (recorded in renv.lock). R packages install inside report entry points.
entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- normalizePath(file.path(dirname(entry), ".."), mustWork = TRUE)
files <- c(list.files(file.path(root, "aggregate"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
           list.files(file.path(root, "rural_urban"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE))
stopifnot(length(files) == 7L)
lock <- jsonlite::read_json(file.path(root, "renv.lock"))
for (file in files) {
  module <- new.env(parent = globalenv())
  source(file, local = module)
  stopifnot(!module$run_analysis, module$render_html, module$install_missing_packages,
    identical(eval(formals(module$select_input_zip)$chooser), base::file.choose),
    identical(module$.report_root, root), !length(module$.report_steps),
    all(module$required_packages %in% names(module$locked_environment$Packages)))
  for (package in names(module$locked_environment$Packages)) {
    stopifnot(identical(module$locked_environment$Packages[[package]]$Version,
                        lock$Packages[[package]]$Version))
  }
  for (i in seq_len(nrow(module$report_resources))) {
    path <- file.path(root, module$report_resources$file[i])
    stopifnot(digest::digest(file = path, algo = "sha256") == module$report_resources$sha256[i])
  }
  local({
    zip <- tempfile(fileext = ".zip")
    file.create(zip)
    on.exit(unlink(zip))
    selected <- module$select_input_zip(function() zip)
    stopifnot(identical(selected, normalizePath(zip)))
  })
  cancelled <- try(module$select_input_zip(function() stop("cancel")), silent = TRUE)
  stopifnot(inherits(cancelled, "try-error"))
}
# Quoting uses the platform's R implementation; exercise Windows spaces explicitly.
stopifnot(identical(shQuote("input files/bundle.zip", type = "cmd"), '"input files/bundle.zip"'))
cat("PASS: seven independent R entry points, embedded package versions, native picker, public resource checksums and spaced Windows argument quoting.\n")
