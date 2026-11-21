# Use a restored project library when available; do not install at startup.
local({
  if (dir.exists("renv/library")) {
    library <- file.path(getwd(), "renv", "library", paste0("R-", getRversion()), R.version$platform)
    if (dir.exists(library)) {
      .libPaths(c(library, .libPaths()))
      Sys.setenv(R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep))
    }
  }
})
