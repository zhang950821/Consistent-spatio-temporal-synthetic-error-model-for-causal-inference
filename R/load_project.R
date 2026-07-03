# Source this file to load the project helper API.

.pstsem_load_project_file <- {
  frames <- sys.frames()
  ofiles <- vapply(frames, function(x) {
    if (is.null(x$ofile)) NA_character_ else x$ofile
  }, character(1))
  ofiles <- stats::na.omit(ofiles)
  if (length(ofiles)) {
    normalizePath(ofiles[[length(ofiles)]], winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(file.path(getwd(), "R", "load_project.R"),
                  winslash = "/", mustWork = FALSE)
  }
}

pstsem_source_project <- function(root = dirname(dirname(.pstsem_load_project_file)),
                                  load_packages = FALSE) {
  files <- c(
    "R/paths.R",
    "R/packages.R",
    "R/data_preparation.R",
    "R/model_helpers.R",
    "R/simulation_helpers.R",
    "R/diagnostics.R",
    "R/plotting.R",
    "R/legacy_chunks.R"
  )
  for (file in files) {
    source(file.path(root, file), local = .GlobalEnv)
  }
  if (isTRUE(load_packages)) {
    pstsem_load_packages()
  }
  invisible(root)
}
