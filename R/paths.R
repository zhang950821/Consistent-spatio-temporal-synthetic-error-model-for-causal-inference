# Project path helpers ---------------------------------------------------------

pstsem_project_root <- function(start = getwd()) {
  env_root <- Sys.getenv("PSTSEM_PROJECT_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "PSTSEM.Rproj")) ||
        file.exists(file.path(current, "DESCRIPTION"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the PSTSEM project root from: ", start, call. = FALSE)
    }
    current <- parent
  }
}

pstsem_path <- function(..., root = pstsem_project_root()) {
  file.path(root, ...)
}

pstsem_data_path <- function(..., root = pstsem_project_root()) {
  pstsem_path("data", ..., root = root)
}

pstsem_results_path <- function(..., root = pstsem_project_root(), create = TRUE) {
  path <- pstsem_path("results", ..., root = root)
  if (create) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  path
}

pstsem_set_project_wd <- function(root = pstsem_project_root()) {
  old <- getwd()
  setwd(root)
  invisible(old)
}
