# Package management -----------------------------------------------------------

pstsem_required_packages <- function(include_optional = TRUE) {
  core <- c(
    "dplyr", "tidyr", "ggplot2", "glmnet", "MASS", "mgcv", "np",
    "Synth", "gsynth", "augsynth", "spdep", "foreach", "doParallel",
    "iterators", "gstat", "patchwork", "naniar", "forecast", "tseries",
    "FinTS", "spacetime", "xts", "haven", "Rcpp", "varycoef", "knitr",
    "rmarkdown"
  )
  optional <- c("testthat")
  unique(if (include_optional) c(core, optional) else core)
}

pstsem_check_packages <- function(packages = pstsem_required_packages(),
                                  stop_on_missing = TRUE) {
  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  result <- data.frame(package = packages, installed = installed, row.names = NULL)
  missing <- result$package[!result$installed]

  if (length(missing) && stop_on_missing) {
    stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      "\nInstall them before running the full workflows.",
      call. = FALSE
    )
  }

  result
}

pstsem_load_packages <- function(packages = pstsem_required_packages(FALSE)) {
  pstsem_check_packages(packages, stop_on_missing = TRUE)
  invisible(lapply(packages, library, character.only = TRUE))
}
