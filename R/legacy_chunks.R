# Legacy chunk runner ----------------------------------------------------------

pstsem_chunk_manifest <- function(root = pstsem_project_root()) {
  read.csv(pstsem_path("scripts", "chunk_manifest.csv", root = root),
           stringsAsFactors = FALSE)
}

pstsem_chunk_file <- function(chunk, root = pstsem_project_root()) {
  manifest <- pstsem_chunk_manifest(root)
  idx <- match(chunk, manifest$chunk)
  if (is.na(idx)) {
    stop("Unknown chunk: ", chunk, call. = FALSE)
  }
  pstsem_path(manifest$file[[idx]], root = root)
}

pstsem_run_chunk <- function(chunk, env = .GlobalEnv, root = pstsem_project_root()) {
  file <- pstsem_chunk_file(chunk, root)
  old <- pstsem_set_project_wd(root)
  on.exit(setwd(old), add = TRUE)
  source(file, local = env, echo = FALSE)
  invisible(env)
}

pstsem_run_chunks <- function(chunks, env = .GlobalEnv, root = pstsem_project_root()) {
  for (chunk in chunks) {
    message("Running chunk: ", chunk)
    pstsem_run_chunk(chunk, env = env, root = root)
  }
  invisible(env)
}

pstsem_legacy_stages <- function() {
  list(
    setup = c("functions", "data preparasion"),
    empirical_pstsem = c(
      "vary-coefficient spatial temporal model",
      "kernel non-parametric model",
      "extract residuals for kernel",
      "synthetic error: kernel--linear",
      "residual from LST",
      "least square synthetic error",
      "lasso synthetic error",
      "adaptive lasso syntehtic error--validation part",
      "empirical results by adaptive LASSO--prediction part"
    ),
    empirical_core = c(
      "vary-coefficient spatial temporal model",
      "residual from LST",
      "least square synthetic error",
      "lasso synthetic error",
      "adaptive lasso syntehtic error--validation part",
      "empirical results by adaptive LASSO--prediction part"
    ),
    kernel_smoke = c("kernel non-parametric model"),
    baselines = c("synthetic control and ASCM", "Robust synthetic control"),
    plots = c("plot", "ggplot"),
    multi_step = c("multi-step-forward empirical experiment"),
    spatial_skip = c("spatial smooth skip-step-model empirical experiment"),
    simulation = c(
      "simulation study LSTSEM--data generating",
      "simulation main",
      "1step prediction result",
      "implement all"
    ),
    multithreading = c("Multithreading"),
    interference = c("interference test"),
    spatial_correlation = c("spatial correlation test"),
    diagnostics = c("interference test", "spatial correlation test")
  )
}

pstsem_run_legacy_stage <- function(stage,
                                    env = .GlobalEnv,
                                    root = pstsem_project_root()) {
  stages <- pstsem_legacy_stages()
  if (identical(stage, "all")) {
    chunks <- unlist(stages, use.names = FALSE)
  } else {
    chunks <- stages[[stage]]
  }
  if (is.null(chunks)) {
    stop("Unknown legacy stage: ", stage, call. = FALSE)
  }
  pstsem_run_chunks(chunks, env = env, root = root)
}
