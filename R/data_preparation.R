# Data loading and spatial weights --------------------------------------------

pstsem_distance <- function(u1, u2,
                            latitude_col = "latitude",
                            longitude_col = "longitude") {
  sqrt((u1[[latitude_col]] - u2[[latitude_col]])^2 +
         (u1[[longitude_col]] - u2[[longitude_col]])^2)
}

pstsem_distance_matrix <- function(locations,
                                   latitude_col = "latitude",
                                   longitude_col = "longitude",
                                   state_col = "name") {
  stopifnot(is.data.frame(locations))
  coords <- as.matrix(locations[, c(latitude_col, longitude_col)])
  out <- as.matrix(stats::dist(coords, method = "euclidean", upper = TRUE, diag = TRUE))
  states <- locations[[state_col]]
  rownames(out) <- states
  colnames(out) <- states
  out
}

pstsem_inverse_distance_weights <- function(locations,
                                            standardize = FALSE,
                                            latitude_col = "latitude",
                                            longitude_col = "longitude",
                                            state_col = "name") {
  dmat <- pstsem_distance_matrix(
    locations,
    latitude_col = latitude_col,
    longitude_col = longitude_col,
    state_col = state_col
  )
  weights <- 1 / dmat
  diag(weights) <- 0
  weights[!is.finite(weights)] <- 0
  if (standardize) {
    rs <- rowSums(weights)
    weights <- weights / ifelse(rs == 0, 1, rs)
  }
  weights
}

pstsem_add_diff_covariates <- function(data, reference_row = NULL) {
  diff_specs <- c(
    totalwagescapita_dif = "totalwagescapita",
    emplvlcapita_dif = "emplvlcapita",
    popestimate_dif = "popestimate",
    qtrly_estabs_count_dif = "qtrly_estabs_count"
  )

  out <- data
  for (new_col in names(diff_specs)) {
    raw_col <- diff_specs[[new_col]]
    if (!raw_col %in% names(out)) {
      next
    }
    out[[new_col]] <- NA_real_
    if (!is.null(reference_row)) {
      out[[new_col]][1] <- out[[raw_col]][1] / reference_row[[raw_col]][1] - 1
      start_index <- 2
    } else {
      start_index <- 2
    }
    if (nrow(out) >= start_index) {
      for (i in start_index:nrow(out)) {
        out[[new_col]][i] <- out[[raw_col]][i] / out[[raw_col]][i - 1] - 1
      }
    }
  }
  out
}

pstsem_load_empirical_data <- function(data_dir = pstsem_data_path()) {
  kansas <- read.csv(file.path(data_dir, "kansas.csv"), header = TRUE)
  kansas_raw <- read.csv(file.path(data_dir, "kansas_raw.csv"))
  location <- read.csv(file.path(data_dir, "us-state-capitals.csv"))
  states <- unique(location$name)

  training_dir <- file.path(data_dir, "total_matrix", "diff_using_60train_data")
  all_diff_dir <- file.path(data_dir, "total_matrix", "all_diff_data")

  training_data <- setNames(lapply(states, function(s) {
    read.csv(file.path(training_dir, paste0(s, ".csv")))
  }), states)

  training_data_mod <- lapply(training_data, function(df) {
    pstsem_add_diff_covariates(df)[-1, , drop = FALSE]
  })

  pre_treat_data <- setNames(lapply(states, function(s) {
    read.csv(file.path(all_diff_dir, paste0(s, "_matrix_pre_and_post_mod.csv")))
  }), states)

  validation_data <- lapply(pre_treat_data, function(df) df[72:83, , drop = FALSE])

  validation_data_mod <- Map(function(valid, train) {
    pstsem_add_diff_covariates(valid, reference_row = train[70, , drop = FALSE])
  }, validation_data, training_data)

  state_dataframe <- cbind(index = seq_len(nrow(location)), location)
  W <- pstsem_inverse_distance_weights(location)

  list(
    kansas = kansas,
    kansas_raw = kansas_raw,
    location = location,
    state = states,
    state_dataframe = state_dataframe,
    W = W,
    training_data = training_data,
    training_data_mod = training_data_mod,
    validation_data = validation_data,
    validation_data_mod = validation_data_mod,
    pre_treat_data = pre_treat_data
  )
}

pstsem_export_empirical_data <- function(empirical_data, env = .GlobalEnv) {
  assign("kansas", empirical_data$kansas, envir = env)
  assign("kansas_raw", empirical_data$kansas_raw, envir = env)
  assign("location", empirical_data$location, envir = env)
  assign("state", empirical_data$state, envir = env)
  assign("state_dataframe", empirical_data$state_dataframe, envir = env)
  assign("W", empirical_data$W, envir = env)

  for (s in empirical_data$state) {
    assign(paste0(s, "_training_data"), empirical_data$training_data[[s]], envir = env)
    assign(paste0(s, "_training_data_mod"), empirical_data$training_data_mod[[s]], envir = env)
    assign(paste0(s, "_validation_data"), empirical_data$validation_data[[s]], envir = env)
    assign(paste0(s, "_validation_data_mod"), empirical_data$validation_data_mod[[s]], envir = env)
    assign(paste0(s, "_pre_treat_data"), empirical_data$pre_treat_data[[s]], envir = env)
  }

  invisible(empirical_data)
}

pstsem_prepare_legacy_environment <- function(data_dir = pstsem_data_path(),
                                             env = .GlobalEnv) {
  empirical_data <- pstsem_load_empirical_data(data_dir)
  pstsem_export_empirical_data(empirical_data, env)
  invisible(empirical_data)
}

# Compatibility names used by the original Rmd chunks.
distance <- pstsem_distance

distance_matrix_func <- function(m = NULL, state_dataframe = NULL) {
  if (is.data.frame(m)) {
    state_dataframe <- m
  }
  if (is.null(state_dataframe)) {
    state_dataframe <- get("state_dataframe", envir = parent.frame())
  }
  pstsem_distance_matrix(state_dataframe)
}
