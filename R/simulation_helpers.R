# Simulation helpers -----------------------------------------------------------

pstsem_lag <- function(x, n = 1L) {
  if (n <= 0) {
    return(x)
  }
  c(rep(NA, n), head(x, -n))
}

pstsem_generate_phi <- function(location, n_covariates = 8) {
  out <- matrix(NA_real_, nrow = nrow(location), ncol = n_covariates)
  for (i in seq_len(nrow(location))) {
    for (j in seq_len(n_covariates)) {
      out[i, j] <- sin(j) * cos(location[i, "longitude"] + location[i, "latitude"]) / 10
    }
  }
  out
}

pstsem_generate_common_phi <- function(n_covariates = 4) {
  0.5 + 0.2 * sin(seq_len(n_covariates))
}

pstsem_simulate_panel <- function(seed,
                                  weight_matrix,
                                  coefficients,
                                  states = rownames(weight_matrix),
                                  location = NULL,
                                  n = 300,
                                  burn_in = 50,
                                  sigma_common = 0.1,
                                  sigma_error = 0.01,
                                  n_private_covariates = 8,
                                  n_common_covariates = 4) {
  if (is.null(states)) {
    states <- paste0("unit_", seq_len(nrow(weight_matrix)))
  }
  set.seed(seed)
  m <- length(states)
  W <- weight_matrix
  W[!is.finite(W)] <- 0

  if (is.null(location)) {
    location <- data.frame(longitude = seq_len(m), latitude = seq_len(m))
  }
  phi <- pstsem_generate_phi(location, n_private_covariates)
  common_phi <- pstsem_generate_common_phi(n_common_covariates)

  common_x <- matrix(NA_real_, nrow = n, ncol = n_common_covariates)
  for (j in seq_len(n_common_covariates)) {
    common_x[, j] <- as.numeric(stats::arima.sim(
      model = list(ar = common_phi[j]),
      n = n,
      sd = sigma_common
    ))
  }

  x <- array(NA_real_, dim = c(n, m, n_private_covariates))
  for (j in seq_len(n_private_covariates)) {
    sigma <- matrix(0, m, m)
    for (i1 in seq_len(m)) {
      for (i2 in seq_len(m)) {
        sigma[i1, i2] <- if (i1 == i2) 1 else sin((1 / W[i1, i2]) * j)
      }
    }
    sigma[!is.finite(sigma)] <- 0
    sigma_pd <- sigma %*% sigma / max(m, 1)
    innovations <- MASS::mvrnorm(n, mu = rep(0, m), Sigma = sigma_pd)
    for (i in seq_len(m)) {
      x[, i, j] <- as.numeric(stats::arima.sim(
        n = n,
        model = list(ar = phi[i, j]),
        innov = innovations[, i]
      ))
    }
  }

  coef_matrix <- as.matrix(coefficients)
  if (nrow(coef_matrix) != m) {
    stop("coefficients must have one row per state/unit.", call. = FALSE)
  }
  if (ncol(coef_matrix) < 11 + n_private_covariates + n_common_covariates) {
    stop("coefficients does not contain enough columns for the configured covariates.",
         call. = FALSE)
  }

  y <- matrix(0, nrow = n, ncol = m)
  real_y <- matrix(0, nrow = n, ncol = m)
  ysl <- matrix(0, nrow = n, ncol = m)

  for (t in 6:n) {
    for (i in seq_len(m)) {
      beta <- coef_matrix[i, ]
      ysl[t - 1, i] <- sum(W[i, ] * y[t - 1, ])
      y_lags <- y[(t - 1):(t - 5), i]
      ysl_lags <- ysl[(t - 1):(t - 5), i]
      private_cov <- x[t, i, seq_len(n_private_covariates)]
      common_cov <- common_x[t, seq_len(n_common_covariates)]
      linear_part <- beta[1] +
        sum(beta[2:6] * ysl_lags) +
        sum(beta[7:11] * y_lags) +
        sum(beta[12:(11 + n_private_covariates)] * private_cov) +
        sum(beta[(12 + n_private_covariates):(11 + n_private_covariates + n_common_covariates)] * common_cov)
      err <- stats::rnorm(1, mean = 0, sd = sigma_error)
      y[t, i] <- linear_part + err
      real_y[t, i] <- linear_part + err
    }
  }

  units <- vector("list", m)
  names(units) <- states
  for (i in seq_len(m)) {
    df <- data.frame(
      y = y[, i],
      y_lag1 = pstsem_lag(y[, i], 1),
      y_lag2 = pstsem_lag(y[, i], 2),
      y_lag3 = pstsem_lag(y[, i], 3),
      y_lag4 = pstsem_lag(y[, i], 4),
      y_lag5 = pstsem_lag(y[, i], 5),
      ysl = ysl[, i],
      ysl_lag1 = pstsem_lag(ysl[, i], 1),
      ysl_lag2 = pstsem_lag(ysl[, i], 2),
      ysl_lag3 = pstsem_lag(ysl[, i], 3),
      ysl_lag4 = pstsem_lag(ysl[, i], 4),
      ysl_lag5 = pstsem_lag(ysl[, i], 5)
    )
    for (j in seq_len(n_private_covariates)) {
      df[[paste0("x", j)]] <- x[, i, j]
    }
    for (j in seq_len(n_common_covariates)) {
      df[[paste0("comm_x", j)]] <- common_x[, j]
    }
    units[[i]] <- df[(burn_in + 1):n, , drop = FALSE]
  }

  panel <- do.call(rbind, lapply(seq_along(units), function(i) {
    cbind(state = states[i], timepoint = seq_len(nrow(units[[i]])), units[[i]])
  }))
  rownames(panel) <- NULL

  list(units = units, panel = panel, y = y, real_y = real_y, ysl = ysl, x = x)
}

pstsem_split_unit_series <- function(unit_data, T0, horizon = 20) {
  list(
    pre = unit_data[seq_len(T0), , drop = FALSE],
    post = unit_data[T0 + 1, , drop = FALSE],
    post_series = unit_data[(T0 + 1):(T0 + horizon), , drop = FALSE]
  )
}
