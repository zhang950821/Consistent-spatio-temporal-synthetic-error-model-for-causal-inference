# Model helpers ----------------------------------------------------------------

pstsem_fit_step_lm <- function(data, response = "y", predictors = NULL,
                               use_step = TRUE, trace = 0) {
  if (is.null(predictors)) {
    predictors <- setdiff(names(data), response)
  }
  model_data <- data[, c(response, predictors), drop = FALSE]
  fit <- stats::lm(stats::reformulate(predictors, response), data = model_data)
  if (use_step) {
    fit <- stats::step(fit, trace = trace)
  }
  fit
}

pstsem_make_residual_matrices <- function(training_data,
                                          validation_data = NULL,
                                          states = names(training_data),
                                          response = "y",
                                          predictors = NULL,
                                          validation_response_col = response,
                                          use_step = TRUE) {
  models <- list()
  train_residuals <- NULL
  valid_residuals <- NULL

  for (s in states) {
    fit <- pstsem_fit_step_lm(
      training_data[[s]],
      response = response,
      predictors = predictors,
      use_step = use_step
    )
    models[[s]] <- fit
    train_residuals <- cbind(train_residuals, stats::residuals(fit))

    if (!is.null(validation_data)) {
      pred <- stats::predict(fit, newdata = validation_data[[s]])
      valid_residuals <- cbind(valid_residuals, validation_data[[s]][[validation_response_col]] - pred)
    }
  }

  colnames(train_residuals) <- states
  if (!is.null(valid_residuals)) {
    colnames(valid_residuals) <- states
  }

  list(train = train_residuals, validation = valid_residuals, models = models)
}

pstsem_fit_adaptive_lasso <- function(residual_matrix,
                                      treated = "Kansas",
                                      alpha = 1,
                                      nfolds = 10,
                                      seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!treated %in% colnames(residual_matrix)) {
    stop("The treated unit column was not found: ", treated, call. = FALSE)
  }

  x <- as.matrix(residual_matrix[, colnames(residual_matrix) != treated, drop = FALSE])
  y <- as.matrix(residual_matrix[, treated, drop = FALSE])

  lm_data <- as.data.frame(residual_matrix)
  lm_fit <- stats::lm(stats::reformulate(setdiff(colnames(residual_matrix), treated), treated),
                      data = lm_data)
  lm_coef <- stats::coef(lm_fit)[-1]
  penalty <- 1 / abs(lm_coef)
  penalty[!is.finite(penalty)] <- max(penalty[is.finite(penalty)], 1) * 100

  cv_fit <- glmnet::cv.glmnet(
    x = x,
    y = y,
    type.measure = "mse",
    nfolds = nfolds,
    alpha = alpha,
    penalty.factor = penalty,
    keep = TRUE
  )

  fit <- glmnet::glmnet(
    x = x,
    y = y,
    alpha = alpha,
    penalty.factor = penalty,
    lambda = cv_fit$lambda.min
  )

  train_pred <- stats::predict(fit, s = cv_fit$lambda.min, newx = x)
  list(
    lm_fit = lm_fit,
    glmnet_fit = fit,
    cv_fit = cv_fit,
    lambda = cv_fit$lambda.min,
    coefficients = stats::coef(cv_fit, s = cv_fit$lambda.min),
    train_prediction = train_pred,
    train_mse = mean((y - train_pred)^2)
  )
}

pstsem_predict_adaptive_lasso <- function(fit, residual_matrix, treated = "Kansas") {
  x <- as.matrix(residual_matrix[, colnames(residual_matrix) != treated, drop = FALSE])
  stats::predict(fit$glmnet_fit, s = fit$lambda, newx = x)
}
