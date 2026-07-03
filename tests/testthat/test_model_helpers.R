testthat::test_that("adaptive lasso synthetic error helper fits and predicts", {
  set.seed(1)
  residual_matrix <- matrix(rnorm(80 * 6), nrow = 80)
  colnames(residual_matrix) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "Kansas", "California")
  residual_matrix[, "Kansas"] <- residual_matrix[, "Alabama"] * 0.4 +
    residual_matrix[, "Arizona"] * -0.2 + rnorm(80, sd = 0.2)

  fit <- pstsem_fit_adaptive_lasso(residual_matrix, treated = "Kansas", nfolds = 5, seed = 123)
  pred <- pstsem_predict_adaptive_lasso(fit, residual_matrix, treated = "Kansas")

  testthat::expect_true(is.finite(fit$train_mse))
  testthat::expect_equal(nrow(pred), nrow(residual_matrix))
})

testthat::test_that("stepwise lm helper returns a model", {
  fit <- pstsem_fit_step_lm(mtcars, response = "mpg", predictors = c("wt", "hp"), use_step = FALSE)
  testthat::expect_s3_class(fit, "lm")
})
