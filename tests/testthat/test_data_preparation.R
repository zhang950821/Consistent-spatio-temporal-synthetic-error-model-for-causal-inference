testthat::test_that("empirical data loads into decoupled lists", {
  empirical_data <- pstsem_load_empirical_data()

  testthat::expect_length(empirical_data$state, 50)
  testthat::expect_true("Kansas" %in% empirical_data$state)
  testthat::expect_s3_class(empirical_data$training_data$Kansas, "data.frame")
  testthat::expect_s3_class(empirical_data$validation_data$Kansas, "data.frame")
  testthat::expect_equal(dim(empirical_data$W), c(50, 50))
  testthat::expect_true(all(diag(empirical_data$W) == 0))
  testthat::expect_true(all(colnames(empirical_data$W) == empirical_data$state))
})

testthat::test_that("legacy environment export keeps original object names", {
  env <- new.env(parent = globalenv())
  empirical_data <- pstsem_load_empirical_data()
  pstsem_export_empirical_data(empirical_data, env = env)

  testthat::expect_true(exists("state", envir = env))
  testthat::expect_true(exists("Kansas_training_data", envir = env))
  testthat::expect_true(exists("Kansas_validation_data_mod", envir = env))
  testthat::expect_equal(ncol(get("W", envir = env)), 50)
})
