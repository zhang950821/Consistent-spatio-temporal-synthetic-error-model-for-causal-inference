testthat::test_that("simulation helper returns panel and unit lists", {
  states <- c("Kansas", "Alabama", "Alaska", "Arizona")
  W <- matrix(0.25, nrow = 4, ncol = 4, dimnames = list(states, states))
  diag(W) <- 0
  location <- data.frame(
    name = states,
    latitude = c(39, 33, 61, 34),
    longitude = c(-98, -86, -150, -111)
  )
  coefficients <- matrix(0.01, nrow = 4, ncol = 23)
  coefficients[, 1] <- 0
  coefficients[, 7] <- 0.2

  sim <- pstsem_simulate_panel(
    seed = 42,
    weight_matrix = W,
    coefficients = coefficients,
    states = states,
    location = location,
    n = 70,
    burn_in = 10
  )

  testthat::expect_length(sim$units, 4)
  testthat::expect_true(all(c("state", "timepoint", "y") %in% names(sim$panel)))
  testthat::expect_equal(nrow(sim$panel), 4 * 60)
})

testthat::test_that("Moran diagnostics run by time point", {
  states <- c("Kansas", "Alabama", "Alaska", "Arizona")
  W <- matrix(0.25, nrow = 4, ncol = 4, dimnames = list(states, states))
  diag(W) <- 0
  y_table <- expand.grid(state = states, timepoint = 1:3)
  y_table$y <- seq_len(nrow(y_table))

  result <- pstsem_moran_by_time(y_table, W)

  testthat::expect_equal(nrow(result), 3)
  testthat::expect_true(all(c("estimate", "p_value") %in% names(result)))
})
