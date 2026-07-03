# Diagnostics ------------------------------------------------------------------

pstsem_moran_by_time <- function(y_table,
                                 W,
                                 state_col = "state",
                                 time_col = "timepoint",
                                 value_col = "y") {
  states <- rownames(W)
  if (is.null(states)) {
    states <- unique(y_table[[state_col]])
    rownames(W) <- states
    colnames(W) <- states
  }
  listw <- spdep::mat2listw(W, style = "W")
  times <- sort(unique(y_table[[time_col]]))

  rows <- lapply(times, function(tt) {
    temp <- y_table[y_table[[time_col]] == tt, , drop = FALSE]
    temp <- temp[match(states, temp[[state_col]]), , drop = FALSE]
    test <- spdep::moran.test(temp[[value_col]], listw, zero.policy = TRUE)
    data.frame(
      timepoint = tt,
      estimate = unname(test$estimate[["Moran I statistic"]]),
      expected = unname(test$estimate[["Expectation"]]),
      variance = unname(test$estimate[["Variance"]]),
      statistic = unname(test$statistic),
      p_value = test$p.value
    )
  })

  do.call(rbind, rows)
}
