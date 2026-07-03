# Plot helpers -----------------------------------------------------------------

pstsem_pad_na <- function(x, total_len, start_index) {
  out <- rep(NA_real_, total_len)
  end_index <- min(total_len, start_index + length(x) - 1)
  out[start_index:end_index] <- as.numeric(x)[seq_len(end_index - start_index + 1)]
  out
}

pstsem_plot_prediction_paths <- function(data,
                                         time_col = "TimePoint",
                                         value_col = "Value",
                                         model_col = "Model",
                                         intervention_time = NULL) {
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[time_col]], y = .data[[value_col]], color = .data[[model_col]])
  ) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = "Time", y = value_col, color = NULL)

  if (!is.null(intervention_time)) {
    plot <- plot + ggplot2::geom_vline(xintercept = intervention_time, linetype = "dashed")
  }
  plot
}
