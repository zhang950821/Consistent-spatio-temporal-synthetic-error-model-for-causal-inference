# Source chunk: spatial correlation test
# Original Rmd lines: 3623-3756

W_std<-W


for (i in c(1:50)){
  W_std[i,]<-W[i,]/sum(W[i,])
}



y_table_total<-diff_kansas_total_mod[,c('state', 'timepoint', 'y')]

y_table_total<-y_table_total[y_table_total$timepoint<=89,]

W_std<-as.matrix(W_std)

listw_Wstd<-mat2listw(W_std, style = "W")


library(spdep)
library(ggplot2)




times <- sort(unique(y_table_total$timepoint))

moran_results <- data.frame(
  timepoint = times,
  moran_I = NA,
  expectation = NA,
  variance = NA,
  statistic = NA,
  p_value = NA
)

for (k in seq_along(times)) {
  
  tt <- times[k]
  
  data_t <- y_table_total[y_table_total$timepoint == tt, ]
  
  # 按照 W_std 的州名顺序排列
  data_t <- data_t[match(colnames(W_std), data_t$state), ]
  
  # 检查该时间点是否所有州都齐全
  if (any(is.na(data_t$state))) {
    stop(paste("Some states are missing at timepoint", tt))
  }
  
  if (!all(data_t$state == colnames(W_std))) {
    stop(paste("State order mismatch at timepoint", tt))
  }
  
  test <- moran.test(
    x = data_t$y,
    listw = listw_Wstd,
    zero.policy = TRUE
  )
  
  moran_results$moran_I[k] <- as.numeric(test$estimate["Moran I statistic"])
  moran_results$expectation[k] <- as.numeric(test$estimate["Expectation"])
  moran_results$variance[k] <- as.numeric(test$estimate["Variance"])
  moran_results$statistic[k] <- as.numeric(test$statistic)
  moran_results$p_value[k] <- test$p.value
}

moran_results

moran_results$significant_5pct <- moran_results$p_value < 0.05
moran_results$significant_1pct <- moran_results$p_value < 0.01

moran_results


ggplot(moran_results, aes(x = timepoint, y = moran_I)) +
  geom_line() +
  geom_point(aes(shape = significant_5pct), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Time point",
    y = "Moran's I",
    shape = "p < 0.05",
    title = "Moran's I Test for Spatial Autocorrelation over Time"
  )





moran_results$significance_label <- ifelse(
  moran_results$p_value < 0.05,
  "p < 0.05",
  "p > 0.05"
)

moran_results$significance_label <- factor(
  moran_results$significance_label,
  levels = c("p > 0.05", "p < 0.05")
)

ggplot(moran_results, aes(x = timepoint, y = moran_I)) +
  geom_line() +
  geom_point(aes(shape = significance_label), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_shape_manual(
    values = c("p > 0.05" = 16, "p < 0.05" = 17)
  ) +
  theme_bw() +
  labs(
    x = "Time point",
    y = "Moran's I",
    shape = "",
    title = "Moran's I Test for Spatial Autocorrelation over Time"
  )





summary_table <- data.frame(
  total_timepoints = nrow(moran_results),
  mean_moran_I = mean(moran_results$moran_I, na.rm = TRUE),
  median_moran_I = median(moran_results$moran_I, na.rm = TRUE),
  min_moran_I = min(moran_results$moran_I, na.rm = TRUE),
  max_moran_I = max(moran_results$moran_I, na.rm = TRUE),
  significant_5pct_count = sum(moran_results$p_value < 0.05, na.rm = TRUE),
  significant_1pct_count = sum(moran_results$p_value < 0.01, na.rm = TRUE)
)

summary_table

