# Source chunk: ggplot
# Original Rmd lines: 1553-1659

#SC vs MSC vs PSTSEM vs GSC-------------------------------------------
# 时间点向量
timepoints <- 60:105

# 先确认长度
n_total <- length(timepoints)  # 46
n_pred <- length(post_SC_lngdpcapita_series)  # 17

# 确定预测起点：timepoint = 89，是 timepoints 中的第 (89 - 60 + 1) = 30 个
pred_start_index <- which(timepoints == 89)

# 用 NA 补齐前面的值，使预测长度对齐为 46
pad_na <- function(x, total_len, start_index) {
  c(rep(NA, start_index - 1), x, rep(NA, total_len - length(x) - (start_index - 1)))
}

# 对 SC， mSC 和 PSTSEM 做 padding
SC_full <- pad_na(post_SC_lngdpcapita_series, n_total, pred_start_index)
mSC_full <- pad_na(post_mSC_lngdpcapita_series, n_total, pred_start_index)
RSC_full <- pad_na(post_RSC_lngdpcapita_series, n_total, pred_start_index)
PSTSEM_full <- pad_na(post_se_lngdpcapita_series_plt, n_total, pred_start_index)
GSC_full<-pad_na(post_gsynth_lngdpcapita_series, n_total, pred_start_index)


# 构造数据框
df <- data.frame(
  TimePoint = timepoints,
  Observed = obs_kansas_lngdpcapita,
  SC = SC_full,
  STPSEM = PSTSEM_full,
  MSC=mSC_full,
  RSC=RSC_full,
  GSC=GSC_full
  
)

# 转为长格式
df_long <- pivot_longer(df, cols = -TimePoint, names_to = "Model", values_to = "Value")

# 画图
ggplot(df_long, aes(x = TimePoint, y = Value, color = Model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Observed" = "black", "SC" = "red", "STPSEM" = "blue", "MSC" = "brown", "RSC"="purple", "GSC"="orange")
                     ) +
  labs(y = "Kansas ln(GDP) per capita", x = "Time Point") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_vline(xintercept = 89, linetype = "dashed", color = "gray40") +
  annotate("text", x = 89, y = max(df$Observed, na.rm = TRUE), label = "Intervention", vjust = -0.5, angle = 90, size = 3)
# LST vs PSTSEM vs NPSTSEM-------------------------------------------
# pad 各预测序列
LST_full <- pad_na(post_LST_lngdpcapita_series, n_total, pred_start_index)
PSTSEM_full <- pad_na(post_se_lngdpcapita_series_plt, n_total, pred_start_index)
NPSTSEM_full <- pad_na(post_LSE_se_lngdpcapita_series_plt, n_total, pred_start_index)

# 时间点向量
timepoints <- 60:105

df1 <- data.frame(
  TimePoint = timepoints,
  Observed = obs_kansas_lngdpcapita,
  LST = LST_full,
  STPSEM = PSTSEM_full,
  STSEM = NPSTSEM_full
)

df1_long <- pivot_longer(df1, cols = -TimePoint, names_to = "Model", values_to = "Value")

ggplot(df1_long, aes(x = TimePoint, y = Value, color = Model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Observed" = "black", "LST" = "red", "STPSEM" = "blue", "STSEM" = "green")) +
  labs( y = "Kansas ln(GDP) per capita", x = "Time Point") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_vline(xintercept = 89, linetype = "dashed", color = "gray40") +
  annotate("text", x = 89, y = max(df1$Observed, na.rm = TRUE), label = "Intervention", angle = 90, vjust = -0.5, size = 3)


#---------------------------ASCM vs PSTSEM----------or ST-ASCM vs PSTSEM----------------------------------
ASCM_full <- pad_na(post_ASCM_lngdpcapita_series, n_total, pred_start_index)
ST_ASCM_full <- pad_na(post_ST_ASCM_lngdpcapita_series, n_total, pred_start_index)


PSTSEM_full <- pad_na(post_se_lngdpcapita_series_plt, n_total, pred_start_index)

df4 <- data.frame(
  TimePoint = timepoints,
  Observed = obs_kansas_lngdpcapita,
  ASCM = ASCM_full,
  STPSEM = PSTSEM_full,
  ST_ASCM=ST_ASCM_full
)

df4_long <- pivot_longer(df4, cols = -TimePoint, names_to = "Model", values_to = "Value")

ggplot(df4_long, aes(x = TimePoint, y = Value, color = Model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Observed" = "black", "ASCM" = "red", "STPSEM" = "blue", "ST_ASCM"="yellow")) +
  labs( y = "Kansas ln(GDP) per capita", x = "Time Point") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_vline(xintercept = 89, linetype = "dashed", color = "gray40") +
  annotate("text", x = 89, y = max(df4$Observed, na.rm = TRUE), label = "Intervention", angle = 90, vjust = -0.5, size = 3)


