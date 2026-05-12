# Working directoryを適宜設定すること
# setwd("~/Dropbox/Projects/Iran")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# csv読み込む
data <- read.csv("iran.csv")
data$date <- as.Date(data$date)

# long形式のデータにする(日次・累積)
daily <- data %>%
  select(date, uav_daily, ballistic_daily, cruise_daily) %>%
  pivot_longer(-date, names_to = "type", values_to = "daily")

cum <- data %>%
  select(date, uav_cum, ballistic_cum, cruise_cum) %>%
  pivot_longer(-date, names_to = "type", values_to = "cum")

# 日本語
daily$type <- recode(daily$type,
                     uav_daily = "ドローン",
                     ballistic_daily = "弾道ミサイル",
                     cruise_daily = "巡航ミサイル")

cum$type <- recode(cum$type,
                   uav_cum = "ドローン",
                   ballistic_cum = "弾道ミサイル",
                   cruise_cum = "巡航ミサイル")

# 日次・累積データの結合
plot_data <- left_join(daily, cum, by = c("date", "type"))

# スケール因子（2軸のスケール調整）
scale_factor <- max(plot_data$cum) / max(plot_data$daily)

# 作図
p <- ggplot(plot_data, aes(x = date)) +
  
  # 累積（棒グラフ，右）
  geom_col(aes(y = cum / scale_factor, fill = type),
           alpha = 0.3,
           width = 0.8,
           position = "identity") +
  
  # 日次（折れ線，左）
  geom_line(aes(y = daily, color = type),
            linewidth = 1.2) +
  
  geom_point(aes(y = daily, color = type),
             size = 2.5) +
  
  geom_text(
    aes(y = daily, label = daily, color = type),
    vjust = -0.6,
    size = 3,
    show.legend = FALSE
  ) +
  
  # 2軸のラベル
  scale_y_continuous(
    name = "日次",
    labels = comma,
    sec.axis = sec_axis(~ . * scale_factor, name = "累積値")
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("2026-02-28"),
      max(plot_data$date),
      by = "2 day"
    ),
    date_labels = "%m-%d"
  ) +

  
  scale_color_manual(values = c(
    "ドローン" = "#1f77b4",
    "弾道ミサイル" = "#d62728",
    "巡航ミサイル" = "#2ca02c"
  )) +
  
  scale_fill_manual(values = c(
    "ドローン" = "#1f77b4",
    "弾道ミサイル" = "#d62728",
    "巡航ミサイル" = "#2ca02c"
  )) +
  
  labs(
    title = "イランからUAEへの攻撃数の時系列推移（日次・累積）",
    subtitle = "左:日次攻撃数（折れ線） 右:累積攻撃数（棒グラフ）",
    x = "",
    color = "種類",
    fill = "種類",
    caption = "Source：UAE国防省公式X(@modgovae)に基づき張作成"
  ) +
  
  # 日本語
  theme_minimal(base_size = 24, base_family = "Hiragino Sans W4") +
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p)

ggsave(
  filename = "iran_attacks.png",
  plot = p,
  width = 12,
  height = 9,
  dpi = 600,
  bg = "white"
)