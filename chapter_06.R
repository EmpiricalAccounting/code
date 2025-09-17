# 第6章 記述統計を用いた利益分布アプローチ


# パッケージの読み込み
library(tidyverse)

# データの読み込み
financial_data <- read_csv("ch06_benchmark_analysis.csv")
head(financial_data)

# benchmarkのデータベースにROAとWACの変数の作成
financial_data <- financial_data |>
  mutate(roa = earnings / lag_total_assets,
         wac = working_capital / lag_total_assets)

# ROAのヒストグラムの作成（
# ビンは灰色で黒枠，範囲は-0.1から0.1，必ず[0,0.008]を通る
financial_data |>
  ggplot() +
  geom_histogram(aes(x = roa),
                 binwidth = 0.008, boundary = 0,
                 fill = "lightgray", color = "black") +
  scale_x_continuous(limits = c(-0.1, 0.1)) +
  labs(title = "Histogram of ROA", x = "ROA", y = "Count") +
  theme_classic()

# ROAのヒストグラムで[0,0.008]のビンの色を黒色にする
financial_data |>
  mutate(highlight = between(roa, 0, 0.008)) |>
  ggplot(aes(x = roa, fill = highlight)) +
  geom_histogram(aes(x = roa, fill = highlight),
                 binwidth = 0.008, boundary = 0, color = "black") +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "lightgray"),
                    guide = "none") +
  scale_x_continuous(limits = c(-0.1, 0.1)) +
  labs(title = "Histogram of ROA", x = "ROA", y = "Count") +
  theme_classic()

# 区切り点を作成する
breaks <- seq(-0.1, 0.1, by = 0.008) |>
  c(0) |>
  sort()

# ROAをビン幅に区分する
financial_data <- financial_data |>
  mutate(bin = cut(roa, breaks = breaks, include.lowest = TRUE, labels = FALSE))

# 四分位点をビン幅ごとに計算
bin_quantiles <- financial_data |>
  group_by(bin) |>
  summarize(q_25  = quantile(wac, 0.25, na.rm = TRUE),
            q_50  = quantile(wac, 0.5, na.rm = TRUE),
            q_75  = quantile(wac, 0.75, na.rm = TRUE),
            .groups = "drop") |>
  # 区間の中央値
  mutate(bin_mid = (breaks[as.integer(bin)] + breaks[as.integer(bin) + 1]) / 2)

# ビン幅ごとの四分位点の図表を表示
bin_quantiles |>
  ggplot(aes(bin_mid)) +
  geom_line(aes(y = q_25, size = "25%"), color = "black") +
  geom_line(aes(y = q_50, size = "50%"), color = "black") +
  geom_line(aes(y = q_75, size = "75%"), color = "black") +
  # 0.004に縦線を引く
  geom_vline(xintercept = 0.004, color = "black", linetype = "dashed", linewidth = 1) +
  scale_size_manual(name = "Quantile",values = c("25%" = 0.5, "50%" = 1.2, "75%" = 2)) +
  labs(title = "Quartile of Working Capital per ROA Bin",
       x     = "ROA Bin (median)",
       y     = "Warking Capital") +
  theme_classic()
