# 第8章 株価と利益情報を用いたイベントスタディ


# パッケージの読み込み
library(tidyverse)

# データの読み込み
# 株価
stock_data <- read_csv("ch08_stock_data.csv")
head(stock_data)

# インデックス
index_data <- read_csv("ch08_index_data.csv")
head(index_data)

# 利益と決算発表日
earnings_data <- read_csv("ch08_earnings_data.csv")
head(earnings_data)

# 増益企業と減益企業の判定
earnings_change <- earnings_data |>
  arrange(firm_id, year) |>
  group_by(firm_id) |>
  mutate(diff_earnings   = earnings - lag(earnings, 1),
         earnings_change = diff_earnings > 0) |>
  ungroup() |>
  drop_na(earnings_change)

# 増益と減益の数の確認
earnings_change |>
  group_by(earnings_change) |>
  count()

# 通算日数のリスト化
dates <- stock_data |>
  select(date) |>
  distinct(date) |>
  arrange(date) |>
  mutate(date_number = row_number())

# 開始時点と終了時点の判定
start_end <- dates |>
  # 通算日数リストの日付と番号をアナウンスメント日とする
  rename(announce_date = date,
         announce_number = date_number) |>
  # アナウンスメント前後の期間の最初と最後の番号
  mutate(start_number = announce_number - 240,
         end_number = announce_number + 120)

# 決算発表日からの相対日数を各日付ごとに適用
dates_announce <- dates |>
  inner_join(start_end, join_by(between(date_number, start_number, end_number))) |>
  mutate(relative_date = date_number - announce_number) |>
  select(announce_date, relative_date, date)

# 相対日数と利益データと株価データとインデックスデータの結合
sample_data <- earnings_change |>
  left_join(dates_announce, by = "announce_date", relationship = "many-to-many") |>
  left_join(stock_data, by = c("firm_id", "date")) |>
  left_join(index_data, by = "date")

# firm_idが1、yearが2023のデータを確認
sample_data |>
  filter(firm_id == 1, year == 2023) |>
  select(announce_date, relative_date, date, earnings_change, stock_price, index) |>
  print(n = Inf)

# 株式リターンの計算
sample_return <- sample_data |>
  arrange(firm_id, date) |>
  group_by(firm_id, year) |>
  mutate(return       = (stock_price - lag(stock_price, 1)) / lag(stock_price, 1),
         index_return = (index - lag(index, 1)) / lag(index, 1),
         return       = replace_na(return, 0),
         index_return = replace_na(index_return, 0)) |>
  ungroup()

# firm_idが1、yearが2023、relative_dateが0のデータについて、
# 株式リターンとインデックスリターンの確認
sample_return |>
  filter(firm_id == 1, year == 2023, relative_date == 0) |>
  select(return, index_return)

# 異常リターン（AR）の計算
sample_ar <- sample_return |>
  mutate(ar = return - index_return)

# firm_idが1、yearが2023、relative_dateが0のデータについて、
# ARの確認
sample_ar |>
  filter(firm_id == 1, year == 2023, relative_date == 0) |>
  select(ar)

# 決算発表日でのARの平均値の計算
sample_ar |>
  filter(relative_date == 0) |>
  summarise(mean_ar = mean(ar), .by = earnings_change)

# 累積異常リターン（CAR）の計算
sample_car <- sample_ar |>
  arrange(firm_id, date) |>
  group_by(firm_id, year) |>
  mutate(car = cumsum(ar)) |>
  ungroup()

# firm_idが1、yearが2023、relative_dateが0のデータについて、
# CARの確認
sample_car |>
  filter(firm_id == 1, year == 2023, relative_date == 0) |>
  select(car)

# 増益減益それぞれでのCARの相対日数ごとの平均値
sample_car_mean <- sample_car |>
  summarise(mean_car = mean(car),
            .by = c(earnings_change, relative_date))

# 決算発表日でのCARの平均値の確認
sample_car_mean |>
  filter(relative_date == 0)

# CARのグラフの作成
sample_car_mean |>
  # 増益（Good）と減益（Bad）のラベルを作って順番を調整
  mutate(earnings_change_label = if_else(earnings_change, "Good", "Bad"),
         earnings_change_label = factor(earnings_change_label, levels = c("Good", "Bad"))) |>
  ggplot() +
  geom_line(aes(x = relative_date, y = mean_car, linetype = earnings_change_label)) +
  # CAR＝0に横線を引く
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  # 0日に縦線を引く
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  labs(title    = "Market Reaction Analysis",
       x        = "Relative Date",
       y        = "CAR",
       linetype = "Earnings Change") +
  theme(legend.position = "bottom") +
  theme_classic()
