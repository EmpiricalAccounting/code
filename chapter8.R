# パッケージのインストール
# 初回は必要（コメントアウトを解除して実行）
#install.packages("tidyverse")

# パッケージの読み込み
library(tidyverse)


# （1）データの概要 ---------------------------------------------------------------

# データの読み込み
# データはコードファイルと同じディレクトリに置く
# 株価
stock_price <- read_csv("08_stock_price.csv")
stock_price

# インデックス
index <- read_csv("08_index.csv")
index

# 利益と決算発表日
earnings <- read_csv("08_earnings.csv")
earnings

earnings |>
  distinct(firm_id) |>
  count()

# （2）増益企業と減益企業の判定 ---------------------------------------------------------------

# 増益か減益かの判定
earning_change <- earnings |>
  arrange(firm_id, year) |>
  group_by(firm_id) |>
  mutate(
    diff_earnings   = earnings - lag(earnings, 1),
    earning_change = diff_earnings > 0
  ) |>
  ungroup() |>
  drop_na(earning_change)

# 数の確認
earning_change |>
  group_by(earning_change) |>
  count()


# （3）相対日数の設定 ---------------------------------------------------------------

# 通算日数の表示
dates <- stock_price |>
  select(date) |>
  distinct(date) |>
  arrange(date) |>
  mutate(date_number = row_number())

# 開始と終了時点の判定
start_end <-
  dates |>

  # さしあたりリストの日付と番号をアナウンスメント日とする
  rename(announce_date   = date,
         announce_number = date_number) |>

  # アナウンスメント前後の期間の最初と最後の番号
  mutate(start_number = announce_number - 240,
         end_number   = announce_number + 120)

# 決算発表日からの相対日数を各日付ごとに適用
dates_announce <-
  dates |>
  inner_join(start_end, join_by(between(date_number, start_number, end_number))) |>
  mutate(relative_date = date_number - announce_number) |>
  select(announce_date, relative_date, date)

# 相対日数と利益変化と株価とインデックスの結合
sample_data <- earning_change |>
  left_join(dates_announce, by = "announce_date") |>
  left_join(stock_price, by = c("firm_id", "date")) |>
  left_join(index, by = "date")

# 確認
sample_data |>
  filter(firm_id == 1, year == 2023) |>
  select(announce_date, relative_date, date, earning_change, price, index) |>
  print(n = Inf)


# （4）株式リターンの計算 ---------------------------------------------------------------

# 株式リターンの計算
sample_return <- sample_data |>
  arrange(firm_id, date) |>
  group_by(firm_id, year) |>
  mutate(return       = (price - lag(price, 1)) / lag(price, 1),
         index_return = (index - lag(index, 1)) / lag(index, 1),
         return       = replace_na(return, 0),
         index_return = replace_na(index_return, 0)) |>
  ungroup()

# 確認
sample_return |>
  filter(firm_id == 1, year == 2023, relative_date == 0) |>
  select(return, index_return)


# （5）異常リターンの計算 ---------------------------------------------------------------

# 異常リターンの計算
sample_ar <- sample_return |>
  mutate(ar = return - index_return)

# 確認（1社）
sample_ar |>
  filter(firm_id == 1, year == 2023, relative_date == 0) |>
  select(ar)

# 確認（平均値）
sample_ar |>
  filter(relative_date == 0) |>
  group_by(earning_change) |>
  summarise(mean(ar))

t.test(sample_ar |>
         filter(relative_date == 0, earning_change == T) |> pull(ar),
       mu = 0)

t.test(sample_ar |>
         filter(relative_date == 0, earning_change == F) |> pull(ar),
       mu = 0)


# （6）累積異常リターンの計算とグラフの作成 ---------------------------------------------------------------

# 累積異常リターンの計算
sample_car <- sample_ar |>
  group_by(firm_id, year) |>
  arrange(date) |>
  mutate(car = cumsum(ar)) |>
  ungroup()

# 確認
sample_car |>
  filter(firm_id == 1, year == 2023, relative_date == 0) |>
  select(car) |>
  print()

# 累積異常リターンの平均値
sample_car_mean <- sample_car |>
  group_by(earning_change, relative_date) |>
  summarise(mean_car = mean(car, na.rm = TRUE),
            .groups = 'drop')

# 確認
sample_car_mean |>
  filter(relative_date == 0)

# グラフの作成
sample_car_mean |>
  mutate(`Earnings Change` = if_else(earning_change, "Good", "Bad"),
         `Earnings Change` = factor(`Earnings Change`, levels = c("Good", "Bad"))) |>
  ggplot(aes(x = relative_date, y = mean_car, linetype = `Earnings Change`)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  labs(x = "Relative Date", y = "CAR") +
  theme(legend.position = "bottom") +
  theme_classic()
