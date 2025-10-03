# 4章 Rでのデータ分析（基礎）


# リセット
rm(list = ls())

# 四則演算
1 + 1
5 - 2
2 * 3
8 / 2

# tidyverseパッケージのインストール（初回）
install.packages(c("tidyverse", "car", "psych", "modelsummary"))

# tidyverseパッケージのインストール（インストール完了後は，# でコメント・アウト）
# install.packages(c("tidyverse", "car", "psych", "modelsummary"))

# パッケージの読み込み
library(tidyverse)

# データの読み込み
financial_data <- read_csv("ch04_financial_data.csv")

# 読み込んだデータの確認
head(financial_data)

# 因子型への変更
financial_data <- financial_data |> 
  mutate(firm_id             = as.factor(firm_id),
         firm_name           = as.factor(firm_name),
         stock_code          = as.factor(stock_code),
         industry_name       = as.factor(industry_name),
         stock_market        = as.factor(stock_market),
         accounting_standard = as.factor(accounting_standard))

# 記述統計
summary(financial_data)

# ROEの三分解を計算して，結果をfinancial_dataに追加する
# 第1引数を直接書く場合
financial_data <- mutate(financial_data,
                         roe                = earnings / equity,
                         profit_margin      = earnings / sales,
                         asset_turnover     = sales / total_assets,
                         financial_leverage = total_assets / equity)

# パイプ演算子を使用する場合
financial_data <- financial_data |>
  mutate(roe                = earnings / equity,
         profit_margin      = earnings / sales,
         asset_turnover     = sales / total_assets,
         financial_leverage = total_assets / equity)

# ROEの三分解の計算結果を表示
# パイプ演算子を使用する場合
financial_data |>
  select(firm_name, year, roe, profit_margin, asset_turnover, financial_leverage) |>
  print()

# パイプ演算子を使用しない場合
print(select(financial_data,
             firm_name, year, roe, profit_margin, asset_turnover, financial_leverage))
