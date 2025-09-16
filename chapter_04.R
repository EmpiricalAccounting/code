# 4章 Rでのデータ分析（基礎）


# リセット
rm(list = ls())

# 四則演算
1 + 1
5 - 2
2 * 3
8 / 2

# データの読み込み
financial_data <- read_csv("ch04_financial_data.csv")

# 読み込んだデータの確認
head(financial_data)

# 記述統計
summary(financial_data)

# tidyverseパッケージのインストール（初回）
install.packages(c("tidyverse", "car", "psych", "modelsummary"))

# tidyverseパッケージのインストール（インストール完了後は、# でコメント・アウト）
# install.packages(c("tidyverse", "car", "psych", "modelsummary"))

# パッケージの読み込み
library(tidyverse)

# ROEの三分解を計算して、結果を financial_dataに追加する（第1引数を直接書く場合）
financial_data <- mutate(financial_data,
                         roe                = earnings / equity,
                         profit_margin      = earnings / sales,
                         asset_turnover     = sales / total_assets,
                         financial_leverage = total_assets / equity)

# ROEの三分解を計算して、結果を financial_data, に追加する（パイプ演算子を使用する場合）
financial_data <- financial_data |>
  mutate(roe                = earnings / equity,
         profit_margin      = earnings / sales,
         asset_turnover     = sales / total_assets,
         financial_leverage = total_assets / equity)

# ROEの三分解の計算結果を表示（パイプ演算子を使用する場合）
financial_data |>
  select(firm_name, year, roe, profit_margin, asset_turnover, financial_leverage) |>
  print()

# ROEの三分解の計算結果を表示（パイプ演算子を使用しない場合）
print(select(financial_data,
             firm_name, year, roe, profit_margin, asset_turnover, financial_leverage))
