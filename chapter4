# 4章

# リセット
rm(list = ls())

# 四則演算
1 + 1
5 - 2
2 * 3
8 / 2

# データの読み込み
fin_df <- read.csv("04_financial_data.csv")

# 読み込んだデータの確認
head(fin_df)

# 記述統計
summary(fin_df)

# tidyverseパッケージのインストール（初回）
install.packages(c("tidyverse", "car", "psych", "modelsummary"))

# tidyverseパッケージのインストール（インストール完了後は、# でコメント・アウト）
# install.packages(c("tidyverse", "car", "psych", "modelsummary"))

# パッケージの読み込み
library(tidyverse)

# ROEの三分解を計算して、結果を fin_df に追加する（第1引数を直接書く場合）
fin_df <- mutate(
  fin_df,
  roe = ni / na,
  profit_margin = ni / sale,
  asset_turnover = sale / ta,
  financial_leverage = ta / na
)

# ROEの三分解を計算して、結果を fin_df に追加する（パイプ演算子を使用する場合）
fin_df <- fin_df |> 
  mutate(
    roe = ni / na,
    profit_margin = ni / sale,
    asset_turnover = sale / ta,
    financial_leverage = ta / na
  )

# ROEの三分解の計算結果を表示（パイプ演算子を使用する場合）
fin_df |> 
  select(name, year, roe, profit_margin, asset_turnover, financial_leverage) |> 
  print()

# ROEの三分解の計算結果を表示（パイプ演算子を使用しない場合）
print(
  select(
    fin_df,
    name, year, roe, profit_margin, asset_turnover, financial_leverage
  )
)
