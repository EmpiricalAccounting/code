# 5章 Rでのデータ分析（応用）


# 第1節 Keyの接続 --------------------------------------------------------------

# ライブラリの読み込み
library(tidyverse)

# "ch04_financial_data.csv"の読み込み
financial_data <- read_csv("ch04_financial_data.csv")

# "ch05_stock_data.csv"の読み込み
stock_data <- read_csv("ch05_stock_data.csv")

# 複合キー（code と year）で結合
join_data <- inner_join(financial_data, stock_data, by = c("stock_code", "year"))

# 不要な列の削除
clean_data <- join_data |>
  # name.x と name.y を統合し、name という新しい列を作る
  mutate(firm_name = coalesce(firm_name.x, firm_name.y)) |>
  # 必要な列だけを選択
  select(firm_id, year, firm_name, stock_code, industry_name, stock_market,
         accounting_standard, dividend, stock_outstanding, sales, earnings,
         equity, total_assets, stock_price)

# 結果の表示
head(clean_data)

# EPS（1株あたり当期純利益）とBPS（1株あたり純資産）を計算して列を追加
clean_data <- clean_data |>
  mutate(eps = earnings / stock_outstanding, # EPS（1株あたり当期純利益の計算）
         bps = equity / stock_outstanding )  # BPS（1株あたり純資産の計算）


# 必要ならCSVに保存
write_csv(clean_data, "ch05_clean_data.csv")


# 第2節 推測統計の基礎 -------------------------------------------------------------

# epsと株価の相関係数の計算
cor(clean_data$eps, clean_data$stock_price, use = "complete.obs")

# 相関係数を計算する対象の変数を選択
correlation_data <- clean_data |>
  select(eps, bps, stock_price, sales, earnings)

# 相関係数表の作成
correlation_matrix <- cor(cor_data, use = "complete.obs")

# 結果を表示
print(cor_matrix)

# 単回帰分析の実行
model_single <- lm(stock_price ~ eps, data = clean_data)

# 結果の表示
summary(model_single)


# 第3節 推測統計の応用 -------------------------------------------------------------

# 重回帰分析の実行
model_multiple <- lm(stock_price ~ eps + bps, data = clean_data)

# 結果の表示
summary(model_multiple)

# carパッケージの読み込み
library(car)

# VIFの計算
vif(model_multiple)

# ロジスティック回帰分析

# TRUEまたはFALSE を1または0に変換
clean_data <- clean_data |>
  mutate(dividend_binary = as.numeric(dividend))

# ロジスティック回帰モデル
model_logistic <- glm(dividend_binary ~ earnings, data = clean_data, family = binomial)

# 結果の表示
summary(model_logistic)

# オッズ比の計算
exp(coef(model_logistic))
