# 5章

# ライブラリの読み込み
library(dplyr)

# データ読み込み
sp_df <- read.csv("05_stock_price_data.csv", fileEncoding = "UTF-8")

# 複合キー（code と year）で結合
merged_df <- inner_join(fin_df, sp_df, by = c("code", "year"))

# 不要な列の削除（name.x と name.y を統合し、name という新しい列を作る）
cleaned_df <- merged_df |>
  mutate(name = coalesce(name.x, name.y)) |>
  select(number, year, name, code, industry, market, standard, div, so, sale, ni, na, ta, roe, sp)

# 結果の表示
head(cleaned_df)

# EPS（1株あたり当期純利益）とBPS（1株あたり純資産）を計算して列を追加
cleaned_df <- cleaned_df |>
  mutate(
    eps = ni / so,   # EPS（1株あたり当期純利益の計算）
    bps = na / so    # BPS（1株あたり純資産の計算）
  )

# 必要ならCSVに保存
write.csv(cleaned_df, "cleaned_df.csv", row.names = FALSE)

# epsと株価の相関係数の計算
cor(cleaned_df$eps, cleaned_df$sp, use = "complete.obs")

# 相関係数を計算する対象の変数を選択
cor_data <- cleaned_df[, c("eps", "bps", "sp", "sale", "ni")]

# 相関係数表の作成
cor_matrix <- cor(cor_data, use = "complete.obs")

# 結果を表示
print(cor_matrix)

# 単回帰分析の実行
model <- lm(sp ~ eps, data = cleaned_df)

# 結果の表示
summary(model)

# 重回帰分析の実行
model_multiple <- lm(sp ~ eps + bps, data = cleaned_df)

# 結果の表示
summary(model_multiple)

# carパッケージの読み込み
library(car)

# VIFの計算
vif(model_multiple)

# ロジスティック回帰分析

# TRUEまたはFALSE を 1または0 に変換
df <- cleaned_df |>
  mutate(div_binary = as.numeric(div))

# ロジスティック回帰モデル
model_logistic <- glm(div_binary ~ ni, data = df, family = binomial)

# 結果の表示
summary(model_logistic)

# オッズ比の計算
exp(coef(model_logistic))
