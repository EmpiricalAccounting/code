# 第12章 IFRS適用の決定要因分析

# パッケージの読み込み
library(tidyverse)


# 第3節　変数の計算と平均値の比較 ---------------------------------------------------------------

# （1）データの読み込み ---------------------------------------------------------------

# データの読み込み
# データはコードファイルと同じディレクトリに置く
financial <- read_csv("determinant.csv")
financial

# ifrsはダミー変数なのでファクター型にしておく
financial <- financial |>
  mutate(ifrs = as.factor(ifrs))

# IFRS適用企業と日本基準適用企業の観測値の数
table(financial$ifrs)


# （2）変数の計算 ---------------------------------------------------------------

# 変数の計算
financial <- financial |>
  mutate(ratio_goodwill       = goodwill / assets,
         ratio_rd             = rd / sales,
         ratio_foreign_sales  = foreign_sales / sales,
         ratio_foreign_shares = foreign_shares * shares_unit / shares_outstanding,
         size                 = log(assets),
         leverage             = liabilities / assets,
         roa                  = earnings / assets)

# 記述統計量の表示
summary(financial)


# （3）IFRSと日本基準の平均値の差の検定 ---------------------------------------------------------------

# のれん
t.test(ratio_goodwill ~ ifrs, data = financial)

# 研究開発費
t.test(ratio_rd ~ ifrs, data = financial)

# 海外売上高比率
t.test(ratio_foreign_sales ~ ifrs, data = financial)

# 外国人持株比率
t.test(ratio_foreign_shares ~ ifrs, data = financial)

# 企業規模
t.test(size ~ ifrs, data = financial)

# レバレッジ
t.test(leverage ~ ifrs, data = financial)

# ROA
t.test(roa ~ ifrs, data = financial)


# （4）ロジット分析 ---------------------------------------------------------------

# ロジット分析の実施
result_logit <- glm(ifrs ~ ratio_goodwill + ratio_rd + ratio_foreign_sales + ratio_foreign_shares
                     + size + leverage + roa,
                     data = financial,
                     family = binomial(link = "logit"))


# modelsummaryパッケージの読み込み
library(modelsummary)

# 結果の表示
msummary(result_logit,
         statistic = "statistic",
         star = TRUE,
         stars = c("*" = .10, "**" = .05, "***" = .01))

