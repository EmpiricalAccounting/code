# パッケージのインストール
# 初回は必要（コメントアウトを解除して実行）
#install.packages("tidyverse")

# パッケージの読み込み
library(tidyverse)


# 第3節　変数の計算と平均値の比較 ---------------------------------------------------------------

# （1）データの読み込み ---------------------------------------------------------------

# データの読み込み
# データはコードファイルと同じディレクトリに置く
financial <- read_csv("12_determinant.csv")
financial

table(financial$year)


# ifrsはダミー変数なのでファクター型にしておく
financial <- financial |>
  mutate(ifrs = as.factor(ifrs))

# IFRS適用企業と日本基準適用企業の観測値の数
table(financial$ifrs)

xtabs(~ year + ifrs, financial)

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

# IFRS適用企業と日本基準適用企業を分ける
financial_ifrs <- financial |>
  filter(ifrs == 1)

financial_jpn <- financial |>
  filter(ifrs == 0)

# のれん
t.test(financial_ifrs$ratio_goodwill, financial_jpn$ratio_goodwill)

# 研究開発費
t.test(financial_ifrs$ratio_rd, financial_jpn$ratio_rd)

# 海外売上高比率
t.test(financial_ifrs$ratio_foreign_sales, financial_jpn$ratio_foreign_sales)

# 外国人持株比率
t.test(financial_ifrs$ratio_foreign_shares, financial_jpn$ratio_foreign_shares)

# 企業規模
t.test(financial_ifrs$size, financial_jpn$size)

# レバレッジ
t.test(financial_ifrs$leverage, financial_jpn$leverage)

# ROA
t.test(financial_ifrs$roa, financial_jpn$roa)


# （4）ロジット分析 ---------------------------------------------------------------

# ロジット分析の実施
result_logit <- glm(ifrs ~ ratio_goodwill + ratio_rd + ratio_foreign_sales + ratio_foreign_shares
                     + size + leverage + roa,
                     data = financial,
                     family = binomial(link = "logit"))


# modelsummaryパッケージのインストール
# 初回は必要（コメントアウトを解除して実行）
#install.packages("modelsummary")

# パッケージの読み込み
library(modelsummary)

# 結果の表示
msummary(result_logit,
         statistic = "statistic",
         star = TRUE,
         stars = c("*" = .10, "**" = .05, "***" = .01))
