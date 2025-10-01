# 第12章 IFRS適用の決定要因分析


# パッケージの読み込み
library(tidyverse)

# データの読み込み
financial_data <- read_csv("ch12_determinant_analysis.csv")
head(financial_data)

# IFRS適用企業と日本基準適用企業の観測値の数
table(financial_data$accounting_standard)

# 変数の計算
financial_data <- financial_data |>
  mutate(ifrs                 = if_else(accounting_standard == "IFRS", 1, 0),
         ratio_goodwill       = goodwill / total_assets,
         ratio_rd             = research_development / sales,
         ratio_foreign_sales  = foreign_sales / sales,
         ratio_foreign_shares = foreign_ownership * shares_unit / shares_outstanding,
         size                 = log(total_assets),
         leverage             = liabilities / total_assets,
         roa                  = earnings / total_assets)

# 記述統計量の表示
summary(financial_data)

# IFRSと日本基準の平均値の差の検定
# のれん
t.test(ratio_goodwill ~ ifrs, data = financial_data)

# 研究開発費
t.test(ratio_rd ~ ifrs, data = financial_data)

# 海外売上高比率
t.test(ratio_foreign_sales ~ ifrs, data = financial_data)

# 外国人持株比率
t.test(ratio_foreign_shares ~ ifrs, data = financial_data)

# 企業規模
t.test(size ~ ifrs, data = financial_data)

# レバレッジ
t.test(leverage ~ ifrs, data = financial_data)

# ROA
t.test(roa ~ ifrs, data = financial_data)

# ロジット分析の実施
model_logit <- glm(ifrs ~ ratio_goodwill + ratio_rd + ratio_foreign_sales
                    + ratio_foreign_shares + size + leverage + roa,
                     data = financial_data,
                     family = binomial(link = "logit"))

# modelsummaryパッケージの読み込み
library(modelsummary)

# 分析結果の表示
msummary(model_logit,
         # z値を表示する
         statistic = "statistic",
         # 有意の星をつける
         star = TRUE,
         # 有意水準と星の数を設定する
         stars = c("*" = .10, "**" = .05, "***" = .01),
         # 観測値の数を表示させる
         gof_map = "nobs")
