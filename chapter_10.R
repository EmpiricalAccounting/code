# 第10章 キャッシュ・フロー計算書を用いた重回帰分析


# パッケージの読み込み
library(tidyverse)

# データの読み込み
financial_data <- read_csv("ch10_fundamental_analysis.csv")
head(financial_data)

# 企業のライフサイクルステージを判定する
financial_data <- financial_data |>
  mutate(
    # intro: ocf・icf が負，fcf が正
    intro = ocf < 0 & icf < 0 & fcf > 0,
    # growth: ocf・fcf が正、icf が負
    growth = ocf > 0 & icf < 0 & fcf > 0,
    # mature: ocf が正，icf・fcf が負
    mature =  ocf > 0 & icf < 0 & fcf < 0,
    # decline: ocf が負，icf が正（fcf は不問）
    decline = ocf < 0 & icf > 0,
    # shakeout: intro・growth・mature・decline がすべて 0 のとき 1
    shakeout = intro == 0 & growth == 0 & mature == 0 & decline == 0
  )

# それぞれのライフサイクルでの観測値の数をカウント
lifecycle_counts <- financial_data |>
  summarise(intro    = sum(intro == 1, na.rm = TRUE),
            growth   = sum(growth == 1, na.rm = TRUE),
            mature   = sum(mature == 1, na.rm = TRUE),
            shakeout = sum(shakeout == 1, na.rm = TRUE),
            decline  = sum(decline == 1, na.rm = TRUE))
lifecycle_counts

# 変数の計算
financial_data <- financial_data |>
  arrange(firm_id, year) |>
  group_by(firm_id) |>
  mutate(roa                  = operating_income / total_assets,
         delta_roa            = roa - lag(roa, 1),
         lead_1_delta_roa     = lead(delta_roa, 1),
         lead_2_delta_roa     = lead(delta_roa, 2),
         delta_assets         = (total_assets - lag(total_assets, 1)) / total_assets,
         asset_turnover       = sales / total_assets,
         delta_asset_turnover = asset_turnover - lag(asset_turnover, 1),
         profit_margin        = operating_income / sales,
         delta_profit_margin  = profit_margin - lag(profit_margin, 1),

         # 年度ダミー
         # 年度をファクター型にすることで重回帰分析で年度ダミーとして扱われる
         year = as.factor(year)
  ) |>
  ungroup()

# 記述統計量の確認
summary(financial_data)

# 重回帰分析
# 従属変数：1期先のROA
model_lead_1 <- lm(lead_1_delta_roa ~ roa + delta_roa + delta_assets + delta_asset_turnover
                   + delta_profit_margin + intro + growth + shakeout + decline + year,
                   data = financial_data)

# 結果の表示：1期先のROA
summary(model_lead_1)

# 従属変数：2期先のROA
model_lead_2 <- lm(lead_2_delta_roa ~ roa + delta_roa + delta_assets + delta_asset_turnover
                   + delta_profit_margin + intro + growth + shakeout + decline + year,
                   data = financial_data)

# 結果の表示：2期先のROA
summary(model_lead_2)

# パッケージの読み込み
library(modelsummary)

# msummary()による結果の表示
# list()で結果を並べることで複数の結果を並べることができる
msummary(list(model_lead_1, model_lead_2),
         # 表示したい係数を指定する
         coef_map = c("roa", "delta_roa", "delta_assets", "delta_asset_turnover",
                      "delta_profit_margin", "introTRUE", "growthTRUE", "shakeoutTRUE",
                      "declineTRUE", "(Intercept)"),
         # t値を表示する
         statistic = "statistic",
         # 有意の星をつける
         star = TRUE,
         # 有意水準と星の数を設定する
         stars = c("*" = .10, "**" = .05, "***" = .01),
         # 観測値の数と自由度調整済み決定係数を表示させる
         gof_map = c("nobs", "adj.r.squared"))
