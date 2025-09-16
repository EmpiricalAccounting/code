# 第10章 キャッシュ・フロー計算書を用いた重回帰分析


# パッケージの読み込み
library(tidyverse)

# データの読み込み
financial_data <- read_csv("ch10_fundamental_analysis.csv")
head(financial_data)


# 条件に合う行だけを抽出
filtered_data <- financial_data |>
  filter(fiveyear_anadata == 1)

# 企業のライフサイクルステージを判定する
filtered_data <- financial_data |>
  mutate(
    # intro: ocf・icf が負、fcf が正
    intro = ocf < 0 & icf < 0 & fcf > 0,
    # growth: ocf・fcf が正、icf が負
    growth = ocf > 0 & fcf > 0 & icf < 0,
    # mature: icf・fcf が負、ocf が正
    mature = icf < 0 & fcf < 0 & ocf > 0,
    # decline: ocf が負、icf が正（fcf は不問）
    decline = ocf < 0 & icf > 0,
    # shakeout: intro・growth・mature・decline がすべて 0 のとき 1
    shakeout = intro == 0L & growth == 0L & mature == 0L & decline == 0L
  )

# それぞれのライフサイクルでの観測値の数をカウント
lifecycle_counts <- filtered_data |>
  summarise(intro    = sum(intro == 1, na.rm = TRUE),
            growth   = sum(growth == 1, na.rm = TRUE),
            mature   = sum(mature == 1, na.rm = TRUE),
            shakeout = sum(shakeout == 1, na.rm = TRUE),
            decline  = sum(decline == 1, na.rm = TRUE))
lifecycle_counts

# 変数の計算
filtered_data <- filtered_data |>
  mutate(roa = earnigns / total_asset,
         roa1 = safe_div(oi1, asset1),
         roa2 = safe_div(oi2, asset2),
    roa3 = safe_div(oi3, asset3),
    roa4 = safe_div(oi4, asset4),
    roa5 = safe_div(oi5, asset5),

    # 前年roa（roab1）
    roab1 = safe_div(oib1, assetb1),

    # Δroa
    delta_roa = roa - roab1, # 指定：roa − roab1
    delta_roa1 = roa1 - roa, # 指定：roa1 − roa
    delta_roa2 = roa2 - roa, # 指定：roa2 − roa
    delta_roa3 = roa3 - roa, # 指定：roa3 − roa
    delta_roa4 = roa4 - roa, # 指定：roa4 − roa
    delta_roa5 = roa5 - roa, # 指定：roa5 − roa

    # Δasset
    delta_asset = safe_div(asset - assetb1, assetb1),

    # ato
    ato = safe_div(sales, asset),
    atob1 = safe_div(salesb1, assetb1),
    delta_ato = ato - atob1,

    # pm
    pm = safe_div(oi, sales),
    pmb1 = safe_div(oib1, salesb1),
    delta_pm = pm - pmb1,

    # 年度ダミー
    # 年度をファクター型にすることで重回帰分析で年度ダミーとして扱われる
    year = as.factor(year)
  )

    # 正しいウィンサライズ処理（yearごとに分割して処理）
    # winsor()を使うためにpsychを準備
    # install.packages("psych")  # 1回だけでOK
    library(psych)
    library(dplyr)

    # 対象変数
    winsor_vars <- c(
      "delta_roa1","delta_roa2","delta_roa3","delta_roa4","delta_roa5",
      "roa","delta_roa","delta_asset","delta_ato","delta_pm"
    )

    # yearごとに上下1% winsorize（_w列として保存）
    full_data <- full_data |>
      group_by(year) |>
      mutate(
        across(
          any_of(winsor_vars),
          ~{
            v <- .
            # NAはそのまま、非NAのみwinsorize
            idx <- !is.na(v)
            if (any(idx)) v[idx] <- psych::winsor(as.numeric(v[idx]), trim = 0.01)
            v
          },
          .names = "{.col}_w"
        )
      ) |>
      ungroup()

    # 重回帰モデルの推定
    regression_vars <- c(
      "delta_roa1", "roa", "delta_roa", "delta_asset", "delta_ato", "delta_pm",
      "intro", "growth", "shakeout", "decline",
      paste0("dummy", 2002:2018)  # dummy2002〜dummy2018
    )

# 重回帰分析
model <- lm(delta_roa1 ~ roa + delta_roa + delta_asset + delta_ato + delta_pm +
            intro + growth + shakeout + decline + year,
            data = financial_data)

# 結果の表示
summary(model)

# パッケージの読み込み
library(modelsummary)

# 結果の表示
msummary(model,
         statistic = "statistic",
         star = TRUE,
         stars = c("*" = .10, "**" = .05, "***" = .01))
