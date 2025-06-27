# Evaluate portfolio performance in high vs low volatility regimes
#
# This script loads portfolio backtest results created in the
# backtesting_weights_keras_sp500_set.Rmd vignette. It computes
# Sharpe ratio and information ratio separately for periods of
# high and low market volatility (using the `vol_mkt` predictor).
#
# Usage:
#   source("R/Testing/performance_by_volatility.R")
#
# The script expects that two objects are available on disk:
#   1. combined_portfolios.RData        (contains `combined_portfolios`)
#   2. sp500_m_signals1.RData           (contains `sp500_m_signals1`)
# Adjust the paths below if the files are stored elsewhere.

library(dplyr)
library(PerformanceAnalytics)
library(xts)

# Load portfolio object and feature data
load("combined_portfolios.RData")      # loads `combined_portfolios`
load("sp500_m_signals1.RData")         # loads `sp500_m_signals1`

# Compute market volatility per date
vol_df <- sp500_m_signals1 %>%
  group_by(date) %>%
  summarise(vol_mkt = mean(vol_mkt, na.rm = TRUE), .groups = "drop")

# Merge volatility with portfolio and benchmark returns
ret_df <- combined_portfolios$portfolio_returns %>%
  left_join(vol_df, by = "date") %>%
  left_join(combined_portfolios$benchmark_returns %>%
              rename(benchmark = benchmark_return),
            by = "date")

# Define high and low volatility regimes using the median vol_mkt
threshold <- median(ret_df$vol_mkt, na.rm = TRUE)
ret_df <- ret_df %>%
  mutate(vol_regime = if_else(vol_mkt >= threshold, "High", "Low"))

# Identify portfolio columns
portfolio_cols <- setdiff(names(combined_portfolios$portfolio_returns), "date")

calc_metrics <- function(df) {
  lapply(portfolio_cols, function(col) {
    port_xts <- xts::xts(df[[col]], order.by = df$date)
    bench_xts <- xts::xts(df$benchmark, order.by = df$date)
    tibble(
      Portfolio = col,
      Sharpe = as.numeric(PerformanceAnalytics::SharpeRatio.annualized(port_xts, geometric = FALSE)),
      Information_Ratio = as.numeric(PerformanceAnalytics::InformationRatio(port_xts, bench_xts))
    )
  }) %>% bind_rows()
}

results <- ret_df %>%
  group_by(vol_regime) %>%
  group_modify(~calc_metrics(.x)) %>%
  ungroup() %>%
  select(Volatility = vol_regime, Portfolio, Sharpe, Information_Ratio)

print(results)
