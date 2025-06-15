# Plot outperformance of keras_portfolios9_bigger_net against its benchmark
#
# This script loads the portfolio object saved in
# "keras_portfolios9v3_new_bm.RData" and plots the period-by-period
# outperformance of the predicted portfolio relative to the benchmark.
#
# Usage:
#   source("R/Testing/outperformance_plot.R")
#
library(dplyr)
library(ggplot2)
library(tidyr)

# 1) Load portfolio object (expects variable `keras_portfolios9_bigger_net`)
load("keras_portfolios9v3_new_bm.RData")

# 2) Determine the portfolio column name
model_col <- setdiff(names(keras_portfolios9_bigger_net$portfolio_returns), "date")[1]

# 3) Prepare period returns and outperformance
perf_df <- keras_portfolios9_bigger_net$portfolio_returns %>%
  select(date, portfolio_return = !!model_col) %>%
  left_join(keras_portfolios9_bigger_net$benchmark_returns %>%
              rename(benchmark_return = benchmark_return), by = "date") %>%
  arrange(date) %>%
  mutate(outperformance = portfolio_return - benchmark_return)

# 4) Plot period outperformance with positive (green) vs negative (red) bars
ggplot(perf_df, aes(x = date, y = outperformance)) +
  geom_col(aes(fill = outperformance > 0), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Outperformance vs Benchmark",
    x = "Date",
    y = "Return Difference"
  ) +
  scale_fill_manual(values = c(`TRUE` = "seagreen3", `FALSE` = "firebrick3")) +
  theme_minimal()
