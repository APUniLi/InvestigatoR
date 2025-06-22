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
library(lubridate)

# 1) Load portfolio object (expects variable `keras_portfolios9_bigger_net`)
load("keras_portfolios9v3_new_bm.RData")

# 2) Determine the portfolio column name
model_col <- setdiff(names(keras_portfolios1_pp$portfolio_returns), "date")[1]

# 3) Prepare period returns and outperformance
perf_df <- keras_portfolios1_pp$portfolio_returns %>%
  select(date, portfolio_return = !!model_col) %>%
  left_join(keras_portfolios9_bigger_net$benchmark_returns %>%
              rename(benchmark_return = benchmark_return), by = "date") %>%
  arrange(date) %>%
  mutate(outperformance = portfolio_return - benchmark_return)

# 4) Plot period outperformance with positive (green) vs negative (red) bars
p_period <- ggplot(perf_df, aes(x = date, y = outperformance)) +
  geom_col(aes(fill = outperformance > 0), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Outperformance vs Benchmark",
    x = "Date",
    y = "Return Difference"
  ) +
  scale_fill_manual(values = c(`TRUE` = "seagreen3", `FALSE` = "firebrick3")) +
  theme_minimal()


# build a Date column for plotting
annual_df <- perf_df %>%
  mutate(
    year = year(date),
    year_date = as.Date(paste0(year, "-01-01"))
  ) %>%
  group_by(year, year_date) %>%
  summarise(
    port_ret = prod(1 + portfolio_return) - 1,
    bench_ret = prod(1 + benchmark_return) - 1,
    outperformance = port_ret - bench_ret,
    .groups = "drop"
  )

p_year <- ggplot(annual_df, aes(x = year_date, y = outperformance)) +
  geom_col(aes(fill = outperformance > 0), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(
    date_breaks = "5 years",            # put a tick every 5 years
    date_labels = "%Y",                 # label them as 4-digit years
    expand = expansion(add = c(0.5, 0.5)) # give a little padding at the ends
  ) +
  scale_fill_manual(values = c(`TRUE` = "seagreen3", `FALSE` = "firebrick3")) +
  labs(
    title = "Yearly Outperformance vs Benchmark",
    x = "Year",
    y = "Return Difference"
  ) +
  theme_minimal()

print(p_year)
print(p_period)

# 7) Display table of annual returns
print(annual_df)

# 8) Compute 12-month rolling tracking error of outperformance
tracking_df <- perf_df %>%
  arrange(date) %>%
  mutate(tracking_error = zoo::rollapply(outperformance, 12, sd, fill = NA, align = "right"))

# 9) Plot tracking error over time
p_te <- ggplot(tracking_df, aes(x = date, y = tracking_error)) +
  geom_line(color = "steelblue") +
  labs(
    title = "12-Month Rolling Tracking Error",
    x = "Date",
    y = "Tracking Error"
  ) +
  theme_minimal()

print(p_te)
