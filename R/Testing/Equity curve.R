library(ggplot2)
library(dplyr)
# 1) Load your backtest results
#load("keras_portfolios9v2.RData")  # brings in `keras_portfolios9`

# 2) Prepare model equity curve

model_eq <- keras_portfolios1_pp$portfolio_returns %>%
  arrange(date) %>%
  # assume your model column is named "keras_regularized_bm_ir2_1";
  # adjust the name below to whatever appears in names(portfolio_returns)
  mutate(model_equity = cumprod(1 + keras_regularized_bm_ir2_1))

# 3) Prepare benchmark equity curve
benchmark_new <- sp500_m_signals %>%
  # 1) Compute each stock’s contribution to the benchmark return
  mutate(contrib = ret * bm_new) %>%
  # 2) Sum up by date
  group_by(date) %>%
  summarise(
    benchmark_ret = sum(contrib, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # 3) Turn into an equity curve
  arrange(date) %>%
  mutate(
    benchmark_ret = replace_na(benchmark_ret, 0),
    EC_benchmark  = cumprod(1 + benchmark_ret)
  )

# 4) Combine for plotting
eq_df <- model_eq %>%
  select(date, model_equity) %>%
  left_join(benchmark_new %>% select(date, EC_benchmark), by = "date") %>%
  tidyr::pivot_longer(-date, names_to = "strategy", values_to = "wealth")

# 5) Plot with ggplot2
ggplot(eq_df, aes(x = date, y = wealth, color = strategy)) +
  geom_line(size = 1) +
  labs(
    title = "Equity Curves: Keras Model vs. Benchmark",
    x = "Date", y = "Wealth Index",
    color = ""
  ) +
  theme_minimal()


#Log chart

eq_df %>%  # df with columns date, strategy, wealth
  ggplot(aes(x = date, y = wealth, color = strategy)) +
  geom_line(size = 0.8) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Equity Curves: Keras Model vs. Benchmark (log scale)",
    x = "Date", y = "Wealth Index (log scale)",
    color = ""
  ) +
  theme_minimal()



# What’s the range of your OOS returns?
keras_portfolios1_pp$portfolio_returns %>%
  tidyr::pivot_longer(-date, names_to="model", values_to="ret") %>%
  group_by(model) %>%
  summarise(
    n_obs   = n(),
    n_na    = sum(is.na(ret)),
    ret_min = min(ret, na.rm=TRUE),
    ret_max = max(ret, na.rm=TRUE),
    ret_mean= mean(ret, na.rm=TRUE)
  )


