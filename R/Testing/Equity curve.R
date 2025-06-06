# 1) Load your backtest results
#load("keras_portfolios9v2.RData")  # brings in `keras_portfolios9`

# 2) Prepare model equity curve
library(dplyr)
model_eq <- keras_portfolios9$portfolio_returns %>%
  arrange(date) %>%
  # assume your model column is named "keras_regularized_bm_ir2_1";
  # adjust the name below to whatever appears in names(portfolio_returns)
  mutate(model_equity = cumprod(1 + keras_regularized_bm_ir2_1/100))

# 3) Prepare benchmark equity curve
bench_eq <- keras_portfolios9$benchmark_returns %>%
  arrange(date) %>%
  mutate(benchmark_equity = cumprod(1 + benchmark_return))

# 4) Combine for plotting
eq_df <- model_eq %>%
  select(date, model_equity) %>%
  left_join(bench_eq %>% select(date, benchmark_equity), by = "date") %>%
  tidyr::pivot_longer(-date, names_to = "strategy", values_to = "wealth")

# 5) Plot with ggplot2
library(ggplot2)
ggplot(eq_df, aes(x = date, y = wealth, color = strategy)) +
  geom_line(size = 1) +
  labs(
    title = "Equity Curves: Keras Model vs. Benchmark",
    x = "Date", y = "Wealth Index",
    color = ""
  ) +
  theme_minimal()


# What’s the range of your OOS returns?
library(dplyr)
keras_portfolios9_bigger_net$portfolio_returns %>%
  tidyr::pivot_longer(-date, names_to="model", values_to="ret") %>%
  group_by(model) %>%
  summarise(
    n_obs   = n(),
    n_na    = sum(is.na(ret)),
    ret_min = min(ret, na.rm=TRUE),
    ret_max = max(ret, na.rm=TRUE),
    ret_mean= mean(ret, na.rm=TRUE)
  )


# Identify your model’s weight column(s)
wcols <- setdiff(names(keras_portfolios9$weights), c("stock_id","date"))
keras_portfolios9$weights %>%
  group_by(date) %>%
  summarise(across(all_of(wcols), ~ sum(.x, na.rm=TRUE)))


library(ggplot2)
ret_df <- keras_portfolios9_bigger_net$portfolio_returns %>%
  arrange(date) %>%
  select(date, model_ret = your_model_column)  # substitute your col name

ggplot(ret_df, aes(date, model_ret)) +
  geom_line() +
  labs(title="Raw Period Returns", y="Return", x="Date")
