# Regression analysis of predicted weights on features
#
# This script loads the `keras_portfolios9_bigger_net` object from the
# `keras_portfolios9v3_new_bm.RData` file and regresses the model's
# predicted weights on the input predictors. The goal is to identify
# which predictors drive the tilts in the portfolio.
#
# Usage:
#   source("R/Testing/weights_regression.R")
#   # Make sure `sp500_m_signals` (or a similar data set containing the
#   # predictors) is loaded in your environment before sourcing.

library(dplyr)
library(broom)

# 1) Load portfolio object (creates `keras_portfolios9_bigger_net`)
#load("keras_portfolios9v3_new_bm.RData")

# 2) Specify the predictor columns used to train the model
features <- c(
  "div_yield_st", "ep", "mom12m", "idio_vol3f", "investment", "bm",
  "am", "fr", "gp", "ms", "noa", "ps", "rd", "ro_e", "sp", "vol_mkt"
)

# 3) Determine the weight column name produced by the backtest
weight_col <- setdiff(names(combined_portfolios$weights),
                      c("stock_id", "date"))[1]

# 4) Merge weights with predictors
reg_data <- combined_portfolios$weights %>%
  select(stock_id, date, weight = !!sym(weight_col)) %>%
  left_join(sp500_m_signals %>% select(stock_id, date, all_of(features)),
            by = c("stock_id", "date")) %>%
  drop_na()

# 5) Run linear regression of weights on predictors (full sample)
reg_formula <- as.formula(paste("weight ~", paste(features, collapse = " + ")))
reg_model <- lm(reg_formula, data = reg_data)

# 6) Display coefficients ordered by absolute value for full sample
coeff_table <- broom::tidy(reg_model) %>%
  arrange(desc(abs(estimate)))
print(coeff_table)

# 7) Estimate coefficients separately for each time window
coeff_by_window <- reg_data %>%
  group_by(date) %>%
  do(broom::tidy(lm(reg_formula, data = .))) %>%
  ungroup()

print(coeff_by_window)

# 8) Plot coefficient estimates over time
coeff_by_window %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = date, y = estimate)) +
  geom_line() +
  facet_wrap(~ term, scales = "free_y") +
  labs(
    title = "Regression Coefficients Over Time",
    x = "Date",
    y = "Coefficient"
  ) +
  theme_minimal() -> coeff_plot

print(coeff_plot)




#Per Model

# 3) pivot all weight-columns into long form
reg_data <- combined_portfolios$weights %>%
  pivot_longer(
    cols      = -c(stock_id, date),
    names_to  = "Portfolio",
    values_to = "weight"
  ) %>%
  left_join(
    sp500_m_signals %>%
      select(stock_id, date, all_of(features)),
    by = c("stock_id", "date")
  ) %>%
  drop_na()

# 4) regression formula (same for every portfolio)
reg_formula <- as.formula(paste("weight ~", paste(features, collapse = " + ")))

# 5) full‐sample coefficients **per portfolio**
coeff_full <- reg_data %>%
  group_by(Portfolio) %>%
  do(tidy(lm(reg_formula, data = .))) %>%
  ungroup() %>%
  arrange(Portfolio, desc(abs(estimate)))

print(coeff_full)

# 6) time‐series of coefficients **per portfolio & date**
coeff_by_window <- reg_data %>%
  group_by(Portfolio, date) %>%
  do(tidy(lm(reg_formula, data = .))) %>%
  ungroup()

print(coeff_by_window)

unique_models <- unique(coeff_by_window$Portfolio)

for (model_name in unique_models) {

  df_m <- coeff_by_window %>%
    filter(Portfolio == model_name, term != "(Intercept)")

  p_m <- ggplot(df_m, aes(x = date, y = estimate)) +
    geom_line() +
    facet_wrap(~ term, scales = "free_y", ncol = 4) +
    labs(
      title = paste("Regression Coefficients Over Time for", model_name),
      x     = "Date",
      y     = "Coefficient"
    ) +
    theme_minimal() +
    theme(
      strip.text      = element_text(size = 8),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      plot.title      = element_text(hjust = 0.5)
    )

  print(p_m)

  # If you'd like to save each plot to disk, uncomment:
  # ggsave(
  #   filename = sprintf("coefficients_over_time_%s.png", model_name),
  #   plot     = p_m,
  #   width    = 12,
  #   height   = 8
  # )
}
