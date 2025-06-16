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
load("keras_portfolios9v3_new_bm.RData")

# 2) Specify the predictor columns used to train the model
features <- c(
  "div_yield_st", "ep", "mom12m", "idio_vol3f", "investment", "bm",
  "am", "fr", "gp", "ms", "noa", "ps", "rd", "ro_e", "sp", "vol_mkt"
)

# 3) Determine the weight column name produced by the backtest
weight_col <- setdiff(names(keras_portfolios9_bigger_net$weights),
                      c("stock_id", "date"))[1]

# 4) Merge weights with predictors
reg_data <- keras_portfolios9_bigger_net$weights %>%
  select(stock_id, date, weight = !!sym(weight_col)) %>%
  left_join(sp500_m_signals %>% select(stock_id, date, all_of(features)),
            by = c("stock_id", "date")) %>%
  drop_na()

# 5) Run linear regression of weights on predictors
reg_formula <- as.formula(paste("weight ~", paste(features, collapse = " + ")))
reg_model <- lm(reg_formula, data = reg_data)

# 6) Display coefficients ordered by absolute value
coeff_table <- broom::tidy(reg_model) %>%
  arrange(desc(abs(estimate)))
print(coeff_table)
