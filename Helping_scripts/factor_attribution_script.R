# Example script: factor tilt attribution for combined_portfolios_pp
# Requires `sp500_m_signals` and the portfolio object to be loaded in the environment

library(dplyr)
library(tidyr)

# --------------------------------------------------------
# 1. Extract benchmark-relative weights from the portfolioReturns object
# --------------------------------------------------------
weights_df <- combined_portfolios_pp$delta_weights

# --------------------------------------------------------
# 2. Define the factor feature set used in the model
# --------------------------------------------------------
features <- c(
  "div_yield_st", "ep", "mom12m", "idio_vol3f", "investment", "bm",
  "am", "fr", "gp", "ms", "noa", "ps", "rd", "ro_e", "sp", "vol_mkt"
)

# --------------------------------------------------------
# 3. Estimate factor loadings (betas) for each stock
#    Simple covariance-based slope estimate
# --------------------------------------------------------
betas <- sp500_m_signals %>%
  filter(date >= min(weights_df$date), date <= max(weights_df$date)) %>%
  group_by(stock_id) %>%
  summarise(
    across(
      all_of(features),
      ~ cov(ret, .x, use = "pairwise.complete.obs") / var(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# --------------------------------------------------------
# 4. Realized factor returns
# --------------------------------------------------------
factor_real <- sp500_m_signals %>%
  group_by(date) %>%
  summarise(
    across(
      all_of(features),
      ~ cov(ret, .x, use = "pairwise.complete.obs") / var(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# --------------------------------------------------------
# 5. Run tilt attribution
# --------------------------------------------------------
attr_res <- factor_tilt_attribution(weights_df, betas, factor_real)

# attr_res$exposures
# attr_res$contributions
