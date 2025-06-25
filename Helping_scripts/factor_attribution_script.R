#===============================================================================
# 0.  Libraries
#===============================================================================
library(dplyr)
library(tidyr)
library(slider)
library(lubridate)

#===============================================================================
# 1.  Inputs
#===============================================================================
# 1.1  Your model’s monthly benchmark-tilt weights:
#      combined_portfolios_pp$delta_weights must have columns
#      stock_id, date, <one column per portfolio>
weights_df <- combined_portfolios_pp$delta_weights

# 1.2  Raw monthly stock returns & feature signals:
#      sp500_m_signals must have stock_id, date, ret, and one column per feature
signals <- sp500_m_signals

# 1.3  List of feature names
features <- c(
  "div_yield_st", "ep", "mom12m", "idio_vol3f", "investment", "bm",
  "am", "fr", "gp", "ms", "noa", "ps", "rd", "ro_e", "sp", "vol_mkt"
)

#===============================================================================
# 2.  Compute *realized* factor premia via equal-weighted long–short decile
#     For each feature f and date t:
#       r_t^f = avg return of top decile (by f_t) − avg return of bottom decile
#===============================================================================
factor_real <- signals %>%
  group_by(date) %>%
  summarise(across(all_of(features), ~{
    x <- .x
    r <- ret
    q_top <- quantile(x, 0.9, na.rm = TRUE)
    q_bot <- quantile(x, 0.1, na.rm = TRUE)
    mean(r[x >= q_top], na.rm = TRUE) -
      mean(r[x <= q_bot], na.rm = TRUE)
  }, .names = "{.col}"), .groups = "drop")

# reshape to long form
factor_real_long <- factor_real %>%
  pivot_longer(-date, names_to = "Factor", values_to = "Return")

#===============================================================================
# 3.  Estimate *time-varying* betas β_{i,f}(t) via 36-month rolling covariance/variance
#     β_{i,f}(t) = cov( ret_{i, t−35:t},  r^f_{t−35:t} ) / var( r^f_{t−35:t} )
#===============================================================================
# first join each stock’s ret series to each factor’s realized premia
joined <- signals %>%
  select(stock_id, date, ret) %>%
  left_join(factor_real, by = "date")

# then compute rolling betas
betas_tv <- joined %>%
  group_by(stock_id) %>%
  arrange(date) %>%
  mutate(across(all_of(features),
                ~ slide2_dbl(
                  .x = ret,
                  .y = get(cur_column()),
                  .f = ~ cov(.x, .y, use = "pairwise.complete.obs") /
                    var(.y, na.rm = TRUE),
                  .before = 35,
                  .complete = TRUE
                ),
                .names = "{.col}"
  )) %>%
  ungroup() %>%
  select(stock_id, date, all_of(features))

# pivot to long form
beta_long <- betas_tv %>%
  pivot_longer(-c(stock_id, date), names_to = "Factor", values_to = "Beta")

#===============================================================================
# 4.  Compute exposures:  Exposure_{t,f} = Σ_i Δw_{t,i} · β_{i,f}(t)
#===============================================================================
weights_long <- weights_df %>%
  pivot_longer(-c(stock_id, date),
               names_to = "Portfolio",
               values_to = "Tilt")

exposures <- weights_long %>%
  left_join(beta_long, by = c("stock_id", "date")) %>%
  group_by(Portfolio, date, Factor) %>%
  summarise(Exposure = sum(Tilt * Beta, na.rm = TRUE), .groups = "drop")

#===============================================================================
# 5.  Compute contributions:
#     Contribution_{t,f} = Exposure_{t,f} × r_{t,f}
#===============================================================================
contributions <- exposures %>%
  left_join(factor_real_long, by = c("date", "Factor")) %>%
  mutate(Contribution = Exposure * Return)

#===============================================================================
# 6.  Compute portfolio excess returns (for residuals):
#     p_excess_{t} = Σ_i Δw_{t,i} · ret_{i, t+1}
#===============================================================================
# first build ret_{next} = return over (t → t+1)
returns_shift <- signals %>%
  group_by(stock_id) %>%
  arrange(date) %>%
  mutate(ret_next = lead(ret)) %>%
  ungroup() %>%
  select(stock_id, date, ret_next)

p_excess <- weights_long %>%
  left_join(returns_shift, by = c("stock_id", "date")) %>%
  group_by(Portfolio, date) %>%
  summarise(Port_Excess_Ret = sum(Tilt * ret_next, na.rm = TRUE),
            .groups = "drop")

#===============================================================================
# 7.  Compute residuals:  Residual_{t} = Port_Excess_Ret_{t} − Σ_f Contribution_{t,f}
#===============================================================================
residuals <- p_excess %>%
  left_join(
    contributions %>%
      group_by(Portfolio, date) %>%
      summarise(Factor_Return = sum(Contribution, na.rm = TRUE),
                .groups = "drop"),
    by = c("Portfolio", "date")
  ) %>%
  mutate(Residual = Port_Excess_Ret - Factor_Return)

#===============================================================================
# 8.  Summarize & rank predictors by average contribution
#===============================================================================
avg_contrib <- contributions %>%
  group_by(Factor) %>%
  summarise(Avg_Contribution = mean(Contribution, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(Avg_Contribution))

avg_contrib_by_model <- contributions %>%
  group_by(Portfolio, Factor) %>%
  summarise(
    Avg_Contribution = mean(Contribution, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Portfolio, desc(Avg_Contribution))
#===============================================================================
# 9.  Outputs
#    - `exposures`      : tibble (Portfolio, date, Factor, Exposure)
#    - `contributions`  : tibble (Portfolio, date, Factor, Exposure, Return, Contribution)
#    - `residuals`      : tibble (Portfolio, date, Port_Excess_Ret, Factor_Return, Residual)
#    - `avg_contrib`    : tibble (Factor, Avg_Contribution) ranked descending
#===============================================================================

# Example: print the top 5 exploited predictors
print(head(avg_contrib, 16))
print(avg_contrib_by_model, n  = 64)


# 1) One plot *per model*, facets = factors
unique_models <- unique(contributions$Portfolio)

for(model in unique_models) {
  df_m <- contributions %>% filter(Portfolio == model)
  p_m <- ggplot(df_m, aes(x = date, y = Contribution)) +
    geom_line() +
    facet_wrap(~ Factor, scales = "free_y", ncol = 4) +
    labs(
      title = paste("Factor Contributions for", model),
      x     = "Date",
      y     = "Contribution"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(p_m)
  # optionally: ggsave(sprintf("contrib_%s.png", model), p_m, width=12, height=8)
}

# 2) One plot *per factor*, facets = models
unique_factors <- unique(contributions$Factor)

for(f in unique_factors) {
  df_f <- contributions %>% filter(Factor == f)
  p_f <- ggplot(df_f, aes(x = date, y = Contribution)) +
    geom_line() +
    facet_wrap(~ Portfolio, scales = "free_y", ncol = 3) +
    labs(
      title = paste("Portfolio Contributions of Factor:", f),
      x     = "Date",
      y     = "Contribution"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(p_f)
  # optionally: ggsave(sprintf("contrib_factor_%s.png", f), p_f, width=12, height=6)
}
