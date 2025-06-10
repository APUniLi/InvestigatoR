library(dplyr)
conflict_prefer("lag", "dplyr")

sp500_m_signals <- sp500_m_signals %>%
  arrange(stock_id, date) %>%
  group_by(stock_id) %>%
  mutate(
    mktcap_lag = dplyr::lag(mktcap)       # use dplyr::lag explicitly
  ) %>%
  ungroup() %>%
  group_by(date) %>%
  # 1) drop rows where we have no lagged cap (so first‐obs are out)
  filter(!is.na(mktcap_lag)) %>%
  # 2) rank uniquely by lagged cap, breaking ties deterministically
  mutate(rank_desc = row_number(desc(mktcap_lag))) %>%
  # 3) top‐500 get value‐weights, everyone else 0
  mutate(
    bm_new = if_else(
      rank_desc <= 500,
      mktcap_lag / sum(mktcap_lag[rank_desc <= 500], na.rm = TRUE),
      0
    )
  ) %>%
  ungroup() %>%
  select(-rank_desc)



sp500_m_signals %>%
  group_by(date) %>%
  summarise(
    n_selected = sum(bm_new > 0),
    tot_weight = sum(bm_new, na.rm = TRUE)
  ) %>%
  filter(n_selected != 500 | abs(tot_weight - 1) > 1e-6)




sp500_m_signals <- sp500_m_signals %>%
  mutate(
    # 1 if this stock is in the top-500 (bm_new>0), else 0
    sp500_flag_new = if_else(bm_new > 0, 1L, 0L)
  )

sp500_m_signals %>%
  summarise(
    n_flag_old   = sum(sp500_flag, na.rm=TRUE),
    n_flag_new   = sum(sp500_flag_new, na.rm=TRUE),
    n_bm_positive = sum(bm_new > 0, na.rm=TRUE)
  )
# n_flag_new and n_bm_positive should be identical
