window_stocks_fixed <- lapply(seq_len(nrow(indices)), function(i) {
  idx <- indices[i, ]

  # only those in the S&P 500 *on* the very last training day:
  tr <- dp %>%
    filter(
      date == idx$training_end,
      mask == 1
    ) %>%
    pull(stock_id) %>%
    unique()

  # only those in the S&P 500 *on* the very first test day:
  te <- dp %>%
    filter(
      date == idx$prediction_start,
      mask == 1
    ) %>%
    pull(stock_id) %>%
    unique()

  tibble(
    window            = i,
    training_start    = idx$training_start,
    training_end      = idx$training_end,
    prediction_start  = idx$prediction_start,
    prediction_end    = idx$prediction_end,
    train_stocks      = list(tr),
    test_stocks       = list(te)
  )
})
window_stocks <- bind_rows(window_stocks_fixed)
qc <- window_stocks %>%
  pivot_longer(c(train_stocks, test_stocks),
               names_to="phase", values_to="stock_id") %>%
  unnest(stock_id) %>%
  mutate(
    check_date = if_else(phase=="train_stocks",
                         training_end,
                         prediction_start)
  ) %>%
  left_join(dp %>% select(stock_id, date, mask),
            by = c("stock_id","check_date"="date"))

violations2 <- qc %>%
  filter(is.na(mask) | mask==0) %>%
  distinct(window, phase, stock_id, check_date)

# Option A: base-R
bad <- violations2[1, ]
print(bad)

# Option B: using head()
bad <- head(violations2, 1)
print(bad)

nrow(violations2)





library(dplyr)

keras_portfolios9$benchmark_returns %>%
  summarise(
    n_periods = n(),
    n_na     = sum(is.na(benchmark_return)),
    min_ret  = min(benchmark_return, na.rm=TRUE),
    max_ret  = max(benchmark_return, na.rm=TRUE),
    mean_ret = mean(benchmark_return, na.rm=TRUE)
  )



keras_portfolios9$benchmark_returns %>%
  filter(benchmark_return == max(benchmark_return, na.rm=TRUE))

# assuming you attached them to your object as benchmark_weights
keras_portfolios9$benchmark_weights %>%
  group_by(date) %>%
  summarise(total_w = sum(benchmark_weight, na.rm=TRUE)) %>%
  filter(abs(total_w - 1) > 1e-6)



# After loading sp500_m_signals:
str(sp500_m_signals)
# or, for a tidy‐verse style glimpse:
library(dplyr)
sp500_m_signals %>% glimpse()


sp500_m_signals %>%
  summarise(
    total_rows    = n(),
    nonzero_weight = sum(bm_weight != 0, na.rm = TRUE),
    zero_weight    = sum(bm_weight == 0, na.rm = TRUE)
  )


# 2.2 For each date, what is the total benchmark‐weight across all stocks?

library(dplyr)

weights_by_date <- sp500_m_signals %>%
  group_by(date) %>%
  summarise(
    total_bm_w = sum(bm_weight, na.rm = TRUE),
    n_stocks   = n(),
    n_nonzero  = sum(bm_weight != 0, na.rm = TRUE)
  ) %>%
  ungroup()

# Now slice() will work:
weights_by_date %>% dplyr::slice(1:10)

library(dplyr)

# 1.1 Group by date and compute:
#     • total_bm_w   = sum of bm_weight across all stocks in that month
#     • n_stocks     = number of rows (i.e. number of stocks present)
#     • n_nonzero    = how many of those had bm_weight != 0
weights_by_date <- sp500_m_signals %>%
  group_by(date) %>%
  summarise(
    total_bm_w = sum(bm_weight, na.rm = TRUE),
    n_stocks   = n(),
    n_nonzero  = sum(bm_weight != 0, na.rm = TRUE)
  ) %>%
  ungroup()

# 1.2 Look at the very first 10 dates to see if anything is zero
weights_by_date %>% dplyr::slice(1:10)

library(dplyr)

# 2.1 Compute per‐date “raw benchmark return” by summing ret * bm_weight:
raw_benchmark_by_date <- sp500_m_signals %>%
  mutate(contrib = ret * bm_weight) %>%
  group_by(date) %>%
  summarise(
    n_stocks          = n(),
    n_nonzero_weight  = sum(bm_weight != 0, na.rm = TRUE),
    total_weight      = sum(bm_weight, na.rm = TRUE),
    benchmark_return  = sum(contrib, na.rm = TRUE)
  ) %>%
  ungroup()

# 2.2 See which is the very first date with total_weight > 0:
raw_benchmark_by_date %>%
  filter(total_weight > 0) %>%
  arrange(date) %>%
  dplyr::slice(1)

raw_benchmark_by_date %>% filter(total_weight == 0) %>% dplyr::slice(1:5)

sp500_m_signals %>%
  summarise(
    n_obs    = n(),
    n_na     = sum(is.na(ret)),
    min_ret  = min(ret, na.rm = TRUE),
    max_ret  = max(ret, na.rm = TRUE),
    mean_ret = mean(ret, na.rm = TRUE)
  )


sp500_m_signals %>%
  arrange(desc(ret)) %>%
  dplyr::select(stock_id, date, ret, bm_weight) %>%
  dplyr::slice(1:10)


sp500_m_signals %>%
  filter(ret > 5) %>%     # e.g. all returns > 500%
  select(stock_id, date, ret, sp500_flag, bm_weight) %>%
  arrange(desc(ret))


library(dplyr)

# 3.1 List the first date where total_weight == 0
first_zero_w_date <- raw_benchmark_by_date %>%
  filter(total_weight == 0) %>%
  arrange(date) %>%
  dplyr::slice(1) %>%
  pull(date)

first_zero_w_date
# [1] "1960-09-01"

# 3.2 Show all rows in sp500_m_signals for that month
sp500_m_signals %>%
  filter(date == first_zero_w_date) %>%
  select(stock_id, ret, bm_weight) %>%
  distinct() %>%
  arrange(stock_id) %>%
  head(10)

sp500_m_signals %>%
  group_by(date) %>%
  summarise(
    total_bm_w = sum(bm_weight, na.rm = TRUE),
    n_nonzero  = sum(bm_weight != 0, na.rm = TRUE),
    n_stocks   = n()
  ) %>%
  filter(total_bm_w == 0) %>%
  nrow()
# [1]  something like 100+  (e.g. 1960-09, 1962-03, …)

weights_by_date %>%
  filter(total_bm_w == 0) %>%
  select(date, n_stocks) %>%
  arrange(date) %>%
  dplyr::slice(1:10)



