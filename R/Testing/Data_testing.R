library(dplyr)

library(dplyr)

distinct_members_by_date <- sp500_m_signals1 %>%
  filter(sp500_flag == 1) %>%
  group_by(date) %>%
  summarise(
    n_distinct_members = n_distinct(stock_id),
    n_rows             = n()
  ) %>%
  ungroup()

# View the first 10 dates
distinct_members_by_date %>% dplyr::slice(1:10)

# Get summary stats
distinct_members_by_date %>%
  summarise(
    min_members    = min(n_distinct_members),
    median_members = median(n_distinct_members),
    max_members    = max(n_distinct_members)
  )

# See which dates are far from ~500
distinct_members_by_date %>%
  filter(n_distinct_members < 450 | n_distinct_members > 550) %>%
  arrange(date) %>%
  dplyr::slice(1:20)


# 2) Look at the first few rows
distinct_members_by_date %>% dplyr::slice(1:10)

# 3) Get summary statistics (min, median, max, etc.)
members_by_date %>%
  summarise(
    min_members   = min(n_members),
    median_members= median(n_members),
    max_members   = max(n_members)
  )

# 4) See if there are any dates where n_members deviates from ≈500
distinct_members_by_date %>%
  filter(n_members < 450 | n_members > 550) %>%
  arrange(date)





library(dplyr)

universe_vs_flagged <- sp500_m_signals1 %>%
  group_by(date) %>%
  summarise(
    total_tickers    = n_distinct(stock_id),
    flagged_tickers  = sum(sp500_flag == 1, na.rm = TRUE) %>% as.integer(),
    distinct_flagged = n_distinct(stock_id[sp500_flag == 1])
  ) %>%
  ungroup()

# View first 10
universe_vs_flagged %>% dplyr::slice(1:10)

# Get summary
universe_vs_flagged %>%
  summarise(
    min_total   = min(total_tickers),
    max_total   = max(total_tickers),
    min_flagged = min(distinct_flagged),
    max_flagged = max(distinct_flagged),
    median_flagged = median(distinct_flagged)
  )


library(dplyr)

# 1) Count how many rows have sp500_flag == 1 vs. != 1
sp500_m_signals1 %>%
  summarise(
    total_rows    = n(),
    n_flag1       = sum(sp500_flag == 1),
    n_not_flag1   = sum(sp500_flag != 1)
  )

library(dplyr)
library(ggplot2)

# 1) How many NAs in mktcap?
sp500_m_signals %>%
  summarise(
    total_rows = n(),
    n_mktcap_na = sum(is.na(mktcap)),
    pct_na     = 100 * n_mktcap_na / total_rows
  )

# 2) Are there any zero or negative mktcaps?
sp500_m_signals %>%
  summarise(
    n_zero    = sum(mktcap == 0, na.rm = TRUE),
    n_neg     = sum(mktcap < 0,  na.rm = TRUE),
    pct_zero  = 100 * n_zero / n(),
    pct_neg   = 100 * n_neg  / n()
  )

# 3) Quick summary (min, 1st/median/3rd quartile, max)
sp500_m_signals %>%
  summarise(
    min_mktcap  = min(mktcap, na.rm = TRUE),
    q1_mktcap   = quantile(mktcap, 0.25, na.rm = TRUE),
    median_mktcap = median(mktcap, na.rm = TRUE),
    q3_mktcap   = quantile(mktcap, 0.75, na.rm = TRUE),
    max_mktcap  = max(mktcap, na.rm = TRUE),
    mean_mktcap = mean(mktcap, na.rm = TRUE)
  )
# 4) Plot a histogram (on log scale) to spot outliers
ggplot(sp500_m_signals, aes(x = mktcap)) +
  geom_histogram(bins = 60) +
  scale_x_log10() +
  labs(
    title = "Distribution of mktcap (log₁₀ scale)",
    x = "Market Cap (log₁₀)",
    y = "Count"
  ) +
  theme_minimal()

# 5) Check for duplicate (date, stock_id) with conflicting mktcap
duplicates_mktcap <- sp500_m_signals %>%
  group_by(date, stock_id) %>%
  filter(n() > 1) %>%
  arrange(date, stock_id) %>%
  select(date, stock_id, mktcap)

# How many duplicates?
nrow(duplicates_mktcap)

# View a few
duplicates_mktcap %>% head(10)



library(dplyr)

sp500_m_signals %>%
  summarise(
    total_rows  = n(),
    n_ret_na    = sum(is.na(ret)),
    pct_ret_na  = 100 * n_ret_na / total_rows
  )

sp500_m_signals %>%
  summarise(
    n_ret_le_neg100 = sum(ret <= -1, na.rm=TRUE),
    n_ret_gt_500pct = sum(ret > 5, na.rm=TRUE)  # 5 ≡ +500%
  )


sp500_m_signals %>%
  summarise(
    min_ret   = min(ret, na.rm = TRUE),
    q1_ret    = quantile(ret, 0.25, na.rm = TRUE),
    median_ret= median(ret, na.rm = TRUE),
    q3_ret    = quantile(ret, 0.75, na.rm = TRUE),
    max_ret   = max(ret, na.rm = TRUE),
    mean_ret  = mean(ret, na.rm = TRUE),
    sd_ret    = sd(ret, na.rm = TRUE)
  )
library(ggplot2)

ggplot(sp500_m_signals, aes(x = ret)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  labs(
    title = "Histogram of Raw Returns (ret) on log₁₀ Scale",
    x = "ret (log₁₀)",
    y = "Count"
  ) +
  theme_minimal()

# Positive returns
ggplot(filter(sp500_m_signals, ret > 0), aes(x = ret)) +
  geom_histogram(bins = 60) +
  scale_x_log10() +
  labs(
    title = "Positive Returns (log₁₀ scale)",
    x = "ret (> 0)",
    y = "Count"
  ) +
  theme_minimal()

# Negative returns
ggplot(filter(sp500_m_signals, ret < 0), aes(x = abs(ret))) +
  geom_histogram(bins = 60) +
  scale_x_log10() +
  labs(
    title = "Absolute Value of Negative Returns (log₁₀ scale)",
    x = "|ret| (< 0)",
    y = "Count"
  ) +
  theme_minimal()
library(dplyr)

# 5.1  Largest positive returns
sp500_m_signals %>%
  arrange(desc(ret)) %>%
  select(stock_id, date, ret, mktcap) %>%
  dplyr::slice(1:10)

# 5.2  Largest negative returns
sp500_m_signals %>%
  arrange(ret) %>%
  select(stock_id, date, ret, mktcap) %>%
  dplyr::slice(1:10)



# -----------------------------------------------------------------------------
# R Script: Data Description Checks for S&P 500 Neural Network Dataset
# -----------------------------------------------------------------------------
# Purpose: Programmatically verify key aspects of the dataset:
#   1. Universe & Coverage
#   2. Variables & Definitions
#   3. Summary Statistics
#   4. Index Membership Dynamics
#   5. Market-Cap & Weight Dispersion
#   6. Correlation Structure
#   7. Missing Data & Imputation
#   8. Normalization & Scaling
#   9. Sample Splits
#  10. Survivorship / Start‐Date Bias (limitations)
# -----------------------------------------------------------------------------

# 0. Setup --------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
install.packages("skimr")  # Install skimr for summary statistics
library(skimr)

# Load preprocessed data (assumes you have saved as .RData)
load("~/Thesis/InvestigatoR/data-raw/sp500_m_signals1.RData")
# sp500_m_signals1 should be a data.frame or tibble with:
#   stock_id, date, ret, mktcap, bm_weight, sp500_flag, plus 16 features

# 1. Universe & Coverage -----------------------------------------------------
cat("1. Universe & Coverage\n")
universe <- sp500_m_signals1 %>% select(stock_id) %>% distinct()
cat("  • # unique stocks (PERMNO):", nrow(universe), "\n")
date_range <- range(sp500_m_signals1$date)
cat("  • Date range:", format(date_range[1]), "to", format(date_range[2]), "\n\n")

# 2. Variables & Definitions -------------------------------------------------
cat("2. Variables & Definitions\n")
all_vars <- names(sp500_m_signals1)
print(all_vars)
cat("\n")

# 3. Summary Statistics ------------------------------------------------------
cat("3. Summary Statistics (returns + features)\n")
# Returns summary
cat("  • Monthly returns:\n")
print(skim(sp500_m_signals1$ret))
# Features summary
features <- setdiff(all_vars, c("stock_id","date","ret","mktcap","bm_weight","sp500_flag"))
cat("  • Feature variables:\n")
print(skim(sp500_m_signals1 %>% select(all_of(features))))
cat("\n")

# 4. Index Membership Dynamics -----------------------------------------------
cat("4. Index Membership Dynamics\n")
membership_ts <- sp500_m_signals1 %>%
  group_by(date) %>%
  summarize(n_in = sum(sp500_flag))
print(head(membership_ts))
# Plot membership over time
ggplot(membership_ts, aes(x = date, y = n_in)) +
  geom_line() +
  labs(title = "S&P 500 Membership Over Time",
       x = "Month", y = "Number of Constituents")

# 5. Market-Cap & Weight Dispersion ------------------------------------------
cat("5. Market-Cap & Weight Dispersion\n")
# Distribution of bm_weight each month
weight_stats <- sp500_m_signals1 %>%
  group_by(date) %>%
  summarize(
    median_w = median(bm_weight),
    p90_w    = quantile(bm_weight, 0.9),
    max_w    = max(bm_weight)
  )
print(head(weight_stats))
# Plot top‐decile weight over time
ggplot(weight_stats, aes(x = date, y = p90_w)) +
  geom_line() +
  labs(title = "90th Percentile Benchmark Weight Over Time",
       x = "Month", y = "P90(bm_weight)")

# 6. Correlation Structure ---------------------------------------------------
cat("6. Correlation Structure of Signals\n")
feature_mat <- sp500_m_signals1 %>% select(all_of(features))
cor_mat <- cor(feature_mat, use = "pairwise.complete.obs")
print(round(cor_mat, 2))
# Visualize correlation heatmap
ggplot(melt(cor_mat), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(title = "Feature Correlation Matrix")

# 7. Missing Data & Imputation ------------------------------------------------
cat("7. Missing Data & Imputation\n")
missing_counts <- sp500_m_signals1 %>%
  summarize(across(everything(), ~ sum(is.na(.))))
print(missing_counts)
cat("  • All features and returns should now have zero missing values.\n\n")

# 8. Normalization & Scaling -------------------------------------------------
cat("8. Normalization & Scaling Checks\n")
norm_ranges <- sp500_m_signals1 %>%
  summarize(across(all_of(features),
                   list(min = ~ min(.), max = ~ max(.))))
print(norm_ranges)

# 9. Sample Splits -----------------------------------------------------------
cat("9. Sample Splits\n")
# Define your train/val/test cutoffs
cut_train_end <- as.Date("2009-12-31")
cut_val_end   <- as.Date("2014-12-31")
counts_splits <- sp500_m_signals1 %>%
  mutate(split = case_when(
    date <= cut_train_end ~ "train",
    date <= cut_val_end   ~ "validation",
    TRUE                  ~ "test"
  )) %>%
  group_by(split) %>%
  summarize(n_obs = n())
print(counts_splits)
cat("\n")

# 10. Survivorship & Start‐Date Bias -----------------------------------------
cat("10. Survivorship / Start‐Date Bias Check\n")
first_dates <- sp500_m_signals1 %>%
  group_by(stock_id) %>%
  summarize(start = min(date))
hist(first_dates$start, breaks = "years",
     main = "Histogram of Stock Listing (First Observations)",
     xlab = "First Month in Dataset", ylab = "Count of Stocks")

# End of Script
# -----------------------------------------------------------------------------

