library(dplyr)

vars <- c("div_yield_st","ep","mom12m","idio_vol3f",
          "investment","bm","am","fr","gp","ms")

dup_pct <- sp500_m_signals %>%
  summarise(
    across(all_of(vars),
           ~ (sum( duplicated(.) | duplicated(., fromLast = TRUE) ) / n()) * 100,
           .names = "{.col}_pct_imp"
    )
  )

print(dup_pct)
library(dplyr)

vars <- c("div_yield_st","ep","mom12m","idio_vol3f",
          "investment","bm","am","fr","gp","ms")

# 1) compute % duplicates _per month_
monthly_dup_pct <- sp500_m_signals %>%
  group_by(yyyymm) %>%
  summarise(
    across(all_of(vars),
           ~ mean( duplicated(.) | duplicated(., fromLast = TRUE) ) * 100,
           .names = "{.col}_pct_imp"
    ),
    .groups = "drop"
  )

# this gives you one row per month, ten columns of percentages
print(monthly_dup_pct)

# 2) if you really want a single “overall” percentage for each variable,
#    just average those monthly percentages:
overall_dup_pct <- monthly_dup_pct %>%
  summarise(across(everything(), mean))

print(overall_dup_pct)
