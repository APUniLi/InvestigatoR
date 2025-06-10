#setwd("/home/sstoeckl/Packages/InvestigatoR")
library(reticulate)
library(tensorflow)
# Thread and core limitations

tf$config$threading$set_intra_op_parallelism_threads(8L)
tf$config$threading$set_inter_op_parallelism_threads(2L)

# intra‐op: how many threads per single op (e.g. one matmul)
intra <- tf$config$threading$get_intra_op_parallelism_threads()
# inter‐op: how many independent ops at once
inter <- tf$config$threading$get_inter_op_parallelism_threads()

cat("TF intra_op_parallelism_threads =", intra, "\n")
cat("TF inter_op_parallelism_threads =", inter, "\n")

cat("OMP_NUM_THREADS        =", Sys.getenv("OMP_NUM_THREADS"), "\n")
cat("MKL_NUM_THREADS        =", Sys.getenv("MKL_NUM_THREADS"), "\n")
cat("OPENBLAS_NUM_THREADS   =", Sys.getenv("OPENBLAS_NUM_THREADS"), "\n")
library(keras3)
#reticulate::use_virtualenv("/home/sstoeckl/Packages/python/")
reticulate::use_virtualenv("r-reticulate", required = TRUE)
# check for python availability and whether modules are installed
reticulate::py_config()
reticulate::py_module_available("tensorflow")
reticulate::py_module_available("keras")
# tf$config$threading$set_intra_op_parallelism_threads(32)  # Adjust as needed, start around 32
# tf$config$threading$set_inter_op_parallelism_threads(32)
library(dplyr)
library(tibble)
library(PerformanceAnalytics)
# devtools::load_all()
devtools::load_all("/home/apischetsrieder/Thesis/InvestigatoR")

# library(RPostgres)
# library(RSQLite)
# library(data.table)
# library(dtplyr)
# tidy_finance <- dbConnect(
#   SQLite(),
#   "/home/shared/data/tidy_finance.sqlite",
#   extended_types = TRUE
# )



### Configurations for Different Settings

#### 1. Unlimited Deviations

load(file = "/home/apischetsrieder/Thesis/InvestigatoR/data-raw/sp500_m_signals.RData", verbose=TRUE)

return_label <- "ret"
colnames(sp500_m_signals)
features <- c("div_yield_st", "ep", "mom12m", "idio_vol3f", "investment", "bm", "am","fr","gp","ms","noa","ps","rd","ro_e","sp","vol_mkt")
rolling <- TRUE
window_size <- "5 years"
step_size <- "1 year"
offset <- "1 month"
in_sample <- TRUE



# Configuration for Unlimited Deviations
config_unlimited_sharpe <- list(
  layers = list(
    list(type = "dense", units = 64, activation = "relu",dropout = 0.3),
    list(type = "dense", units = 32, activation = "relu",dropout = 0.2),
    list(type = "dense", units = 16, activation = "relu"),
    list(type = "dense", units =  8, activation = "relu"),
    list(type = "dense", units = 1, activation = activation_box_sigmoid(min_weight = -1, max_weight = 1))  # Bound delta_w
  ),
  loss = list(
    #name = "sharpe_ratio_difference_loss",
    name = "information_ratio_loss_active_returns",
    lambda_l1 = 0.0,
    lambda_l2 = 0.0
  ),
  optimizer = list(name = "optimizer_adam", learning_rate = 0.0005), #originally 0.001
  metrics = list(distance_from_benchmark_l1_metric(), distance_from_benchmark_l2_metric()),
  callbacks = list(
   callback_early_stopping(monitor = "loss", min_delta = -0.0001, patience = 8), #patience = 10 originally
   callback_reduce_lr_on_plateau( monitor    = "loss", factor     = 0.5, patience   = 4, min_lr     = 1e-6 ) #added
   ),
  epochs = 80, #originally 60
  batch_size = 256, #originally 128
  verbose = 1,
  seeds = c(19), #(1,13,19,42,57)
  plot_training = FALSE,
  plot_result = FALSE
)

#### 2. Limited Deviations

# Configuration for Limited Deviations (+-20%)
config_limited_sharpe <- config_unlimited_sharpe
config_limited_sharpe$layers[[3]]$activation <- activation_box_sigmoid(min_weight = -0.2, max_weight = 0.2)

#### 3. Regularized Deviations (L1 and L2)

# Configuration for Limited Deviations with L1 and L2 Regularization
config_regularized_sharpe <- config_limited_sharpe
config_regularized_sharpe$loss$lambda_l1 <- 0.001  # L1 regularization
config_regularized_sharpe$loss$lambda_l2 <- 0.001  # L2 regularization

### Backtesting

# Define portfolio config with multiple Keras configurations
pf_config1 <- list(
  keras_unlimited_bm_sharpe = list(
    weight_func = "keras_weights_new",  # originally keras_weights
    config = config_unlimited_sharpe
  )
)
# Run the backtest with multiple Keras configurations
keras_portfolios1 <- backtesting_weights_new(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config1,  # Portfolio config with multiple Keras configurations
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 10,
  verbose = TRUE,
)
save(keras_portfolios1, file = "keras_portfolios1-v2.RData")


# Define portfolio config with multiple Keras configurations
pf_config2 <- list(
  keras_limited_bm_sharpe = list(
    weight_func = "keras_weights",
    config = config_limited_sharpe
  )
)
keras_portfolios2 <- backtesting_weights(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config2,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios1,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = FALSE
)
save(keras_portfolios2, file = "keras_portfolios2-v2.RData")

# Define portfolio config with multiple Keras configurations
pf_config3 <- list(
  keras_regularized_bm_sharpe = list(
    weight_func = "keras_weights",
    config = config_regularized_sharpe
  )
)
keras_portfolios3 <- backtesting_weights(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config3,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios2,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = TRUE
)
save(keras_portfolios3, file = "keras_portfolios3-v2.RData")

## Portfolio Optimization Settings (Information Ratio 1)

# Configuration for Information Ratio Loss based on Active Returns
config_unlimited_ir1 <- config_unlimited_sharpe
config_unlimited_ir1$loss$name <- "information_ratio_loss_active_returns"
config_limited_ir1 <- config_limited_sharpe
config_limited_ir1$loss$name <- "information_ratio_loss_active_returns"
config_regularized_ir1 <- config_regularized_sharpe
config_regularized$loss$name <- "information_ratio_loss_active_returns"

### Backtesting

# Define portfolio config with multiple Keras configurations
pf_config4 <- list(
  keras_unlimited_bm_ir1 = list(
    weight_func = "keras_weights",
    config = config_unlimited_ir1
  )
)
keras_portfolios4 <- backtesting_weights(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config4,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios3,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = TRUE
)
save(keras_portfolios4, file = "keras_portfolios4-v2.RData")

# Define portfolio config with multiple Keras configurations
pf_config5 <- list(
  keras_limited_bm_ir1 = list(
    weight_func = "keras_weights",
    config = config_limited_ir1
  )
)
keras_portfolios5 <- backtesting_weights(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config5,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios4,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = TRUE
)
save(keras_portfolios5, file = "keras_portfolios5-v2.RData")

# Define portfolio config with multiple Keras configurations
pf_config6 <- list(
  keras_regularized__bm_ir1 = list(
    weight_func = "keras_weights",
    config = config_regularized_ir1
  )
)
keras_portfolios6 <- backtesting_weights(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config6,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios5,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = TRUE
)
save(keras_portfolios6, file = "keras_portfolios6-v2.RData")

## Portfolio Optimization Settings (Information Ratio 1)

# Configuration for Information Ratio Loss based on Active Returns
config_unlimited_ir2 <- config_unlimited_sharpe
config_unlimited_ir2$loss$name <- "information_ratio_loss_regression_based"
config_limited_ir2 <- config_limited_sharpe
config_limited_ir2$loss$name <- "information_ratio_loss_regression_based"
config_regularized_ir2 <- config_regularized_sharpe
config_regularized_ir2$loss$name <- "information_ratio_loss_regression_based"

### Backtesting

# Define portfolio config with multiple Keras configurations
pf_config7 <- list(
  keras_unlimited_bm_ir2 = list(
    weight_func = "keras_weights",
    config = config_unlimited_ir2
  )
)
keras_portfolios7 <- backtesting_weights(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config7,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios6,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = TRUE
)
save(keras_portfolios7, file = "keras_portfolios7-v2.RData")

# Define portfolio config with multiple Keras configurations
pf_config8 <- list(
  keras_limited_bm_ir2 = list(
    weight_func = "keras_weights",
    config = config_limited_ir2
  )
)
keras_portfolios8 <- backtesting_weights(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_weight",
  mask_label = "sp500_flag",
  features = features,
  pf_config = pf_config8,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios7,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = TRUE
)
save(keras_portfolios8, file = "keras_portfolios8-v2.RData")

# Define portfolio config with multiple Keras configurations
pf_config9 <- list(
  keras_regularized_bm_ir2 = list(
    weight_func = "keras_weights_new",
    config = config_regularized_ir2
  )
)

keras_portfolios9_bigger_net <- backtesting_weights_new(
  data = sp500_m_signals,
  return_label = return_label,
  benchmark_label = "bm_new",
  mask_label = "sp500_flag_new",
  features = features,
  pf_config = pf_config9,  # Portfolio config with multiple Keras configurations
  # portfolio_object = keras_portfolios8,
  rolling = TRUE,
  window_size = window_size,
  step_size = step_size,
  offset = offset,
  in_sample = in_sample,
  num_cores = 1,
  verbose = TRUE
)

save(keras_portfolios9_bigger_net, file = "keras_portfolios9v3_new_bm.RData")
