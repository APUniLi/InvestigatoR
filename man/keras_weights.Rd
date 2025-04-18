% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weight_pred_functions.R
\name{keras_weights}
\alias{keras_weights}
\title{Keras Weight Prediction Function with Benchmark Handling}
\usage{
keras_weights(train_data, test_data, model_config = list())
}
\arguments{
\item{train_data}{A data frame with columns: \code{stock_id}, \code{date}, \code{benchmark}, \code{actual_return}, and features.}

\item{test_data}{A data frame with columns: \code{stock_id}, \code{date}, and features.}

\item{config}{A list containing the configuration for the Keras model, including layers, optimizer, loss function, and advanced features like callbacks.}
}
\value{
A tibble with \code{stock_id}, \code{date}, and predicted portfolio weights for the test data.
}
\description{
This function trains a Keras neural network to predict portfolio weights based on the provided training data.
The Keras model structure is passed in the \code{config} argument, and you can define custom layers, optimizers, loss functions, and callbacks (like early stopping).
Additionally, this function can handle benchmark weights, which will be passed to the loss function as part of \code{y_true}.
}
\details{
\subsection{Config Structure}{
\itemize{
\item \code{layers}: A list of layer definitions.
\item \code{loss}: The loss function to use (e.g., Sharpe ratio loss).
\item \code{optimizer}: A list specifying the optimizer.
\item \code{epochs}: Number of epochs to train the model.
\item \code{batch_size}: Batch size for training.
\item \code{verbose}: Verbosity level for Keras training.
\item \code{callbacks}: List of Keras callback functions.
\item \code{seeds}: A list of seeds for reproducibility and multiple prediction averaging.
\item \code{python_env}: Python environment to use for training.
}
}
}
\examples{
\dontrun{
library(keras3)
reticulate::use_virtualenv("C:/R/python/")
data(data_ml)
test_data_ml <- data_ml \%>\% filter(stock_id <= 29)
features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
train_data_ex <- test_data_ml |> filter(date<="2012-12-31") |>
  group_by(date) |>
  mutate(benchmark = Mkt_Cap_3M_Usd/sum(Mkt_Cap_3M_Usd)) |> ungroup() |>
  select(stock_id,date,benchmark,return=R1M_Usd,all_of(features)) |> arrange(date,stock_id)
test_data_ex <- test_data_ml |> filter(date>"2012-12-31") |>
  group_by(date) |>
  mutate(benchmark = Mkt_Cap_3M_Usd/sum(Mkt_Cap_3M_Usd)) |> ungroup() |>
  select(stock_id,date,benchmark,all_of(features)) |> arrange(date,stock_id)

config <- list(
  layers = list(
    list(type = "dense", units = 32, activation = "relu"),
    list(type = "dense", units = 16, activation = "relu"),
    list(type = "dense", units = 1, activation = "linear")
  ),
  loss = list(name = "sharpe_ratio_loss",
      transaction_costs = 0.001, # prevent too much turnover
      delta = 0.1,               # Diversification target
      lambda = 0.1,              # Diversification penalty multiplier
      leverage = 1.0,            # Target leverage
      eta = 0                  # Leverage penalty multiplier
      ),  # Custom Sharpe ratio loss
  optimizer = list(name = "optimizer_rmsprop", learning_rate = 0.001),
  metrics = list(turnover_metric(), leverage_metric(), diversification_metric(), dummy_mse_loss ),  # Custom metrics added here
  callbacks = list(
    callback_early_stopping(monitor = "loss", min_delta = 0.001, patience = 3)
  ),
  epochs = 10,
  batch_size = 128,
  verbose = 1,
  seeds = c(42, 123, 456),
  plot_training = TRUE,
  plot_result = TRUE
)
weights <- keras_weights(train_data_ex, test_data_ex, config)
print(weights)
}
}
