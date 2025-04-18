% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappers.R
\name{weightpred_map}
\alias{weightpred_map}
\title{Weight Prediction Mapping Function (Helper)}
\usage{
weightpred_map(t, data_subset, indices, mask, model_function, model_config)
}
\arguments{
\item{t}{Integer. Index of the current iteration.}

\item{data_subset}{Tibble. Subset of the data containing \code{stock_id}, \code{date}, \code{return_label}, features, and any optional benchmark/masking data.}

\item{indices}{Tibble. Contains \code{training_start}, \code{training_end}, \code{prediction_start}, and \code{prediction_end} dates.}

\item{mask}{Tibble. \code{stock_id}-\code{date} combinations in \code{data_subset} that are part of the index. Will be filtered}

\item{model_function}{Character. Name of the weight prediction function to be invoked.}

\item{model_config}{List. Configuration parameters for the model function.}
}
\value{
Tibble containing \code{stock_id}, \code{date}, and predicted \code{weights}.
}
\description{
This helper function is used within \code{backtesting_weights} to map over time indices and generate weight predictions using specified ML models.
}
\examples{
\dontrun{
data(data_ml)
# Create a subset of data_ml for testing
test_data_ml <- data_ml \%>\%
  filter(stock_id <= 5)

# Define common parameters for testing
return_label <- "R1M_Usd"
features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")

indices <- tibble::tibble(
  training_start = rep(as.Date("2012-01-01"), 2),
  training_end = rep(as.Date("2012-12-31"), 2),
  prediction_start = rep(as.Date("2013-01-01"), 2),
  prediction_end = rep(as.Date("2013-12-31"), 2)
)

# Dummy function for weight prediction (in place of keras_weights)
dummy_weights_func <- function(train_data, test_data, config) {
  tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_weight = runif(nrow(test_data), -1, 1)  # Random weights for example
  )
}
dummy_weights_func(train_data_ml, test_data_ml, config)
# Use weight_map to predict weights for the specified time step
t <- 1  # First time index
weights <- weightpred_map(
  t = t,
  data_subset = test_data_ml,
  indices = indices,
  model_function = "dummy_weights_func",
  model_config = list()
)
print(weights)
}
}
