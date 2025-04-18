% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/return_pred_functions.R
\name{caret_wrapper}
\alias{caret_wrapper}
\title{Caret Wrapper Prediction Function}
\usage{
caret_wrapper(train_data, test_data, config = list())
}
\arguments{
\item{train_data}{A data frame containing \code{stock_id}, \code{date}, \code{return_label}, and feature columns.}

\item{test_data}{A data frame containing \code{stock_id}, \code{date}, and feature columns.}

\item{config}{A list containing caret configuration parameters, including \code{method}, \code{tuneGrid}, \code{trControl}, etc.}
}
\value{
A tibble with \code{stock_id}, \code{date}, and \code{pred_return} matching the \code{test_data}.
}
\description{
This function trains machine learning models using the caret package to predict returns based on the provided
training data and makes predictions for the test data. It supports various models with optional hyperparameter tuning.
}
\examples{
\dontrun{
data(data_ml)
train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps",
                                  "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps",
                                  "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
config <- list(
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0.5, lambda = 0.1),
  trControl = trainControl(method = "cv", number = 5),
  metric = "RMSE"
)
caret_predictions <- caret_wrapper(train_data_ex, test_data_ex, config)
}
}
