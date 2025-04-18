% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappers.R
\name{retpred_map}
\alias{retpred_map}
\title{Return Prediction Mapping Function (Helper)}
\usage{
retpred_map(t, data_subset, indices, model_function, model_config)
}
\arguments{
\item{t}{Integer. Index of the current iteration.}

\item{data_subset}{Tibble. Subset of the data containing \code{stock_id}, \code{date}, \code{return_label}, and features.}

\item{indices}{Tibble. Contains \code{training_start}, \code{training_end}, \code{prediction_start}, and \code{prediction_end} dates.}

\item{model_function}{Character. Name of the prediction (model) function to be invoked.}

\item{model_config}{List. Configuration parameters for the model function.}

\item{mask}{Tibble. \code{stock_id}-\code{date} combinations in \code{data_subset} that are part of the index. Will be filtered
at \code{training_end} date to include exactly those stocks that are part of the index at training-date.}
}
\value{
Tibble containing \code{stock_id}, \code{date}, and \code{pred_return}.
}
\description{
This helper function is used within \code{backtesting_returns} to map over time indices and generate return predictions using specified ML models.
}
