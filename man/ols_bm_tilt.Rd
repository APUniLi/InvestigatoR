% Please edit documentation in R/weight_pred_functions.R
\name{ols_bm_tilt}
\alias{ols_bm_tilt}
\title{OLS Benchmark Weight Prediction Function}
\usage{
ols_bm_tilt(train_x, train_y, train_meta, test_x, test_meta, model_config = list())
}
\arguments{
\item{train_x}{Matrix of predictor variables for the training sample.}
\item{train_y}{Numeric vector of returns for the training sample.}
\item{train_meta}{Data frame with \code{stock_id}, \code{date}, and \code{benchmark} columns for the training sample. Included for interface compatibility.}
\item{test_x}{Matrix of predictors for the prediction sample.}
\item{test_meta}{Data frame with \code{stock_id} and \code{date} for the prediction sample.}
\item{model_config}{Optional list of parameters (currently unused).}
}
\value{
A tibble with \code{stock_id}, \code{date}, and \code{pred_weight} columns.
}
\description{
Simple baseline to forecast benchmark tilts using multiple linear regression. The function fits an ordinary least squares model of returns on the provided features for each training window and converts the predicted returns into benchmark-relative weights. Predicted returns are demeaned so the weights sum to zero and scaled so the sum of absolute weights equals one. Weights are finally clipped to the range [-1, 1].
}
\seealso{\code{\link{backtesting_weights_new}}}
\examples{
\dontrun{
# Train/test matrices and metadata should be prepared separately
}}
