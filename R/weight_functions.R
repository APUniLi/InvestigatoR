#' Create weights based on predictions with given constraints
#'
#' @param return_predictions tibble of stock_id, date, and various predictions
#' @param errors tibble of stock_id, date, and various errors
#' @param constraints list of constraints for the weights. possible values are:
#'  - quantiles: list with long and short quantiles (default: median portfolios)
#'  - allow_short_sale: logical, if TRUE, allow short sales (default: FALSE)
#'  - max_weight: numeric, maximum weight for a single stock (default: 0.1)
#'  - min_weight: numeric, minimum weight for a single stock (default: -0.1)
#'  - b: numeric, in case of allow_short_sale=TRUE this is the max weight sum in both legs,
#'  in case of allow_short_sale=FALSE this is the max weight sum in long leg
#'
#' @return tibble with stock_id, date, and weight
#'
#' @importFrom dplyr arrange group_by select mutate
#' @importFrom purrr map reduce
#'
#' @export
#'
#' @examples
#' constraints <- list(
#'   quantiles = list(long = 0.10, short = 0.10)
#'  )
#'
#'  # Example return predictions
#'  return_predictions <- data.frame(
#'    stock_id = sample(1:100, 100, replace = TRUE),
#'    date = seq.Date(from = Sys.Date() - 100, by = "day", length.out = 100),
#'    pred = runif(100, -1, 1)
#'    )
quantile_weights <- function(return_predictions, errors, constraints=NULL) {
  # Define default constraints if none provided
  if (is.null(constraints)) {
    constraints <- list(
      quantiles = list(long = 0.10, short = 0.10),
      allow_short_sale = TRUE,
      min_weight = -0.1,  # Allows short sales up to -5%
      max_weight = 0.1,   # Max weight per position
      # position_limit = 100,  # Limits active positions
      b = 10#,  # max weight sum in both legs
      # diversification = TRUE  # Apply diversification constraints
    )
  }
  weights <- lapply(colnames(return_predictions[-(1:2)]), function(pred_col) {
    pred_data <- return_predictions %>%
      dplyr::select(stock_id, date, pred=!!pred_col)
  if (constraints$allow_short_sale) {
    # generate quantile portfolios
    weights <- pred_data %>%
      dplyr::arrange(stock_id, date) %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(
        quantile_long = quantile(pred, probs = 1 - constraints$quantiles$long),
        quantile_short = quantile(pred, probs = constraints$quantiles$short),
        weight = ifelse(pred>=quantile_long,1,ifelse(pred<=quantile_short,-1,0))) |>
      dplyr::mutate( # apply max/min weights
        weight = pmax(weight, constraints$min_weight),
        weight = pmin(weight, constraints$max_weight)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate( # apply sum constraint
        short=ifelse(weight<0,1,0)) |>
      dplyr::group_by(date,short) |>
      dplyr::mutate(weight = weight/sum(weight)*constraints$b) |>
      dplyr::ungroup() |>
      dplyr::select(stock_id,date,weight)
  } else {
    # generate quantile portfolios
    weights <- pred_data %>%
      dplyr::arrange(stock_id, date) %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(
        quantile_long = quantile(pred, probs = 1 - constraints$quantiles$long),
        weight = ifelse(pred>=quantile_long,1,0)) |>
      dplyr::mutate( # apply max/min weights
        weight = pmax(weight, -0.1)
      ) |>
      dplyr::group_by(date) |>
      dplyr::mutate( # apply sum constraint
        weight = weight/sum(weight)*constraints$b) |>
      dplyr::ungroup() |>
      dplyr::select(stock_id,date,weight)
  }
  })
  weights <- weights |> purrr::reduce(left_join, by = c("stock_id", "date")) |>
    dplyr::select(stock_id, date, everything())
  return(weights)
}

#' Create weights based on various predictions, potentially using their errors with given constraints
#'
#' @param return_predictions tibble of stock_id, date, and various predictions
#' @param errors tibble of stock_id, date, and various prediction errors
#' @param method method to use for ensemble weights. Possible values are:
#' - simple_average: simple average of predictions
#' - weighted_average: weighted average based on the inverse of average errors
#' - error_covariance: using the covariance of errors to calculate weights
#'
#'
#' @return tibble with stock_id, date, and weight
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
ensemble_weights <- function(return_predictions, errors, method = "simple_average") {
  # Validate inputs
  if (!is.data.frame(return_predictions) || !is.data.frame(errors)) {
    stop("Both predictions and errors should be data frames with the same structure.")
  }

  # Ensure predictions and errors align
  if (nrow(return_predictions) != nrow(errors) || ncol(return_predictions) != ncol(errors)) {
    stop("Predictions and errors must have the same dimensions.")
  }

  # Define weights and ensemble prediction
  n <- ncol(return_predictions)
  ensemble_prediction <- rep(0, nrow(return_predictions))

  if (method == "simple_average") {
    # Simple average of predictions
    ensemble_prediction <- rowMeans(return_predictions, na.rm = TRUE)

  } else if (method == "weighted_average") {
    # Weighted average based on the inverse of average errors
    average_errors <- rowMeans(errors, na.rm = TRUE)
    weights <- 1 / average_errors
    weights <- weights / sum(weights)  # Normalize weights
    ensemble_prediction <- return_predictions %*% weights

  } else if (method == "error_covariance") {
    # Using the covariance of errors to calculate weights
    E <- errors
    cov_E <- cov(E, use = "pairwise.complete.obs")
    inv_cov_E <- solve(cov_E)
    one_vector <- rep(1, n)
    weights <- inv_cov_E %*% one_vector
    weights <- weights / sum(weights)  # Normalize weights
    ensemble_prediction <- return_predictions %*% weights

  } else {
    stop("Specified method is not supported. Choose from 'simple_average', 'weighted_average', or 'error_covariance'.")
  }

  return(ensemble_prediction)
}

