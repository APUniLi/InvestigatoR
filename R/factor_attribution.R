#' Factor Attribution Analysis
#'
#' Computes forecast accuracy for factor predictions and decomposes
#' portfolio returns into factor contributions.
#'
#' @param weights Tibble of portfolio weights with columns `stock_id`,
#'   `date` and one column per portfolio containing weights.
#' @param betas Tibble of factor loadings with columns `stock_id` and one
#'   column per factor.
#' @param factor_pred Tibble of predicted factor returns with columns
#'   `date` and one column per factor.
#' @param factor_real Tibble of realized factor returns with columns
#'   `date` and one column per factor.
#'
#' @return A list with two tibbles: `forecast_accuracy` summarises
#'   correlation and RMSE for each factor, while `contributions` contains
#'   the time series of factor contributions for each portfolio.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- factor_attribution(weights_df, betas_df, pred_df, real_df)
#' res$forecast_accuracy
#' res$contributions
#' }
factor_attribution <- function(weights, betas, factor_pred, factor_real) {
  checkmate::assert_data_frame(weights)
  checkmate::assert_data_frame(betas)
  checkmate::assert_data_frame(factor_pred)
  checkmate::assert_data_frame(factor_real)

  # ---------------- Forecast Accuracy -----------------
  factors <- intersect(names(factor_pred), names(factor_real))
  factors <- setdiff(factors, "date")

  forecast_acc <- purrr::map_dfr(
    factors,
    ~{
      tibble::tibble(
        Factor = .x,
        Correlation = stats::cor(factor_pred[[.x]], factor_real[[.x]],
                                 use = "complete.obs"),
        RMSE = sqrt(mean((factor_pred[[.x]] - factor_real[[.x]])^2,
                          na.rm = TRUE))
      )
    }
  )

  # ---------------- Exposures -------------------------
  weights_long <- weights %>%
    tidyr::pivot_longer(-c(stock_id, date), names_to = "Portfolio",
                        values_to = "Weight")

  beta_long <- betas %>%
    tidyr::pivot_longer(-stock_id, names_to = "Factor", values_to = "Beta")

  exposures <- weights_long %>%
    dplyr::left_join(beta_long, by = "stock_id") %>%
    dplyr::group_by(Portfolio, date, Factor) %>%
    dplyr::summarise(Exposure = sum(Weight * Beta, na.rm = TRUE),
                     .groups = "drop")

  # ---------------- Contributions ---------------------
  factor_long <- factor_real %>%
    tidyr::pivot_longer(-date, names_to = "Factor", values_to = "Return")

  contributions <- exposures %>%
    dplyr::left_join(factor_long, by = c("date", "Factor")) %>%
    dplyr::mutate(Contribution = Exposure * Return)

  list(
    forecast_accuracy = forecast_acc,
    contributions = contributions
  )
}
