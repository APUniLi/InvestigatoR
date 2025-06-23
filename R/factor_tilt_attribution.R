#' Factor Exposure of Benchmark Tilts
#'
#' Computes factor exposures of portfolio tilts relative to a benchmark.
#' Optionally multiplies those exposures with realized factor returns
#' to obtain factor contributions.
#'
#' @param delta_weights Tibble with columns `stock_id`, `date` and one
#'   column per portfolio containing weights relative to the benchmark.
#' @param betas Tibble of factor loadings with columns `stock_id` and
#'   one column per factor.
#' @param factor_real Optional tibble of realized factor returns with
#'   columns `date` and one column per factor.
#'
#' @return If `factor_real` is `NULL`, a tibble of factor exposures.
#'   Otherwise a list with `exposures` and `contributions`.
#' @export
#'
#' @examples
#' # exposures <- factor_tilt_attribution(delta_weights, betas)
#' 
factor_tilt_attribution <- function(delta_weights, betas, factor_real = NULL) {
  checkmate::assert_data_frame(delta_weights)
  checkmate::assert_data_frame(betas)
  if (!is.null(factor_real)) checkmate::assert_data_frame(factor_real)

  weights_long <- delta_weights %>%
    tidyr::pivot_longer(-c(stock_id, date), names_to = "Portfolio",
                        values_to = "Tilt")

  beta_long <- betas %>%
    tidyr::pivot_longer(-stock_id, names_to = "Factor", values_to = "Beta")

  exposures <- weights_long %>%
    dplyr::left_join(beta_long, by = "stock_id") %>%
    dplyr::group_by(Portfolio, date, Factor) %>%
    dplyr::summarise(Exposure = sum(Tilt * Beta, na.rm = TRUE),
                     .groups = "drop")

  if (is.null(factor_real)) {
    return(exposures)
  }

  factor_long <- factor_real %>%
    tidyr::pivot_longer(-date, names_to = "Factor", values_to = "Return")

  contributions <- exposures %>%
    dplyr::left_join(factor_long, by = c("date", "Factor")) %>%
    dplyr::mutate(Contribution = Exposure * Return)

  list(
    exposures = exposures,
    contributions = contributions
  )
}
