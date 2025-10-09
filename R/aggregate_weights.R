#' Aggregate discrete values
#'
#' Aggregates discrete values together with their weights
#' into a data frame or tibble.
#'
#' @param y Vector of outcomes.
#' @param weights Vector of weights, one for each of `y`.
#' These need not sum to one, but must not be negative and non-NA.
#' @param sum_to_one Logical; should the weights be normalized
#' to sum to 1? Default is FALSE.
#' @return Data frame with the following columns:
#' - `y`: Increasing vector of unique values of `y` that have positive weight.
#' - `weight`: Weights corresponding to each outcome.
#' @details
#' For a vector of outcomes `y` with a matching vector of `weights`,
#' `aggregate_weights()` provides a single non-zero, non-NA
#' weight per unique value of `y`.
aggregate_weights <- function(y,
                              weights,
                              sum_to_one = FALSE) {
  checkmate::assert_numeric(y)
  checkmate::assert_numeric(weights, lower = 0, any.missing = FALSE)
  checkmate::assert_logical(sum_to_one, len = 1L, any.missing = FALSE)
  if (length(y) != length(weights)) {
    stop("`y` and `weights` must have the same length.")
  }
  zero_w <- weights == 0
  y <- y[!zero_w]
  weights <- weights[!zero_w]
  if (length(y) == 0L) {
    return(convert_dataframe_to_tibble(
      data.frame(location = numeric(), size = numeric())
    ))
  }
  if (sum_to_one) {
    weights <- weights / sum(weights)
  }
  grps <- match(y, y)
  agg_w <- tapply(weights, grps, FUN = sum, simplify = TRUE)
  agg_y <- tapply(y, grps, FUN = unique, simplify = TRUE)
  df <- data.frame(y = agg_y, weight = agg_w)
  convert_dataframe_to_tibble(df)
}
