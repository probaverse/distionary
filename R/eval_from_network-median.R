#' @noRd
eval_median_from_network <- function(distribution) {
  eval_quantile(distribution, at = 0.5)
}
