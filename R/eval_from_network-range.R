#' @noRd
eval_range_from_network <- function(distribution) {
  eval_quantile(distribution, at = 0:1)
}
