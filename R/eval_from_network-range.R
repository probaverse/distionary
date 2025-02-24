#' @noRd
eval_range_from_network <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  eval_quantile(distribution, at = 0:1)
}
