#' @noRd
eval_realise_from_network <- function(distribution, n) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_integerish(n, len = 1)
  u <- stats::runif(n)
  eval_quantile(distribution, at = u)
}
