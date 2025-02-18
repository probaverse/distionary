#' @noRd
eval_realise_from_network <- function(distribution, n) {
  u <- stats::runif(n)
  eval_quantile(distribution, at = u)
}
