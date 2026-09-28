#' @rdname moments
#' @export
kurtosis <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  assert_univariate(distribution, "kurtosis")
  eval_property(distribution, "kurtosis")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  assert_univariate(distribution, "kurtosis_exc")
  eval_property(distribution, "kurtosis_exc")
}
