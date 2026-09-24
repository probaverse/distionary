#' @rdname moments
#' @export
skewness <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  assert_univariate(distribution, "skewness")
  eval_property(distribution, "skewness")
}
