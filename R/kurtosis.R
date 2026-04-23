#' @rdname moments
#' @export
kurtosis <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  eval_property(distribution, "kurtosis")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  eval_property(distribution, "kurtosis_exc")
}
