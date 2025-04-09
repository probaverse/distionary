#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
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
