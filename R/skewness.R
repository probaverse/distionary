#' @rdname moments
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @export
skewness <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  eval_property(distribution, "skewness")
}
