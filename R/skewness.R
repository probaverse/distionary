#' @rdname moments
#' @export
skewness <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  eval_property(distribution, "skewness")
}
