#' @rdname moments
#' @export
stdev <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  eval_property(distribution, "stdev")
}
