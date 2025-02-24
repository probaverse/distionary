#' @rdname moments
#' @export
variance <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  eval_property(distribution, "variance")
}
