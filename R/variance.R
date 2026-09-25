#' @rdname moments
#' @export
variance <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  name_by_variables(eval_property(distribution, "variance"), distribution)
}
