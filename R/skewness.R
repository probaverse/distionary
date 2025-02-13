#' @rdname moments
#' @export
skewness <- function(distribution) {
  eval_property(distribution, "skewness")
}
