#' @rdname moments
#' @export
stdev <- function(distribution, ...) {
  UseMethod("stdev")
}

#' @export
stdev.dst <- function(distribution, ...) {
  ss <- variance(distribution, ...)
  sqrt(ss)
}
