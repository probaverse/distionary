#' @rdname moments
#' @export
stdev <- function(distribution) {
  eval_representation(distribution, "stdev")
}


#' @rdname moments
#' @export
stdev.dst <- function(distribution) {
  ss <- variance(distribution)
  sqrt(ss)
}
