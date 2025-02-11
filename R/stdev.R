#' @rdname moments
#' @export
stdev <- function(distribution) {
  eval_representation(distribution, "stdev")
}

eval_stdev_from_network <- function(distribution) {
  ss <- variance(distribution)
  sqrt(ss)
}
