#' @rdname moments
#' @export
stdev <- function(distribution) {
  UseMethod("stdev")
}


#' @rdname moments
#' @export
stdev.dst <- function(distribution) {
  ss <- variance(distribution)
  sqrt(ss)
}

#' @rdname moments
#' @export
stdev2 <- function(distribution) {
  e <- repres_env(distribution)
  p <- parameters(distribution)
  eval_tidy(e$.stdev, data = p, env = e)
}
