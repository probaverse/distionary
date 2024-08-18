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
  eval_tidy_if_possible(e$.stdev, data = p, env = e)
  # QUESTION:
  # - If this does not evaluate because parameters are not refined,
  #   should it return an expression? Or would it be better if that
  #   behaviour came from a deliberate function like
  #   `view_representation(distribution, "stdev")`?
  # - eval_tidy() returns a zero-length vector when parameters are NULL.
  #   Should the NULLs be removed first, then?
}
