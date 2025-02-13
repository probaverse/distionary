#' @rdname moments
#' @export
variance <- function(distribution) {
  eval_representation(distribution, "variance")
}

