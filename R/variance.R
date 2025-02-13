#' @rdname moments
#' @export
variance <- function(distribution) {
  eval_property(distribution, "variance")
}

