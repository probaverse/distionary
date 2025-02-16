#' @rdname moments
#' @export
kurtosis <- function(distribution) {
  eval_property(distribution, "kurtosis")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(distribution) {
  eval_property(distribution, "kurtosis_exc")
}
