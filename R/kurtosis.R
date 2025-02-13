#' @rdname moments
#' @export
kurtosis <- function(distribution) {
  eval_representation(distribution, "kurtosis")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(distribution) {
  eval_representation(distribution, "kurtosis_exc")
}
