#' Skewness of a Distribution
#'
#' @param distribution Distribution to compute skewness from.
#'
#' @rdname moments
#' @export
skewness <- function(distribution) {
  eval_representation(distribution, "skewness")
}
