#' @export
eval_multi_cdf.dst <- function(distribution, .l) {
  if (length(.l) != 1) {
    stop("For a univariate distribution, `.l` must contain one vector, not ",
         length(.l), ".")
  }
  eval_cdf(distribution, at = .l[[1]])
}

#' @export
eval_multi_cdf.bidst <- function(distribution, .l) {
  if (length(.l) != 2) {
    stop("For a bivariate distribution, `.l` must have two vectors, not ",
         length(.l), ".")
  }
  eval_bi_cdf(distribution, x = .l[[1]], y = .l[[2]])
}

#' @export
eval_multi_cdf.multidst <- function(distribution, .l) {
  stop("Can't find a cdf for this distribution.")
}
