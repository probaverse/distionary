#' @export
eval_cdf.dst <- function(distribution, at) {
  stop("Can't find a cdf for this distribution.")
}

#' @export
eval_cdf.bidst <- function(distribution, at) {
  stop("Expecting a univariate distribution; received bivariate.")
}

#' @export
eval_cdf.multidst <- function(distribution, at) {
  d <- dimension(distribution)
  if (d == 1) {
    return(eval_multi_cdf(distribution, .l = list(at)))
  }
  stop("Expecting a univariate distribution; received ", d, "-variate.")
}
