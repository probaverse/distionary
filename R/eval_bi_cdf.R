#' @export
eval_bi_cdf.dst <- function(distribution, x, y) {
  stop("Expecting a bivariate distribution; received univariate.")
}

#' @export
eval_bi_cdf.bidst <- function(distribution, x, y) {
  stop("Cannot find a cdf for this distribution.")
}

#' @export
eval_bi_cdf.multidst <- function(distribution, x, y) {
  d <- dimension(distribution)
  if (d == 2) {
    return(eval_multi_cdf(distribution, list(xy[[1]], xy[[2]])))
  }
  stop("Expecting a bivariate distribution; received ", d, "-variate.")
}
