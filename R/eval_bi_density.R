#' @rdname eval_density
#' @export
eval_bi_density <- function(distribution, x, y, strict = TRUE, ...) {
  UseMethod("eval_bi_density")
}

#' @export
eval_bi_density.dst <- function(distribution, x, y, strict = TRUE, ...) {
  stop("Expecting a bivariate distribution; received univariate.")
}

#' @export
eval_bi_density.bidst <- function(distribution, x, y, strict = TRUE, ...) {
  if (all(variable(distribution) == "continuous")) {
    stop("Cannot find this distribution's density function.")
  }
  if (strict) {
    stop("This distribution does not have a density function. ",
         "Maybe you want to evaluate outside of strict mode?")
  } else {
    if (all(variable(distribution) == "discrete")) {
      return(rep(0, vctrs::vec_size_common(x, y)))
    } else {
      stop("Cannot find the derivative of the cdf.")
    }
  }
}

#' @export
eval_bi_density.multidst <- function(distribution, x, y, strict = TRUE, ...) {
  d <- dimension(distribution)
  if (d == 2) {
    return(eval_multi_density(distribution, list(xy[[1]], xy[[2]])))
  }
  stop("Expecting a bivariate distribution; received ", d, "-variate.")
}
