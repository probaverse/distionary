#' @export
eval_density.dst <- function(distribution, at, strict = TRUE, ...) {
  if (variable(distribution) == "continuous") {
    stop("Cannot find this distribution's density function.")
  }
  if (strict) {
    stop("This distribution does not have a density function. ",
         "Maybe you want to evaluate outside of strict mode?")
  } else {
    if (variable(distribution) == "discrete") {
      return(rep(0, length(at)))
    } else {
      stop("Cannot find the derivative of the cdf.")
    }
  }
}

#' @export
eval_density.bidst <- function(distribution, at, strict = TRUE, ...) {
  stop("Expecting a univariate distribution; received bivariate.")
}

#' @export
eval_density.multidst <- function(distribution, at, strict = TRUE, ...) {
  d <- dimension(distribution)
  if (d == 1) {
    return(eval_multi_density(distribution, .l = list(at)))
  }
  stop("Expecting a univariate distribution; received ", d, "-variate.")
}
