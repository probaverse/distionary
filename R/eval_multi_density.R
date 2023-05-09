#' @rdname eval_density
#' @export
eval_multi_density <- function(distribution, .l, strict = TRUE, ...) {
  UseMethod("eval_multi_density")
}

#' @export
eval_multi_density.dst <- function(distribution, .l, strict = TRUE, ...) {
  if (length(.l) != 1) {
    stop("For a univariate distribution, `.l` must contain one vector, not ",
         length(.l), ".")
  }
  eval_density(distribution, at = .l[[1]])
}

#' @export
eval_multi_density.bidst <- function(distribution, .l, strict = TRUE, ...) {
  if (length(.l) != 2) {
    stop("For a bivariate distribution, `.l` must have two vectors, not ",
         length(.l), ".")
  }
  eval_bi_density(distribution, x = .l[[1]], y = .l[[2]])
}

#' @export
eval_multi_density.multidst <- function(distribution, .l, strict = TRUE, ...) {
  if (all(variable(distribution) == "continuous")) {
    stop("Cannot find this distribution's density function.")
  }
  if (strict) {
    stop("This distribution does not have a density function. ",
         "Maybe you want to evaluate outside of strict mode?")
  } else {
    if (all(variable(distribution) == "discrete")) {
      return(rep(0, vctrs::vec_size_common(!!!.l)))
    } else {
      stop("Cannot find the derivative of the cdf.")
    }
  }
}
