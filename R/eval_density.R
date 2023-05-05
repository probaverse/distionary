#' Probability Density Function
#'
#' Access a distribution's probability density function (pdf).
#'
#' @inheritParams eval_cdf
#' @return The evaluated density
#' in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @examples
#' d <- dst_unif(0, 4)
#' eval_density(d, at = 0:4)
#' enframe_density(d, at = 0:4)
#' eval_density(dst_pois(1), at = 0:4, strict = FALSE)
#' @family distributional representations
#' @rdname density
#' @export
eval_density <- function(distribution, at) UseMethod("eval_density")

#' @param strict Only evaluate when the density exists? `TRUE` if so;
#' if `FALSE`, evaluates the derivative of the cdf.
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

#' @inheritParams eval_density.dst
#' @param strict Only evaluate when the density exists? `TRUE` if so;
#' if `FALSE`, evaluates the derivative of the cdf.
#' @export
eval_bi_density <- function(distribution, x, y, strict = TRUE, ...) {
  .l <- vctrs::vec_recycle_common(x, y)
  eval_multi_density(distribution, .l = .l, ...)
}

#' @inheritParams eval_density.dst
#' @param strict Only evaluate when the density exists? `TRUE` if so;
#' if `FALSE`, evaluates the derivative of the cdf.
#' @export
eval_multi_density.multi_dst <- function(distribution, .l, strict = TRUE, ...) {
  n <- vctrs::vec_size_common(!!!.l)
  if (variable(distribution) == "continuous") {
    stop("Cannot find this distribution's density function.")
  }
  if (strict) {
    stop("This distribution does not have a density function. ",
         "Maybe you want to evaluate outside of strict mode?")
  } else {
    if (variable(distribution) == "discrete") {
      return(rep(0, n))
    } else {
      stop("Cannot find the derivative of the cdf.")
    }
  }
}


#' @rdname density
#' @export
enframe_density <- function(..., at, arg_name = ".arg",
                            fn_prefix = "density",
                            sep = "_", strict = TRUE) {
  .l <- rlang::set_names(list(at), arg_name)
  enframe_general(..., .l = .l, fn_prefix = fn_prefix, sep = sep,
                  eval_fn = eval_cdf)
}

#' @rdname density
#' @export
enframe_bi_density <- function(..., x, y, arg_name = NULL,
                               fn_prefix = "density",
                               sep = "_", strict = TRUE) {
  enframe_bivariate(..., x = x, y = y,
                    arg_name = arg_name, fn_prefix = fn_prefix,
                    sep = sep, eval_fn = eval_density)
}

#' @rdname density
#' @export
enframe_density.multi_dst <- function(..., .l, arg_name = NULL,
                                      fn_prefix = "density",
                                      sep = "_", strict = TRUE) {
  enframe_multivariate(..., .l,
                       arg_name = arg_name, fn_prefix = fn_prefix,
                       sep = sep, eval_fn = eval_density)
}
