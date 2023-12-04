#' Probability Density Function
#'
#' Access a distribution's probability density function (pdf).
#'
#' @inheritParams eval_cdf
#' @param strict Only evaluate when the density exists? `TRUE` if so;
#' if `FALSE`, evaluates the derivative of the cdf.
#' @return The evaluated density in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @examples
#' d <- dst_unif(0, 4)
#' eval_density(d, at = 0:4)
#' enframe_density(d, at = 0:4)
#' eval_density(dst_pois(1), at = 0:4, strict = FALSE)
#' @family distributional representations
#' @rdname density
#' @export
eval_density <- function(distribution, at, strict = TRUE) {
  UseMethod("eval_density")
}


#' @export
eval_density.dst <- function(distribution, at, strict = TRUE) {
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

#' @rdname density
#' @export
enframe_density <- function(..., at, arg_name = ".arg", fn_prefix = "density",
                            sep = "_", strict = TRUE) {
  enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
                  sep = sep, eval_fn = eval_density,
                  fn_args = list(strict = strict))
}
