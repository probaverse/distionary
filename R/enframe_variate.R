#' Standardize arguments and enframe
#'
#' Internal. An intermediate step in the `enframe_`
#' family of functions, these functions
#' standardize the arguments in which to evaluate the distributional
#' representation at, before calling a general enframer function.
#'
#' @param eval_fn Character name of the eval function to enframe;
#' e.g., `"eval_multi_cdf"`.
#' @inheritParams enframe_cdf
#' @details
#'
#' - The univariate version puts the `at` input into a list,
#'   named by `arg_name`.
#' - The bivariate version recycles `x` and `y` into a common length,
#'   puts them in a list, and names them by `arg_name` if present.
#' - The multivariate version recycles the `.l` input, after making it
#'   a list in the case of a vector input (representing a tuple).
#'
#' @return Calls the `enframe_general()` function with a standardized
#' `.l` input.
#' @rdname enframe_variate
enframe_uni <- function(..., at, arg_name, fn_prefix, sep, eval_fn, fn_args) {
  .l <- rlang::set_names(list(at), arg_name)
  enframe_general(..., .l = .l, fn_prefix = fn_prefix, eval_fn = eval_fn,
                  fn_args = fn_args)
}

#' @rdname enframe_variate
enframe_bi <- function(..., x, y, arg_name, fn_prefix, sep, eval_fn, fn_args) {
  .l <- vctrs::vec_recycle_common(x = x, y = y)
  if (!is.null(arg_name)) {
    names(.l) <- arg_name
  }
  enframe_general(..., .l = .l, fn_prefix = fn_prefix, eval_fn = eval_fn,
                  fn_args = fn_args)
}

#' @rdname enframe_variate
enframe_multi <- function(..., .l, arg_name, fn_prefix, sep, eval_fn, fn_args) {
  if (!is.list(.l)) {
    .l <- as.list(.l)
  }
  .l_enquo <- rlang::quos_auto_name(rlang::enquos(!!!.l))
  .l_names <- rlang::names2(.l_enquo)
  .l_uneven <- lapply(.l_enquo, rlang::eval_tidy)
  .l2 <- vctrs::vec_recycle_common(!!!.l_uneven)
  d <- length(.l2)
  if (!is.null(arg_name)) {
    if (length(arg_name) != d) {
      stop("Specified ", length(arg_name),
           " arguments in `arg_name`, yet there ",
           "are/is ", d, " dimensions of input distribution(s).")
    }
    names(.l2) <- arg_name
  }
  enframe_general(..., .l = .l2, fn_prefix = fn_prefix, eval_fn = eval_fn,
                  fn_args = fn_args)
}
