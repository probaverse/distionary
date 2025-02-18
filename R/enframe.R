#' Enframe a distributional representation
#'
#' This is the workhorse for the `enframe_` family of functions.
#' `enframe_general()` evaluates a specified distributional representation
#' for multiple distributions, and places the results in a data frame
#' or tibble.
#'
#' @inheritParams eval_cdf
#' @param ... Distributions to enframe; optionally, named.
#' @param eval_fn Name of the `eval_` function for the desired distributional
#' representation, such as `eval_cdf` and `eval_density`.
#' @param fn_args A named list of arguments to pass to the `eval_fn` function,
#' besides the distribution and `at` argument.
#' @returns A data frame or tibble of the input argument (`at`), with the
#' evaluated distributional representation for each distribution in
#' `...` in its own column.
#' @details If only one distribution is specified in `...`, then the evaluation
#' column will be named that of `fn_prefix`.
#'
#' If more than one distribution
#' is specified in `...`, the evaluation columns will be named by the
#' prefix `fn_prefix` followed by the distribution names, with `sep` in between.
#'
#' Distributions are named first by their argument names, if given, or if not,
#' the input text. Names are then made unique using `vctrs::vec_as_names()`
#' with the "unique" names repair. "Unique" is chosen instead of "universal"
#' because names are anticipated to be syntactic with the `eval_fn` prefix;
#' "minimal" is not sufficient because it may result in columns having the
#' same names.
#' @examples
#' d <- dst_norm(0, 1)
#' distionary:::enframe_general(
#'   d,
#'   at = 1:9 / 10, arg_name = "tau", fn_prefix = "quantile", sep = "",
#'   eval_fn = eval_quantile
#' )
#'
enframe_general <- function(..., at, arg_name, fn_prefix, sep,
                            eval_fn, fn_args = list()) {
  ellipsis <- rlang::quos(...)
  ellipsis <- rlang::quos_auto_name(ellipsis)
  distributions <- lapply(ellipsis, rlang::eval_tidy)
  is_distributions <- vapply(
    distributions, is_distribution,
    FUN.VALUE = logical(1L)
  )
  if (!all(is_distributions)) {
    stop(
      "`enframe_*()` functions only accept distributions. ",
      "Entries that are not distributions: ",
      paste(which(!is_distributions), collapse = ", ")
    )
  }

  n <- length(distributions)
  if (n == 0L) {
    stop("Need at least one distribution in the `enframe_*()` function.")
  }
  f <- list()
  for (i in seq_len(n)) {
    f[[i]] <- rlang::exec(
      eval_fn,
      distribution = distributions[[i]], at = at, !!!fn_args
    )
  }
  if (n == 1L) {
    eval_col_names <- fn_prefix
  } else {
    ellipsis_names <- rlang::names2(ellipsis)
    dist_names <- vctrs::vec_as_names(ellipsis_names, repair = "unique")
    eval_col_names <- paste0(fn_prefix, sep, dist_names)
  }
  names(f) <- eval_col_names
  arg <- list(at)
  names(arg) <- arg_name
  res <- as.data.frame(c(arg, f))
  convert_dataframe_to_tibble(res)
}
