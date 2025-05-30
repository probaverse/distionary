#' Enframe a distributional representation
#'
#' This is the workhorse for the `enframe_` family of functions.
#' `enframe_general()` evaluates a specified distributional representation
#' for multiple distributions, and places the results in a data frame
#' or tibble.
#'
#' @inheritParams eval_cdf
#' @param ... Distributions to enframe; optionally, named.
#' @param eval_fn The `eval_` function for the desired distributional
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
#' @srrstats {G2.3} Univariate character input specifications are asserted
#' using the checkmate package where relevant (e.g., `.vtype` and `.name`
#' in `distribution()`; `arg_name` and `fn_prefix` in `enframe_*()`).
#' --> Copied to those functions.
#' @srrstats {G2.4c} Explicit conversion to character via `as.character()`
#' (and not `paste` or `paste0`) is done where character input is required:
#' `distribution()`'s `.vtype` and `.name` arguments, and the column naming
#' specifications of `enframe_general()`. --> Copied to both functions.
#' @srrstats {PD3.1} Operations on probability distributions are
#' contained within separate functions which themselves accept the
#' names of the distributions as one input parameter. Examples include
#' the `eval_()` and `enframe_()` families of functions.
#' @noRd
enframe_general <- function(...,
                            at,
                            arg_name,
                            fn_prefix,
                            sep,
                            eval_fn,
                            fn_args = list()) {
  arg_name <- as.character(arg_name)
  fn_prefix <- as.character(fn_prefix)
  sep <- as.character(sep)
  checkmate::assert_numeric(at)
  checkmate::assert_character(arg_name, len = 1)
  checkmate::assert_character(fn_prefix, len = 1)
  checkmate::assert_character(sep, len = 1)
  checkmate::assert_function(eval_fn)
  checkmate::assert_list(fn_args)
  ellipsis <- rlang::quos(...)
  ellipsis <- rlang::quos_auto_name(ellipsis)
  distributions <- lapply(ellipsis, rlang::eval_tidy)
  checkmate::assert_list(distributions, types = "dst")
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
