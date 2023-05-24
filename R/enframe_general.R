#' Enframe a distributional representation's evaluation
#'
#' These are the workhorses for the `enframe_` family of functions, evaluating
#' a specified distributional representation for univariate, bivariate, and
#' multivariate distributions, for multiple distributions,
#' and places the results in a data frame or tibble.
#'
#' @inheritParams enframe_uni
#' @param fn_args A named list of arguments to pass to the `eval_fn` function,
#' besides the distribution and `.l` argument (the `strict` argument
#' being the most common, and perhaps the only use case).
#' @return A data frame or tibble of the input argument(s) (`.l`), with the
#' evaluated distributional representation for each distribution in
#' `...` in its own column.
#' @details If only one distribution is specified in `...`, then the evaluation
#' column will be named that of `fn_prefix`.
#'
#' If more than one distribution
#' is specified in `...`, the evaluation columns will be named by the
#' prefix `fn_prefix` followed by the distribution names.
#'
#' Distributions are named first by their argument names, if given, or if not,
#' the input text. Names are then made unique using `vctrs::vec_as_names()`
#' with the "unique" names repair. "Unique" is chosen instead of "universal"
#' because names are anticipated to be syntactic with the `eval_fn` prefix;
#' "minimal" is not sufficient because it may result in columns having the
#' same names.
#' @rdname enframe_general
enframe_general <- function(..., .l, fn_prefix, eval_fn, fn_args = list()) {
  input_cols <- .l
  output_cols <- dots_to_dsts(...)
  check_dsts(output_cols)
  names(output_cols) <- name_distributions(..., fn_prefix = fn_prefix)
  n <- length(.l[[1]])
  d <- length(output_cols)
  for (i in seq_len(d)) {
    if (is_distribution(output_cols[[i]])) {
      output_cols[[i]] <- rlang::exec(
        eval_fn, distribution = output_cols[[i]], .l = .l, !!!fn_args
      )
    } else {
      output_cols[[i]] <- rep(NA_real_, n)
    }
  }
  res <- as.data.frame(c(input_cols, output_cols))
  convert_dataframe_to_tibble(res)
}

#' Column names for enframe functions
#'
#' Derive names for the columns in the enframe_ family of functions;
#' "input" being the function argument being evaluated, and
#' "output" being the evaluated function.
#'
#' @rdname enframed_names
#' @inheritParams enframe_cdf
name_distributions <- function(..., fn_prefix) {
  ellipsis <- dots_to_quos(...)
  n <- length(ellipsis)
  if (n == 1L) {
    output_names <- fn_prefix
  } else {
    ellipsis_names <- rlang::names2(rlang::quos_auto_name(ellipsis))
    dist_names <- vctrs::vec_as_names(ellipsis_names, repair = "unique")
    output_names <- paste0(fn_prefix, "_", dist_names)
  }
  output_names
}


