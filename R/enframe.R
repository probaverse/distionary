#' Enframe a distributional representation's evaluation
#'
#' These are the workhorses for the `enframe_` family of functions, evaluating
#' a specified distributional representation for univariate, bivariate, and
#' multivariate distributions, for multiple distributions,
#' and places the results in a data frame or tibble.
#'
#' @inheritParams eval_multicdf
#' @param eval_fn Name of the `eval_` function for the desired distributional
#' representation, such as `eval_cdf` and `eval_density`.
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
enframe_general <- function(..., .l, arg_name, fn_prefix, sep,
                            eval_fn, fn_args = list()) {
  output_cols <- process_ellipsis_for_enframe(...)
  names(output_cols) <- enframed_names_output(..., fn_prefix = fn_prefix,
                                              sep = sep)
  input_cols <- vec_recycle_common_and_rename(!!!.l)
  names(.l) <- NULL
  # .l_enquo <- rlang::quos_auto_name(rlang::enquos(!!!.l))
  # .l_names <- rlang::names2(.l_enquo)
  # input_cols <- lapply(.l_enquo, rlang::eval_tidy)
  # names(input_cols) <- enframed_names_input(.l_names, arg_name)
  n_out <- length(output_cols)
  n_in <- length(input_cols)
  if (n_in > 2) {
    # Not totally robust if open to users: could have n_in == 2
    # yet eval_multi invoked through eval_fn.
    .l <- list(.l)
  }
  for (i in seq_len(n_out)) if (is_distribution(output_cols[[i]])) {
    output_cols[[i]] <- rlang::exec(
      eval_fn, distribution = output_cols[[i]], !!!.l, !!!fn_args
    )
  }
  res <- as.data.frame(c(input_cols, output_cols))
  convert_dataframe_to_tibble(res)
}

#' Process ellipsis for enframe_ functions
#'
#' Takes distributions input in the ellipsis of an enframe_ function,
#' checks them, and returns a list of those distributions, with NA where
#' there are empty (NULL) distributions.
#' @param ... Arguments to be processed.
#' @return A list of distributions input in `...`, with `NA_real_` wherever
#' there is an `NA` or `NULL` entry.
process_ellipsis_for_enframe <- function(...) {
  ellipsis <- rlang::quos(...)
  distributions <- lapply(ellipsis, rlang::eval_tidy)
  n <- length(distributions)
  if (n == 0L) {
    stop("Need at least one distribution in the `enframe_*()` function.")
  }
  is_distributions <- vapply(distributions, is_distribution,
                             FUN.VALUE = logical(1L))
  is_null <- vapply(distributions, function(d) {
    !is_distribution(d) && (is.na(d) || is.null(d))
  }, FUN.VALUE = logical(1L))
  valid_entries <- is_distributions | is_null
  if (any(!valid_entries)) {
    stop("`enframe_*()` functions only accept distributions. ",
         "Entries that are not distributions: ",
         paste(which(!valid_entries), collapse = ", "))
  }
  distributions[!valid_entries] <- NA_real_
  distributions
}

#' Column names for enframe functions
#'
#' Derive names for the columns in the enframe_ family of functions;
#' "input" being the function argument being evaluated, and
#' "output" being the evaluated function.
#'
#' @param ... Arguments contained in the ellipsis of an enframe_ function.
#' @param input_names A character vector of  input vectors,
#' on which the distributional
#' representation is to eventually be evaluated at.
#' @rdname enframed_names
#' @inheritParams enframe_univariate
enframed_names_output <- function(..., fn_prefix, sep) {
  ellipsis <- rlang::quos(...)
  n <- length(ellipsis)
  if (n == 1L) {
    output_names <- fn_prefix
  } else {
    ellipsis_names <- rlang::names2(rlang::quos_auto_name(ellipsis))
    dist_names <- vctrs::vec_as_names(ellipsis_names, repair = "unique")
    output_names <- paste0(fn_prefix, sep, dist_names)
  }
  output_names
}

#' @rdname enframed_names
enframed_names_input <- function(input_names, arg_name) {
  n <- length(input_names)
  if (!is.null(arg_name) && length(arg_name) != n) {
    stop("Specified ", length(arg_name), " arguments in `arg_name`, yet there ",
         "are/is ", n, " dimensions of input distribution(s).")
  }
  if (!is.null(arg_name)) {
    input_names <- arg_name
  }
  input_names
}


#' vec_recycle_common and fill in missing names
#'
#' Recycles input vectors to a common length, as in
#' `vctrs::vec_recycle_common()`, but names that were originally unnamed
#' are given names: when an input is a symbol, that symbol becomes the
#' component name; otherwise, the name is `...i`, where `i` is the entry
#' number.
#' @param ... Vectors to recycle.
#' @return A named list, not necessarily with unique names.
#' @examples
#' a <- 1:3
#' distionary:::vec_recycle_common_and_rename(a, b = 2, 32 + 5, f = a, f = 4)
#' distionary:::vec_recycle_common_and_rename(!!!list(a, b = 2, 32 + 5))
vec_recycle_common_and_rename <- function(...) {
  ellipsis <- rlang::enexprs(...)
  .l <- vctrs::vec_recycle_common(...)
  need_names <- !rlang::have_name(ellipsis)
  if (any(need_names)) {
    n_need_names <- sum(need_names)
    names(.l)[need_names] <- paste0("...", 1:length(.l))[need_names]
    are_symbols <- vapply(ellipsis, rlang::is_symbol, FUN.VALUE = logical(1L))
    names(.l)[are_symbols & need_names] <- vapply(
      ellipsis[are_symbols & need_names], rlang::as_string,
      FUN.VALUE = character(1L)
    )
  }
  .l
}


#' #' @rdname enframe_dvariate
#' enframe_univariate <- function(..., at, arg_name, fn_prefix = , sep,
#'                                eval_fn, fn_args = list()) {
#'   output_cols <- process_ellipsis_for_enframe(...)
#'   names(output_cols) <- enframed_names_output(..., fn_prefix, sep)
#'   at_enquo <- rlang::quos_auto_name(rlang::enquos(at))
#'   at_names <- rlang::names2(at_enquo)
#'   input_cols <- lapply(at_enquo, rlang::eval_tidy)
#'   names(input_cols) <- enframed_names_input(at_names, arg_name)
#'   n <- length(output_cols)
#'   for (i in seq_len(n)) if (!is.na(output_cols[[i]])) {
#'     output_cols[[i]] <- rlang::exec(
#'       eval_fn, distribution = output_cols[[i]], at = at, !!!fn_args
#'     )
#'   }
#'   res <- as.data.frame(c(input_cols, output_cols))
#'   convert_dataframe_to_tibble(res)
#' }
#'
#' #' @rdname enframe_dvariate
#' enframe_bivariate <- function(..., x, y, arg_name, fn_prefix, sep,
#'                               eval_fn, fn_args = list()) {
#'   output_cols <- process_ellipsis_for_enframe(...)
#'   names(output_cols) <- enframed_names_output(..., fn_prefix, sep)
#'   xy_enquo <- rlang::quos_auto_name(rlang::enquos(x, y))
#'   xy_names <- rlang::names2(xy_enquo)
#'   input_cols <- lapply(xy_enquo, rlang::eval_tidy)
#'   names(input_cols) <- enframed_names_input(xy_names, arg_name)
#'   n <- length(output_cols)
#'   for (i in seq_len(n)) if (!is.na(output_cols[[i]])) {
#'     output_cols[[i]] <- rlang::exec(
#'       eval_fn, distribution = output_cols[[i]], x = x, y = y, !!!fn_args
#'     )
#'   }
#'   res <- as.data.frame(c(input_cols, output_cols))
#'   convert_dataframe_to_tibble(res)
#' }
#'
#' #' @rdname enframe_dvariate
#' enframe_multivariate <- function(..., .l, arg_name, fn_prefix, sep,
#'                                  eval_fn, fn_args = list()) {
#'   output_cols <- process_ellipsis_for_enframe(...)
#'   names(output_cols) <- enframed_names_output(..., fn_prefix, sep)
#'   .l_enquo <- rlang::quos_auto_name(rlang::enquos(!!!.l))
#'   .l_names <- rlang::names2(.l_enquo)
#'   input_cols <- lapply(.l_enquo, rlang::eval_tidy)
#'   names(input_cols) <- enframed_names_input(.l_names, arg_name)
#'   n <- length(output_cols)
#'   for (i in seq_len(n)) if (!is.na(output_cols[[i]])) {
#'     output_cols[[i]] <- rlang::exec(
#'       eval_fn, distribution = output_cols[[i]], .l = .l, !!!fn_args
#'     )
#'   }
#'   res <- as.data.frame(c(input_cols, output_cols))
#'   convert_dataframe_to_tibble(res)
#' }
