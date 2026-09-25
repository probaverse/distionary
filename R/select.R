#' Resolve a selection of variables to their positions.
#'
#' Selections are captured unevaluated, so that, with \pkg{tidyselect}
#' installed, they can use bare names and helpers, as in
#' `marginal(d, c(runoff, everything()))`. Without it, a selection is
#' evaluated as usual and must give names or positions.
#'
#' @param distribution The distribution whose variables are selected.
#' @param quo A quosure of the selection.
#' @param arg Name of the argument, for messages.
#' @param aliases Other names referring to positions, for names that are not
#' variables: the argument names of `eval_bi_*()`.
#' @returns Integer positions, in the order selected.
#' @noRd
select_variables <- function(distribution, quo, arg, aliases = NULL) {
  if (rlang::quo_is_null(quo)) {
    return(integer(0))
  }
  if (has_tidyselect()) {
    return(tidy_select_variables(distribution, quo, arg, aliases))
  }
  reason <- paste0(
    "to select variables in `", arg, "` by bare name.\n",
    "Otherwise, give names as strings, or positions."
  )
  value <- tryCatch(
    rlang::eval_tidy(quo),
    error = function(e) {
      # Offers to install it, when interactive.
      rlang::check_installed("tidyselect", reason = reason)
      if (!has_tidyselect()) {
        stop("The tidyselect package is needed ", reason, call. = FALSE)
      }
      NULL
    }
  )
  if (has_tidyselect()) {
    return(tidy_select_variables(distribution, quo, arg, aliases))
  }
  resolve_variables(distribution, value, arg, aliases = aliases)
}

#' Selection with tidyselect.
#'
#' Its data is a stand-in list named by the variables. An alias counts only
#' when the whole selection is that one name (`given = x` or `"x"`), and no
#' variable has it.
#' @noRd
tidy_select_variables <- function(distribution, quo, arg, aliases) {
  vars <- variables(distribution)
  if (is.null(vars)) {
    vars <- "x"
  }
  expr <- rlang::quo_get_expr(quo)
  single <- if (rlang::is_symbol(expr)) {
    rlang::as_string(expr)
  } else if (rlang::is_string(expr)) {
    expr
  } else {
    NA_character_
  }
  if (!is.na(single) && !single %in% vars && single %in% aliases) {
    return(match(single, aliases))
  }
  data <- stats::setNames(as.list(seq_along(vars)), vars)
  loc <- tidyselect::eval_select(
    quo,
    data = data,
    allow_rename = FALSE,
    allow_predicates = FALSE,
    error_call = rlang::caller_env(2L)
  )
  as.integer(unname(loc))
}

#' Is tidyselect available? (A function, so that tests can pretend not.)
#' @noRd
has_tidyselect <- function() {
  rlang::is_installed("tidyselect")
}
