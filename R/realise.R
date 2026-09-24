#' Generate a Sample from a Distribution
#'
#' Draw `n` independent observations from a distribution.
#'
#' @param distribution Distribution object.
#' @param n Number of observations to generate.
#' @returns Vector of independent values drawn from the inputted distribution.
#' For a multivariate distribution, a data frame with one column per variable
#' and one row per draw (a tibble, if \pkg{tibble} is installed). It can be
#' passed as `l` to the `eval_mv_*()` functions.
#' @note `realise()` and `realize()` are aliases and do the same thing.
#' @rdname realise
#' @examples
#' d <- dst_pois(5)
#' set.seed(2)
#' realise(d, n = 10)
#' @export
realise <- function(distribution, n = 1) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_integerish(n, lower = 0, len = 1)
  draws <- eval_property(distribution, "realise", n)
  if (is_multivariate(distribution)) {
    draws <- as_draws_frame(draws, variables(distribution))
  }
  draws
}

#' @rdname realise
#' @export
realize <- function(distribution, n = 1) {
  realise(distribution, n = n)
}

#' Tidy multivariate draws into a data frame named by the variables.
#'
#' A stated `realise` may hand back a matrix, a list, or a data frame; the
#' user gets the same thing whichever it was.
#' @noRd
as_draws_frame <- function(draws, vars) {
  draws <- as.data.frame(draws)
  if (ncol(draws) != length(vars)) {
    stop(
      "The distribution's `realise` gave ", ncol(draws), " columns,\n",
      "but the distribution has ", length(vars), " variables."
    )
  }
  names(draws) <- vars
  rownames(draws) <- NULL
  convert_dataframe_to_tibble(draws)
}
