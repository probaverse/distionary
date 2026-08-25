#' Null Distribution
#'
#' Sometimes it's convenient to work with a distribution object that is
#' akin to a missing value. This is especially true when programmatically
#' outputting distributions, such as when a distribution fails to fit to
#' data. This function makes such a distribution object. It always evaluates
#' to `NA`.
#'
#' @details
#' The Null distribution is the missing value of the distribution world, and
#' every query about it answers `NA` in whatever type that query returns:
#' `NA_real_` from [mean()] and the `eval_*()` functions, `NA_character_` from
#' [vtype()], `c(NA, NA)` from [range()], and no support at all --- [support()]
#' returns `NULL`, R's absent-object value.
#'
#' Because of that it is assembled with the package's low-level constructor
#' rather than through [distribution()]. A Null distribution cannot satisfy
#' what [distribution()] asks of a real one, since it has nothing to declare;
#' building it here keeps that bypass internal, so a distribution with no
#' support cannot be made through the front door.
#' @returns A Null distribution.
#' @examples
#' x <- dst_null()
#' mean(x)
#' eval_pmf(x, at = 1:10)
#'
#' # Everything about it is missing, including its support.
#' vtype(x)
#' range(x)
#' support(x)
#' @export
dst_null <- function() {
  new_distribution(
    list(
      cdf = function(x) {
        rep(NA_real_, length(x))
      },
      density = function(x) {
        rep(NA_real_, length(x))
      },
      hazard = function(x) {
        rep(NA_real_, length(x))
      },
      chf = function(x) {
        rep(NA_real_, length(x))
      },
      pmf = function(x) {
        rep(NA_real_, length(x))
      },
      odds = function(x) {
        rep(NA_real_, length(x))
      },
      return = function(x) {
        rep(NA_real_, length(x))
      },
      quantile = function(x) {
        rep(NA_real_, length(x))
      },
      mean = NA_real_,
      variance = NA_real_,
      skewness = NA_real_,
      kurtosis_exc = NA_real_
    ),
    vtype = NA_character_,
    name = "Null",
    parameters = NULL,
    support = NULL
  )
}
