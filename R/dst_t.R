#' Student t Distribution
#'
#' Makes a Student t distribution, optionally shifted and scaled.
#'
#' @param df Degrees of freedom; single positive numeric.
#' @param ... Not used; forces `location` and `scale` to be named.
#' @param location,scale The distribution is that of `location + scale * T`,
#' where `T` has the standard t distribution with `df` degrees of freedom.
#' `scale` is positive. They default to the standard t.
#' @details
#' The `location` is the mean when `df > 1`, and the `scale` is not the
#' standard deviation: the variance is `scale^2 * df / (df - 2)`, for
#' `df > 2`.
#'
#' The standard t's parameters are its degrees of freedom alone, so
#' [parameters()] lists `location` and `scale` only when they are not 0 and
#' 1.
#' @returns A Student t distribution.
#' @seealso [dst_mv_t()] for several variables.
#' @examples
#' dst_t(3)
#' dst_t(3, location = 10, scale = 2)
#' @export
dst_t <- function(df, ..., location = 0, scale = 1) {
  rlang::check_dots_empty()
  checkmate::assert_numeric(df, 0, len = 1)
  checkmate::assert_numeric(location, len = 1)
  checkmate::assert_numeric(scale, len = 1)
  if (is.na(df) || is.na(location) || is.na(scale)) {
    return(dst_null())
  }
  if (scale <= 0) {
    stop("`scale` must be positive.")
  }
  standard <- location == 0 && scale == 1
  params <- if (standard) {
    list(df = df)
  } else {
    list(df = df, location = location, scale = scale)
  }
  distribution(
    .parameters = params,
    density = function(x) {
      stats::dt((x - location) / scale, df = df) / scale
    },
    cdf = function(x) {
      stats::pt((x - location) / scale, df = df)
    },
    quantile = function(p) {
      location + scale * stats::qt(p, df = df)
    },
    realise = function(n) {
      location + scale * stats::rt(n, df = df)
    },
    survival = function(x) {
      stats::pt((x - location) / scale, df = df, lower.tail = FALSE)
    },
    mean = ifelse(df > 1, location, NaN),
    median = location,
    variance = {
      if (df > 2) {
        scale^2 * df / (df - 2)
      } else if (df > 1) {
        Inf
      } else {
        NaN
      }
    },
    skewness = ifelse(df > 3, 0, NaN),
    kurtosis_exc = {
      if (df > 4) {
        6 / (df - 4)
      } else if (df > 2) {
        Inf
      } else {
        NaN
      }
    },
    .name = "Student t",
    .support = continuous(c(-Inf, Inf)),
  )
}
