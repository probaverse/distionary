#' Representations of the Generalized Extreme Value Distribution
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param location Location parameter; numeric vector.
#' @param scale Scale parameter; positive numeric vector.
#' @param shape Shape parameter; numeric vector.
#' This is also the extreme value index,
#' so that `shape > 0` is heavy tailed, and `shape < 0` is short-tailed.
#' @returns Vector of evaluated GEV distribution, with length
#' equal to the recycled lengths of `q`/`x`/`p`, `location`, `scale`, and
#' `shape`.
#' @examples
#' pgev(1:10, 0, 1, 1)
#' dgev(1:10, 1:10, 2, 0)
#' qgev(1:9 / 10, 2, 10, -2)
#' @rdname gev_raw
#' @export
pgev <- function(q, location, scale, shape) {
  checkmate::assert_numeric(q)
  checkmate::assert_numeric(location)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  l <- vctrs::vec_recycle_common(q, location, scale, shape)
  q <- l[[1]]
  location <- l[[2]]
  scale <- l[[3]]
  shape <- l[[4]]
  lo <- gev_lower(location, scale, shape)
  hi <- gev_upper(location, scale, shape)
  t <- gev_t_function(q, location = location, scale = scale, shape = shape)
  res <- exp(-t)
  res[q <= lo] <- 0
  res[q >= hi] <- 1
  res
}

#' @rdname gev_raw
#' @export
qgev <- function(p, location, scale, shape) {
  checkmate::assert_numeric(p)
  checkmate::assert_numeric(location)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  l <- vctrs::vec_recycle_common(p, location, scale, shape)
  p <- l[[1]]
  location <- l[[2]]
  scale <- l[[3]]
  shape <- l[[4]]
  neglogp <- -log(p)
  res <- ifelse(
    p >= 0 & p <= 1,
    ifelse(shape == 0, -log(neglogp), (neglogp^(-shape) - 1) / shape),
    NaN
  )
  location + scale * res
}

#' @rdname gev_raw
#' @export
dgev <- function(x, location, scale, shape) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(location)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  l <- vctrs::vec_recycle_common(x, location, scale, shape)
  x <- l[[1]]
  location <- l[[2]]
  scale <- l[[3]]
  shape <- l[[4]]
  lo <- gev_lower(location, scale, shape)
  hi <- gev_upper(location, scale, shape)
  t <- gev_t_function(x, location = location, scale = scale, shape = shape)
  res <- t^(shape + 1) / scale * exp(-t)
  ifelse(x <= lo | x > hi, 0, res)
  # res[x <= lo] <- 0
  # res[x > hi] <- 0
  # res
}

#' 't()' function for calculating GEV quantities
#'
#' Decides whether to use `exp()` when the shape parameter is 0,
#' or the non-limiting form otherwise. Also useful for the GPD.
#'
#' @param x Argument of the function; vectorized.
#' @returns A vector of the t function evaluated.
#' @inheritParams pgev
#' @seealso See the Wikipedia entry for the GEV distribution,
#' https://en.wikipedia.org/wiki/Generalized_extreme_value_distribution
#' @note The shape parameter is not vectorized. This function is only
#' intended to be used when location, scale, and shape are scalars.
#' @noRd
gev_t_function <- function(x, location, scale, shape) {
  l <- vctrs::vec_recycle_common(x, location, scale, shape)
  x <- l[[1]]
  location <- l[[2]]
  scale <- l[[3]]
  shape <- l[[4]]
  z <- (x - location) / scale
  # res <- numeric(length(x))
  # i_zero <- shape == 0
  # i_non0 <- shape != 0
  ifelse(shape == 0, exp(-z), (1 + shape * z)^(-1 / shape))
}

#' Range of a GEV distribution
#'
#' @inheritParams pgev
#' @returns A vector of mimima for `gev_lower()`, or maxima
#' for `gev_upper()`, corresponding to the input parameters.
#' @noRd
gev_lower <- function(location, scale, shape) {
  l <- vctrs::vec_recycle_common(location, scale, shape)
  location <- l[[1]]
  scale <- l[[2]]
  shape <- l[[3]]
  ifelse(shape > 0, location - scale / shape, -Inf)
  # res <- shape # For NA propagation
  # i_calc <- shape > 0
  # res[i_calc] <- location[i_calc] - scale[i_calc] / shape[i_calc]
  # res[shape <= 0] <- -Inf
  # res
}

#' @inheritParams gev_lower
#' @noRd
gev_upper <- function(location, scale, shape) {
  l <- vctrs::vec_recycle_common(location, scale, shape)
  location <- l[[1]]
  scale <- l[[2]]
  shape <- l[[3]]
  ifelse(shape < 0, location - scale / shape, Inf)
  # res <- shape # For NA propagation
  # i_calc <- shape < 0
  # res[i_calc] <- location[i_calc] - scale[i_calc] / shape[i_calc]
  # res[shape >= 0] <- Inf
  # res
}
