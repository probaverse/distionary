#' Representations of the Pearson Type III Distribution
#'
#' @param x Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Single positive whole number; number of observations to draw
#' from the distribution.
#' @param location Parameter representing the left-endpoint of the distribution;
#' numeric. Vectors are allowed except for `rpearson3()`.
#' @param scale Scale parameter; positive numeric.
#' Vectors are allowed except for `rpearson3()`.
#' @param shape Shape parameter; positive numeric.
#' Vectors are allowed except for `rpearson3()`.
#' @param log Logical; if `TRUE`, probabilities are given as log-probabilities.
#' @param lower.tail Logical; if `TRUE` (default), probabilities are
#' `P(X <= x)`, otherwise, `P(X > x)`.
#' @returns Vector of evaluated Pearson Type III distribution, with length
#' equal to the recycled lengths of `x`/`p`, `location`, `scale`, and
#' `shape`. For `rpearson3()`, a vector of length `n`.
#' @examples
#' ppearson3(1:10, location = 0, scale = 1, shape = 1)
#' dpearson3(1:10, location = 1:10, scale = 2, shape = 0)
#' qpearson3(1:9 / 10, location = -2, scale = 10, shape = 2)
#' set.seed(1)
#' rpearson3(10, location = 2, scale = 10, shape = 2)
#' @rdname pearson3_raw
#' @export
ppearson3 <- function(x, location, scale, shape, lower.tail = TRUE) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(location)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape, 0)
  checkmate::assert_logical(lower.tail, len = 1, any.missing = FALSE)
  l <- vctrs::vec_recycle_common(x, location, scale, shape)
  x <- l[[1]]
  location <- l[[2]]
  scale <- l[[3]]
  shape <- l[[4]]
  stats::pgamma(
    x - location,
    scale = scale,
    shape = shape,
    lower.tail = lower.tail
  )
}

#' @rdname pearson3_raw
#' @export
dpearson3 <- function(x, location, scale, shape, log = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(location)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape, 0)
  checkmate::assert_logical(log, len = 1, any.missing = FALSE)
  l <- vctrs::vec_recycle_common(x, location, scale, shape)
  x <- l[[1]]
  location <- l[[2]]
  scale <- l[[3]]
  shape <- l[[4]]
  stats::dgamma(x - location, scale = scale, shape = shape, log = log)
}

#' @rdname pearson3_raw
#' @export
qpearson3 <- function(p, location, scale, shape) {
  checkmate::assert_numeric(p)
  checkmate::assert_numeric(location)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape, 0)
  l <- vctrs::vec_recycle_common(p, location, scale, shape)
  p <- l[[1]]
  location <- l[[2]]
  scale <- l[[3]]
  shape <- l[[4]]
  location + stats::qgamma(p, shape = shape, scale = scale)
}

#' @rdname pearson3_raw
#' @export
rpearson3 <- function(n, location, scale, shape) {
  checkmate::assert_integerish(n, lower = 0, len = 1)
  checkmate::assert_numeric(location, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  checkmate::assert_numeric(shape, 0, len = 1)
  location + stats::rgamma(n, shape = shape, scale = scale)
}
