#' Representations of the Generalized Extreme Value Distribution
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @returns Vector of evaluated GEV distribution.
#' @examples
#' pgev(1:10, 0, 1, 1)
#' dgev(1:10, 0, 2, 0)
#' qgev(1:9 / 10, 2, 10, -2)
#' @rdname gev_raw
#' @inheritParams dst_gev
#' @export
pgev <- function(q, location, scale, shape) {
  r <- gev_range(location, scale, shape)
  t <- gev_t_function(q, location = location, scale = scale, shape = shape)
  res <- exp(-t)
  res[q <= r[1L]] <- 0
  res[q >= r[2L]] <- 1
  res
}

#' @rdname gev_raw
#' @inheritParams pgev
#' @export
qgev <- function(p, location, scale, shape) {
  invalid_at <- p < 0 | p > 1
  if (shape == 0) {
    res <- location - scale * log(-log(p))
  } else {
    res <- location + scale * ((-log(p))^(-shape) - 1) / shape
  }
  res[invalid_at] <- NaN
  res
}

#' @rdname gev_raw
#' @inheritParams pgev
#' @export
dgev <- function(x, location, scale, shape) {
  r <- gev_range(location, scale, shape)
  t <- gev_t_function(x, location = location, scale = scale, shape = shape)
  res <- t^(shape + 1) / scale * exp(-t)
  res[x <= r[1L]] <- 0
  res[x > r[2L]] <- 0
  res
}

#' 't()' function for calculating GEV quantities
#'
#' @param x Argument of the function; vectorized.
#' @inheritParams dst_gev
#' @seealso See the Wikipedia entry for the GEV distribution,
#' https://en.wikipedia.org/wiki/Generalized_extreme_value_distribution
#' @note The shape parameter is not vectorized. This function is only
#' intended to be used when location, scale, and shape are scalars.
gev_t_function <- function(x, location, scale, shape) {
  z <- (x - location) / scale
  if (shape == 0) {
    exp(-z)
  } else {
    (1 + shape * z)^(-1 / shape)
  }
}

#' Range of a GEV distribution
#'
#' @inheritParams dst_gev
#' @returns A vector of length 2 indicating the support of the distribution.
gev_range <- function(location, scale, shape) {
  if (shape > 0) {
    c(location - scale / shape, Inf)
  } else if (shape == 0) {
    c(-Inf, Inf)
  } else if (shape < 0) {
    c(-Inf, location - scale / shape)
  }
}
