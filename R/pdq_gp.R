#' Representations of the Generalized Pareto Distribution
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param scale Vector of scale parameters; positive numeric.
#' @param shape Vector of shape parameters; positive numeric.
#' @param lower.tail Single logical. If `TRUE`, cdf (default);
#' if `FALSE`, survival function.
#' @returns Vector of evaluated GP distribution, with length
#' equal to the recycled lengths of `q`/`x`/`p`, `scale`, and `shape`.
#' @examples
#' pgp(1:10, 1, 1)
#' dgp(1:10, 2, 0)
#' qgp(1:9 / 10, 10, -2)
#' @rdname gp_raw
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.14} distionary is designed to propagate NA as if it's just
#' another data type. See `eval_*()` functions by way of example.
#' @srrstats {G2.14b} NA inputs are "ignored" in the sense that they are not
#' treated as special, but rather just another type of data, and therefore
#' does not need to alert the user of their presence.
#' @export
pgp <- function(q, scale, shape, lower.tail = TRUE) {
  checkmate::assert_numeric(q)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  checkmate::assert_logical(lower.tail, len = 1, any.missing = FALSE)
  l <- vctrs::vec_recycle_common(q, scale, shape)
  q <- l[[1]]
  scale <- l[[2]]
  shape <- l[[3]]
  hi <- gp_upper(scale = scale, shape = shape)
  left <- q < 0
  right <- q > hi
  t <- gev_t_function(q, location = 0, scale = scale, shape = shape)
  if (lower.tail) {
    res <- 1 - t
    res[left] <- 0
    res[right] <- 1
  } else {
    res <- t
    res[left] <- 1
    res[right] <- 0
  }
  res
}


#' @rdname gp_raw
#' @export
qgp <- function(p, scale, shape) {
  # z = (RP^xi - 1) / xi -> log(RP)
  checkmate::assert_numeric(p)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  l <- vctrs::vec_recycle_common(p, scale, shape)
  p <- l[[1]]
  scale <- l[[2]]
  shape <- l[[3]]
  rp <- 1 / (1 - p)
  res <- ifelse(
    p >= 0 & p <= 1,
    ifelse(shape == 0, log(rp), (rp^shape - 1) / shape),
    NaN
  )
  scale * res
}

#' @rdname gp_raw
#' @export
dgp <- function(x, scale, shape) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  l <- vctrs::vec_recycle_common(x, scale, shape)
  x <- l[[1]]
  scale <- l[[2]]
  shape <- l[[3]]
  z <- x / scale
  hi <- gp_upper(scale = scale, shape = shape)
  t <- gev_t_function(x, location = 0, scale = scale, shape = shape)
  res <- ifelse(shape == 0, t, (1 + shape * z)^(-1 / shape - 1))
  # i_zero <- shape == 0
  # i_non0 <- shape != 0
  # res <- numeric(length(x))
  # res[i_zero] <- t[i_zero]
  # res[i_non0] <- (1 + shape[i_non0] * z[i_non0])^(-1 / shape[i_non0] - 1)
  res[x < 0 | x > hi] <- 0
  res / scale
}


#' Upper endpoint of GP
#'
#' @inheritParams dst_gp
#' @returns A vector of maxima corresponding to the input parameters.
#' @noRd
gp_upper <- function(scale, shape) {
  l <- vctrs::vec_recycle_common(scale, shape)
  scale <- l[[1]]
  shape <- l[[2]]
  res <- shape # For NA propagation
  ifelse(shape < 0, -scale / shape, Inf)
  # i_calc <- shape < 0
  # res[i_calc] <- -scale[i_calc] / shape[i_calc]
  # res[shape >= 0] <- Inf
  # res
}
