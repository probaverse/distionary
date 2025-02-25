#' Representations of the Generalized Pareto Distribution
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param scale Vector of scale parameters; positive numeric.
#' @param shape Vector of shape parameters; positive numeric.
#' @param lower.tail Single logical. If `TRUE`, cdf (default);
#' if `FALSE`, survival function.
#' @returns Vector of evaluated GPD distribution, with length
#' equal to the recycled lengths of `q`/`x`/`p`, `scale`, and `shape`.
#' @examples
#' pgpd(1:10, 1, 1)
#' dgpd(1:10, 2, 0)
#' qgpd(1:9 / 10, 10, -2)
#' @rdname gpd_raw
#' @export
pgpd <- function(q, scale, shape, lower.tail = TRUE) {
  checkmate::assert_numeric(q)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  checkmate::assert_logical(lower.tail, len = 1, any.missing = FALSE)
  l <- vctrs::vec_recycle_common(q, scale, shape)
  q <- l[[1]]
  scale <- l[[2]]
  shape <- l[[3]]
  hi <- gpd_upper(scale = scale, shape = shape)
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


#' @rdname gpd_raw
#' @export
qgpd <- function(p, scale, shape) {
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

#' @rdname gpd_raw
#' @export
dgpd <- function(x, scale, shape) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(scale, 0)
  checkmate::assert_numeric(shape)
  l <- vctrs::vec_recycle_common(x, scale, shape)
  x <- l[[1]]
  scale <- l[[2]]
  shape <- l[[3]]
  z <- x / scale
  hi <- gpd_upper(scale = scale, shape = shape)
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


#' Upper endpoint of GPD
#'
#' @inheritParams dst_gpd
#' @returns A vector of maxima corresponding to the input parameters.
#' @noRd
gpd_upper <- function(scale, shape) {
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
