#' Representations of the Log Pearson Type III Distribution
#'
#' Representations of the Log Pearson Type III (LP3) Distribution
#'
#' @param x Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Single positive whole number; number of observations to draw
#' from the distribution.
#' @param meanlog Parameter representing the mean of the random variable
#' in log (base e) space; numeric. Vectors are allowed except for `rlp3()`.
#' @param sdlog Parameter representing the standard deviation of the
#' random variable in log (base e) space; positive numeric.
#' Vectors are allowed except for `rlp3()`.
#' @param skew Parameter representing the skewness of the random variable
#' in log (base e) space; numeric.
#' Vectors are allowed except for `rlp3()`.
#' @param lower.tail Logical; if `TRUE` (default), probabilities are
#' `P(X <= x)`, otherwise, `P(X > x)`.
#' @returns Vector of evaluated LP3 distribution, with length
#' equal to the recycled lengths of `x`/`p`, `meanlog`, `sdlog`, and
#' `skew`. For `rlp3()`, a vector of length `n`.
#' @examples
#' plp3(1:10, meanlog = 0, sdlog = 1, skew = 1)
#' dlp3(1:10, meanlog = 1:10, sdlog = 2, skew = 0)
#' qlp3(1:9 / 10, meanlog = 2, sdlog = 10, skew = 2)
#' set.seed(1)
#' rlp3(10, meanlog = 2, sdlog = 10, skew = 2)
#' @rdname lp3_raw
#' @export
plp3 <- function(x, meanlog, sdlog, skew, lower.tail = TRUE) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(meanlog)
  checkmate::assert_numeric(sdlog, 0)
  checkmate::assert_numeric(skew)
  checkmate::assert_logical(lower.tail, len = 1, any.missing = FALSE)
  l <- vctrs::vec_recycle_common(x, meanlog, sdlog, skew)
  x <- l[[1]]
  meanlog <- l[[2]]
  sdlog <- l[[3]]
  skew <- l[[4]]
  t <- .lp3_logscale(meanlog, sdlog, skew, log(pmax(0, x)))
  lower <- skew >= 0
  pgamma_lower <- if (lower.tail) lower else !lower
  ifelse(
    t$lnorm,
    stats::pnorm(t$y, mean = 0, sd = sdlog, lower.tail = lower.tail),
    stats::pgamma(
      t$y,
      shape = t$shape,
      scale = t$scale,
      lower.tail = pgamma_lower
    )
  )
}

#' @rdname lp3_raw
#' @export
dlp3 <- function(x, meanlog, sdlog, skew) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(meanlog)
  checkmate::assert_numeric(sdlog, 0)
  checkmate::assert_numeric(skew)
  l <- vctrs::vec_recycle_common(x, meanlog, sdlog, skew)
  x <- l[[1]]
  meanlog <- l[[2]]
  sdlog <- l[[3]]
  skew <- l[[4]]
  t <- .lp3_logscale(meanlog, sdlog, skew, log(pmax(0, x)))
  res <- ifelse(
    t$lnorm,
    stats::dnorm(t$y, mean = 0, sd = sdlog) / x,
    stats::dgamma(t$y, shape = t$shape, scale = t$scale) / x
  )
  res[x == 0] <- 0
  res
}

#' @rdname lp3_raw
#' @export
qlp3 <- function(p, meanlog, sdlog, skew) {
  checkmate::assert_numeric(p)
  checkmate::assert_numeric(meanlog)
  checkmate::assert_numeric(sdlog, 0)
  checkmate::assert_numeric(skew)
  l <- vctrs::vec_recycle_common(p, meanlog, sdlog, skew)
  p <- l[[1]]
  meanlog <- l[[2]]
  sdlog <- l[[3]]
  skew <- l[[4]]
  lnorm <- skew == 0
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  shift_lower <- meanlog - scale * shape
  shift_upper <- meanlog + scale * shape
  ifelse(
    lnorm,
    exp(stats::qnorm(p, meanlog, sdlog)),
    ifelse(
      skew > 0,
      exp(stats::qgamma(p, shape = shape, scale = scale) + shift_lower),
      exp(shift_upper - stats::qgamma(1 - p, shape = shape, scale = scale))
    )
  )
}

#' @rdname lp3_raw
#' @export
rlp3 <- function(n, meanlog, sdlog, skew) {
  checkmate::assert_integerish(n, lower = 0, len = 1)
  checkmate::assert_numeric(meanlog, len = 1)
  checkmate::assert_numeric(sdlog, 0, len = 1)
  checkmate::assert_numeric(skew, len = 1)
  if (skew == 0) {
    return(exp(stats::rnorm(n, meanlog, sdlog)))
  }
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  if (skew > 0) {
    shift <- meanlog - scale * shape
    exp(stats::rgamma(n, shape = shape, scale = scale) + shift)
  } else {
    shift <- meanlog + scale * shape
    exp(shift - stats::rgamma(n, shape = shape, scale = scale))
  }
}

.lp3_logscale <- function(meanlog, sdlog, skew, logx) {
  lnorm <- skew == 0
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  shift_lower <- meanlog - scale * shape
  shift_upper <- meanlog + scale * shape
  lower <- skew >= 0
  y <- ifelse(
    lnorm,
    logx - meanlog,
    ifelse(lower, logx - shift_lower, shift_upper - logx)
  )
  list(
    lnorm = lnorm,
    shape = shape,
    scale = scale,
    y = y
  )
}
