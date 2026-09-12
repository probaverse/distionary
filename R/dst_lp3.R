#' Log Pearson Type III distribution
#'
#' Makes a Log Pearson Type III distribution, which is the
#' distribution of the exponential of a random variable following
#' a Pearson Type III distribution.
#'
#' @param meanlog Mean of the log of the random variable; single numeric.
#' @param sdlog Standard deviation of the log of the random variable;
#' single positive numeric.
#' @param skew Skewness of the log of the random variable;
#' single numeric.
#' @returns A Log Pearson Type III distribution.
#' @examples
#' dst_lp3(0, 1, 1)
#' @export
dst_lp3 <- function(meanlog, sdlog, skew) {
  checkmate::assert_numeric(meanlog, len = 1)
  checkmate::assert_numeric(sdlog, 0, len = 1)
  checkmate::assert_numeric(skew, len = 1)
  if (is.na(meanlog) || is.na(sdlog) || is.na(skew)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(meanlog = meanlog, sdlog = sdlog, skew = skew),
    cdf = function(x) plp3(x, meanlog, sdlog, skew),
    survival = function(x) plp3(x, meanlog, sdlog, skew, lower.tail = FALSE),
    density = function(x) dlp3(x, meanlog, sdlog, skew),
    quantile = function(p) qlp3(p, meanlog, sdlog, skew),
    realise = function(n) rlp3(n, meanlog, sdlog, skew),
    .name = "Log Pearson Type III",
    # The ends of the support are the 0- and 1-quantiles, by definition, so
    # `qlp3()` is asked for them rather than the algebra being restated here.
    # This is exact, not approximate: at those two probabilities `qlp3()`
    # bottoms out in `qgamma(0) == 0` and `qgamma(1) == Inf`, so nothing is
    # searched for. Deriving it keeps the support from drifting away from the
    # quantile function, and keeps the three regimes -- positive, negative and
    # zero skew -- stated in one place instead of two.
    .support = continuous(qlp3(c(0, 1), meanlog, sdlog, skew))
  )
}
