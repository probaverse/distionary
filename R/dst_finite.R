#' Finite Distribution
#'
#' Makes a finite distribution, which is a distribution with a finite number
#' of possible outcomes.
#'
#' @param outcomes Numeric vector representing the potential
#' outcomes of the distribution.
#' @param probs Numeric vector of probabilities corresponding to the outcomes
#' in `outcomes`. Must not be negative and must sum to 1.
#' @returns A distribution with finite outcomes.
#' @seealso [dst_empirical()]
#' @examples
#' dst_finite(2:5, probs = 1:4 / 10)
#' @export
dst_finite <- function(outcomes, probs) {
  if (any(is.na(outcomes)) || any(is.na(probs))) {
    return(dst_null())
  }
  if (length(outcomes) != length(probs)) {
    stop("Inputs `outcomes` and `probs` should have the same length.")
  }
  if (length(outcomes) == 0) {
    warning(
      "Can't make a finite distribution from empty data. ",
      "Returning a Null distribution."
    )
    return(dst_null())
  }
  mask <- probs != 0
  outcomes <- outcomes[mask]
  probs <- probs[mask]
  if (any(probs < 0)) {
    stop("Probabilities must be between 0 and 1.")
  }
  if (abs(sum(probs) - 1) > .Machine$double.eps * length(probs)) {
    stop("Probabilities must add up to 1.")
  }
  if (length(outcomes) == 1L) {
    return(dst_degenerate(outcomes))
  }
  ## Helpful Quantities
  mu <- sum(probs * outcomes)
  ss <- sum(probs * (outcomes - mu)^2)
  sigma <- sqrt(ss)
  ## Sort outcomes and probabilities for the stepfuns
  order <- order(outcomes)
  outcomes <- outcomes[order]
  probs <- probs[order]
  cumsum_p <- cumsum(probs)
  heights <- c(0, cumsum_p)
  taus <- cumsum_p[-length(cumsum_p)]
  distribution(
    .parameters = list(outcomes = outcomes, probs = probs),
    pmf = function(q) {
      na_mask <- is.na(q)
      matched <- match(q, outcomes)
      res <- probs[matched]
      res[is.na(matched)] <- 0
      res[na_mask] <- NA_real_
      res
    },
    cdf = stats::stepfun(outcomes, heights, right = FALSE),
    quantile = stats::stepfun(taus, outcomes, right = TRUE),
    realise = \(n) sample(outcomes, size = n, replace = TRUE, prob = probs),
    survival = stats::stepfun(outcomes, 1 - heights, right = FALSE),
    mean = mu,
    variance = ss,
    skewness = sum(probs * ((outcomes - mu) / sigma)^3),
    kurtosis_exc = sum(probs * ((outcomes - mu) / sigma)^4) - 3,
    range = range(outcomes),
    .vtype = "discrete",
    .name = "Finite"
  )
}
