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
  if (length(outcomes) != length(probs)) {
    stop("Inputs `outcomes` and `probs` should have the same length.")
  }
  if (length(outcomes) == 0) {
    warning(
      "Can't make a finite distribution from empty data. ",
      "Returning an empty distribution."
    )
    return(dst_null())
  }
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
  cumsum_p <- cumsum(probs)
  heights <- c(0, cumsum_p)
  taus <- cumsum_p[-length(cumsum_p)]
  ## Sort outcomes and probabilities for the stepfuns
  order <- order(outcomes)
  outcomes <- outcomes[order]
  probs <- probs[order]
  distribution(
    .parameters = list(values = outcomes, probs = probs),
    pmf = function(q) {
      na_q <- is.na(q)
      matched <- match(q, outcomes)
      res <- probs[matched]
      res[!is.na(q) & is.na(matched)] <- 0
      res
    },
    cdf = stats::stepfun(outcomes, heights, right = FALSE),
    quantile = stats::stepfun(taus, outcomes, right = TRUE),
    realise = \(n) sample(outcomes, size = n, replace = TRUE, prob = probs),
    survival = stats::stepfun(outcomes, heights, right = FALSE),
    mean = mu,
    variance = ss,
    skewness = sum(probs * ((outcomes - mu) / sigma)^3),
    kurtosis = sum(probs * ((outcomes - mu) / sigma)^4),
    range = range(outcomes),
    .vtype = "discrete",
    .name = "Finite"
  )
}
