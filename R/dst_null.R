#' Null Distribution
#'
#' Sometimes it's convenient to work with a distribution object that is
#' akin to a missing value. This is especially true when programmatically
#' outputting distributions, such as when a distribution fails to fit to
#' data. This function makes such a distribution object. It always evaluates
#' to `NA`.
#'
#' @returns A Null distribution.
#' @examples
#' x <- dst_null()
#' mean(x)
#' eval_pmf(x, at = 1:10)
#' @export
dst_null <- function() {
  distribution(
    .parameters = NULL,
    cdf = \(x) rep(NA_real_, length(x)),
    density = \(x) rep(NA_real_, length(x)),
    hazard = \(x) rep(NA_real_, length(x)),
    chf = \(x) rep(NA_real_, length(x)),
    pmf = \(x) rep(NA_real_, length(x)),
    odds = \(x) rep(NA_real_, length(x)),
    return = \(x) rep(NA_real_, length(x)),
    quantile = \(x) rep(NA_real_, length(x)),
    mean = NA_real_,
    variance = NA_real_,
    skewness = NA_real_,
    kurtosis_exc = NA_real_,
    .name = "Null",
    .vtype = NA_character_
  )
}
