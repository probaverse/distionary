#' Gamma Distribution
#'
#' Makes a Gamma distribution.
#'
#' @param shape Shape parameter; single positive numeric.
#' @param rate Rate parameter; single positive numeric.
#' @returns A Gamma distribution.
#' @examples
#' dst_gamma(2, 1)
#' @export
dst_gamma <- function(shape, rate) {
  checkmate::assert_numeric(shape, 0, len = 1)
  checkmate::assert_numeric(rate, 0, len = 1)
  if (is.na(shape) || is.na(rate)) {
    return(dst_null())
  }
  d <- dst_pearson3(location = 0, scale = 1 / rate, shape = shape)
  attributes(d)[["name"]] <- "Gamma"
  parameters(d) <- list(shape = shape, rate = rate)
  d
}
