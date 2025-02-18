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
  if (is.na(shape) || is.na(rate)) {
    return(dst_null())
  }
  if (length(shape) != 1 || length(rate) != 1) {
    stop("Input parameters must have length 1.")
  }
  if (rate <= 0) {
    stop("rate parameter must be positive.")
  }
  d <- dst_pearson3(location = 0, scale = 1 / rate, shape = shape)
  attributes(d)[["name"]] <- "Gamma"
  parameters(d) <- list(shape = shape, rate = rate)
  d
}
