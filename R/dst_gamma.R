#' Gamma Distribution
#'
#' Makes a distribution belonging to the family of
#' Gamma distributions.
#'
#' @param shape Shape parameter; positive.
#' @param rate Rate parameter; positive.
#'
#' @examples
#' dst_gamma(2, 1)
#' @export
dst_gamma <- function(shape, rate) {
  if (rate <= 0) {
    stop("rate parameter must be positive.")
  }
  d <- dst_pearson3(location = 0, scale = 1 / rate, shape = shape)
  attributes(d)[["name"]] <- "Gamma"
  parameters(d) <- list(shape = shape, rate = rate)
  d
}

