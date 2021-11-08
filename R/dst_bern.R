#' Bernoulli Distribution
#'
#' Makes a distribution belonging to the family of
#' bernoulli distributions.
#'
#' @param p
#'
#' @examples
#' dst_bern(0.3)
#'
#' @export
dst_bern <- function(p){
  if(p < 0 | p > 1){
    stop('p must be within 0 and 1')
  }
  dst_binom(size = 1, p = p)
}