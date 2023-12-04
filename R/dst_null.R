#' Empty / NULL distribution
#'
#' Sometimes it's convenient to work with a distribution object that is empty,
#' akin to a `NULL` value. This is especially true when programmatically
#' outputting distributions, such as when a distribution fails to fit to
#' data. This contructor makes such a distribution object. It always evaluates
#' to an `NA` vector.
#'
#' @return Distribution object of class `"dst_null"`.
#' @examples
#' x <- dst_null()
#' mean(x)
#' eval_pmf(x, at = 1:10)
#' enframe_hazard(x, at = 1:10)
#' @export
dst_null <- function() {
  new_distribution(list(), variable = NA_character_, class = "dst_null")
}

#' @export
print.dst_null <- function(x, ...) {
  cat("NULL distribution")
}

#' @export
mean.dst_null <- function(x, ...) {
  NA_real_
}

#' @export
variance.dst_null <- function(x, ...) {
  NA_real_
}

#' @export
skewness.dst_null <- function(x, ...) {
  NA_real_
}

#' @export
kurtosis_exc.dst_null <- function(x, ...) {
  NA_real_
}

#' @export
kurtosis_raw.dst_null <- function(x, ...) {
  NA_real_
}

#' @export
evi.dst_null <- function(x) {
  NA_real_
}

#' @export
eval_density.dst_null <- function(distribution, at) {
  rep(NA_real_, length(at))
}

#' @export
eval_pmf.dst_null <- function(distribution, at) {
  rep(NA_real_, length(at))
}

#' @export
eval_cdf.dst_null <- function(distribution, at) {
  rep(NA_real_, length(at))
}

#' @export
eval_hazard.dst_null <- function(distribution, at) {
  rep(NA_real_, length(at))
}

#' @export
eval_quantile.dst_null <- function(distribution, at) {
  rep(NA, length(at))
}
