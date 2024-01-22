#' @export
eval_cdf.finite <- function(distribution, at) {
	size <- distribution$probabilities$size
	location <- distribution$probabilities$location
	heights <- c(0, cumsum(size))
	cdf <- stats::stepfun(location, heights, right = FALSE)
	cdf(at)
}

#' @export
eval_survival.finite <- function(distribution, at) {
	size <- distribution$probabilities$size
	location <- distribution$probabilities$location
	heights <- 1 - c(0, cumsum(size))
	sf <- stats::stepfun(location, heights, right = FALSE)
	sf(at)
}

#' @export
eval_quantile.finite <- function(distribution, at, ...) {
	size <- distribution$probabilities$size
	location <- distribution$probabilities$location
	if (length(location) == 1L) {
		res <- at
		res[!is.na(res)] <- location
		res
	} else {
		taus <- cumsum(size)
		taus <- taus[-length(taus)]
		qf <- stats::stepfun(taus, location, right = TRUE)
		qf(at)
	}
}

#' @export
eval_pmf.finite <- function(distribution, at, strict = TRUE) {
	size <- distribution$probabilities$size
	location <- distribution$probabilities$location
	vapply(at, function(x) sum(size[x == location]), FUN.VALUE = numeric(1L))
}

#' @export
realise.finite <- function(distribution, n = 1, ...) {
	size <- distribution$probabilities$size
	location <- distribution$probabilities$location
	sample(location, size = n, replace = TRUE, prob = size)
}

#' @export
realise.degenerate <- function(distribution, n = 1, ...) {
  x <- distribution$probabilities$location
  rep(x, n)
}
