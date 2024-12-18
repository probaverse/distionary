#' @export
mean.finite <- function(x, ...) {
	with(x$probabilities, {
		sum(size * location)
	})
}


#' @export
variance.finite <- function(distribution, ...) {
  mu <- mean(distribution)
	with(distribution$probabilities, {
		mu2 <- sum(size * location^2)
		mu2 - mu^2
	})
}

#' @export
skewness.finite <- function(distribution, ...) {
	mu <- mean(distribution)
	sigma <- stdev(distribution)
	with(distribution$probabilities, {
		trans <- ((location - mu) / sigma)^3
		sum(trans * size)
	})
}

#' @export
kurtosis_exc.finite <- function(distribution, ...) {
	mu <- mean(distribution)
	sigma <- stdev(distribution)
	with(distribution$probabilities, {
		trans <- ((location - mu) / sigma)^4
		sum(trans * size) - 3
	})
}

#' @rdname range
#' @export
range.finite <- function(distribution, ...) {
	unlisted_probability_list <- distribution$probabilities$location
	min_val <- min(unlisted_probability_list)
	max_val <- max(unlisted_probability_list)
	c(min_val, max_val)
}
