#' @export
mean.parametric <- function(x, ...) {
  ellipsis::check_dots_empty()
	quantity_parametric(x, "mean")
}

#' @export
variance.parametric <- function(distribution, ...) {
	quantity_parametric(distribution, "variance")
}

#' @export
stdev.parametric <- function(distribution, ...) {
  quantity_parametric(distribution, "stdev")
}

#' @export
skewness.parametric <- function(distribution, ...) {
	quantity_parametric(distribution, "skewness")
}

#' @export
kurtosis_exc.parametric <- function(distribution, ...) {
	quantity_parametric(distribution, "kurtosis_exc")
}

#' @export
range.parametric <- function(distribution, ...) {
	quantity_parametric(distribution, "range")
}

#' @export
median.parametric <- function(x, ...) {
  ellipsis::check_dots_empty()
	quantity_parametric(x, "median")
}

#' Grab a quantity from the database
#'
#' For a parametric distribution, evaluates a quantity
#' (such as mean, median, range, ...) if the distribution has
#' an entry in the `.quantities` database. For distributions without
#' the "parametric" (sub-) class, quantity evaluation is delegated
#' to the next higher level class.
#'
#' @param distribution A distribution.
#' @param quantity Character; name of the quantity to extract.
#' @return The desired quantity, evaluated.
quantity_parametric <- function(distribution, quantity) {
	d_name <- distribution$name
	q_expr <- .quantities[[d_name]][[quantity]]
	if (!is.null(q_expr)) {
		rlang::eval_tidy(q_expr, data = parameters(distribution))
	} else {
		rlang::exec("NextMethod", .env = rlang::caller_env(n = 1))
	}
}
