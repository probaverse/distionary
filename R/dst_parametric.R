#' Convert p/d/q/r Representations to a Distribution
#'
#' When a distribution has `p`, `d`, `q`, `r` functions available (such as
#' `pnorm()`, `dnorm()`, etc.), `dst_parametric()` creates a distribution
#' that draws on these four functions.
#'
#' @param .name Name of the distribution. Specifically, a string with
#' the suffix of the `p`, `d`, `q`, `r` functions (such as `"norm"`).
#' @param ... Name-value pairs of parameters. Names must be found in the
#' parameter names of the `p`, `d`, `q`, `r` functions.
#' @param .variable Type of random variable represented by the distribution.
#' Warning: defaults to "unknown", where density, pmf, and hazard cannot be
#' evaluated.
#' @param .env Environment beginning the search path when looking for the
#' representation, or the name of an environment (or any object that
#' can be coerced to an environment with `as.environment()`.) Defaults to
#' the calling environment.
#' @examples
#' d <- dst_parametric("norm", mean = 0, sd = 1, .variable = "continuous")
#' eval_density(d, at = -3:3)
#' eval_pmf(d, at = -3:3, strict = FALSE)
#' eval_hazard(d, at = -3:3)
#' @export
dst_parametric <- function(
	.name, ..., .variable = c("unknown", "continuous", "discrete", "mixed"),
	.env = parent.frame()) {
  if (!pdq_functions_exist(.name)) {
    stop(
      'Could not find p/d/q functions for the distribution named ', .name, "."
    )
  }
	v <- match.arg(.variable)
	dots <- rlang::enquos(...)
	param_names <- names(dots)
	vctrs::vec_as_names(param_names, repair = "check_unique")
	params <- lapply(dots, rlang::eval_tidy)
	names(params) <- names(dots)
	res <- list(name = .name,
	            env = as.environment(.env),
	            parameters = params)
	new_distribution(res, variable = v, class = c(.name, "parametric"))
}

# dst_parametric2 <- function(
#     .name, ..., .variable = c("unknown", "continuous", "discrete", "mixed"),
#     representations = representations()
#     ) {
#
# }

# representations <- function(..., pkg) {
#   dots <- rlang::list2(...)
#   fns <- lapply(dots, function(x) )
# }

#' Are there p/d/q functions available?
#'
#' For a given distribution name, such as "norm", checks to see
#' whether or not p/d/q functions all exist for that distribution --
#' such as `pnorm()`, `dnorm()`, and `qnorm()`.
#' Methods for `eval_cdf()`, `eval_quantile()`, and `eval_density/mass()`
#' suffice as alternatives. Internal function.
#'
#' @param name Name of the distribution; suffix to p/d/q.
#' @return `TRUE` if p/d/q functions are all available for this distribution;
#' `FALSE` otherwise.
pdq_functions_exist <- function(name) {
  p_exists <- exists(paste0("p", name), where = parent.frame(2)) ||
    exists(paste0("eval_cdf.", name))
  d_exists <- exists(paste0("d", name), where = parent.frame(2)) ||
    exists(paste0("eval_density.", name)) ||
    exists(paste0("eval_pmf.", name))
  # q_exists <- exists(paste0("q", name), where = parent.frame(2)) ||
  #   exists(paste0("eval_quantile.", name))
  p_exists || d_exists
}

#' @export
eval_cdf.parametric <- function(distribution, at) {
	function_name <- paste0("p", distribution$name)
	rlang::exec(
	  function_name, at, !!!distribution$parameters,
	  .env = distribution$env
	)
}

#' @export
eval_survival.parametric <- function(distribution, at) {
	function_name <- paste0("p", distribution$name)
	pdist_formals <- names(formals(
	  function_name, envir = distribution$env
	))
	if ("lower.tail" %in% pdist_formals) {
		rlang::exec(
		  function_name, at, !!!distribution$parameters, lower.tail = FALSE,
		  .env = distribution$env
		)
	} else {
		1 - rlang::exec(
		  function_name, at, !!!distribution$parameters,
		  .env = distribution$env
		)
	}
}

#' @export
eval_density.parametric <- function(distribution, at, strict = TRUE) {
	v <- variable(distribution)
	if (v == "continuous") {
		function_name <- paste0("d", distribution$name)
		return(rlang::exec(
		  function_name, at, !!!distribution$parameters,
		  .env = distribution$env
		))
	}
	if (strict) {
		stop("Distribution is of variable type '", v, "'; density only ",
			 "exists for continuous variables. Perhaps you'd like to ",
			 "evaluate outside of strict mode?")
	}
	if (v == "discrete") {
		return(rep(0, length(at)))
	}
	stop("Evaluating non-strict density for a parametric distribution is ",
		 "not yet available. Check that your distribution's variable type ",
		 "is specified correctly, or for mixed variables, consider creating ",
		 "separate continuous and discrete distributions and",
		 "running `distplyr::mix()`.")
}

#' @export
eval_pmf.parametric <- function(distribution, at, strict = TRUE) {
	v <- variable(distribution)
	if (v == "discrete") {
		function_name <- paste0("d", distribution$name)
		return(suppressWarnings(
			rlang::exec(
			  function_name, at, !!!distribution$parameters,
			  .env = distribution$env
			)
		))
	}
	if (strict) {
		stop("Distribution is of variable type '", v, "'; pmf only ",
			 "exists for discrete variables. Perhaps you'd like to ",
			 "evaluate outside of strict mode?")
	}
	if (v == "continuous") {
		return(rep(0, length(at)))
	}
	stop("Evaluating non-strict pmf for a parametric distribution is ",
		 "not yet available. Check that your distribution's variable type ",
		 "is specified correctly, or for mixed variables, consider creating ",
		 "separate continuous and discrete distributions ",
		 "and running `distplyr::mix()`.")
}

#' @export
eval_quantile.parametric <- function(distribution, at) {
	function_name <- paste0("q", distribution$name)
	e <- distribution$env
	if (exists(function_name, envir = e)) {
	  rlang::exec(
	    function_name, at, !!!distribution$parameters,
	    .env = e
	  )
	} else {
	  NextMethod()
	}
}

#' @export
realise.parametric <- function(distribution, n = 1) {
	function_name <- paste0("r", distribution$name)
	e <- distribution$env
	if (exists(function_name, envir = e)) {
	  rlang::exec(
	    function_name, n, !!!distribution$parameters,
	    .env = e
	  )
	} else {
	  NextMethod()
	}
}
