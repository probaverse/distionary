#' Make a distribution from scratch
#'
#' Initiate a distribution object.
#'
#' @param variable Is this variable continuous, discrete, or mixed?
#' @param parameters Vector of unquoted variable names acting as the
#' distribution's parameters, if relevant.
#' @param env Either the package name where Environment containing objects needed for evaluating the
#' distribution.
#' @details
#' This function sets up a distribution object with high-level
#' information about the distribution. You can add / modify information about
#' the distribution downstream using the `set_*()` family of functions.
#'
#' This becomes the global environment for the function, so that when an object
#' is needed to be evaluated, it ultimately looks here.
#' @return A distribution object with nothing in it.
#' @export
distribution2 <- function(variable = c("continuous", "discrete", "mixed"),
                          parameters = params(), env = rlang::new_environment()) {
  v <- match.arg(variable)
  mask <- rlang::new_data_mask(rlang::new_environment(parent = env), top = env)
  res <- list(variable = v,
              parameters = parameters,
              paramspace = list(),
              mask = mask)
  new_distribution(res, variable = v)
}

#' Constructor Function for "dst" Objects
#'
#' @param l List containing the components of a distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#'   or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_distribution <- function(l, variable, ...,
                             class = character()) {
  structure(
    l,
    variable = variable,
    class    = c(class, "dst")
  )
}

#' Parameters are a named list, the names being the parameters,
#' and the contents of each parameter entry being expressions defining
#' the parameter space. COMPLICATION: space defined with more than one
#' parameter together.
params <- function(...) {
  ellipsis::check_dots_unnamed() # No assignment allowed.
  rlang::ensyms(..., .named = TRUE)
}

#' @export
param_resolve <- function(distribution, ...) {
  ell <- rlang::enquos(...)
  ell2 <- lapply(ell, function(l) rlang::eval_tidy(l, data = distribution$mask))
  check_params(ell2)
  rlang::env_bind(distribution$mask, !!!ell2)
  distribution
}

check_params <- function(params) {
  # Check that parameters fall within the parameter space.
  invisible(params)
}

#' Specify a distributional representation
#'
#' Add a representation to a distribution, such as density, CDF,
#' hazard function, PMF, etc. If a representation is already specified,
#' it will be replaced with a warning message.
#' @param distribution A distribution object.
#' @param fun A function specifying the distributional representation,
#' with arguments only for the distribution variables (not, for example,
#' parameters).
#' @param env Environment; if upon executing the function `fun` an object
#' has not been defined, this environment will be searched,
#' followed by the global one specified in
#' the `distribution()` function.
set_density <- function(distribution, fun) {
  if (!is.null(distribution$representations$density)) {
    warning("Density function has already been specified for this ",
            "distribution; replacing with the new one.")
  }
  distribution$representations$density <- fun
  distribution
}

#' @export
eval_density2 <- function(distribution, at) UseMethod("eval_density2")

#' @export
eval_density2.dst <- function(distribution, at) {
  f <- distribution$representations$density
  # cll <- rlang::call2(f, at)
  rlang::eval_tidy(f, data = distribution$mask)
}

#' @export
restrict_params <- function(distribution, ..., .env) {
  new_restrictions <- rlang::enquos(...)
  distribution$paramspace <- append(distribution$paramspace, new_restrictions)
}

#' @export
set_params <- function(distribution, ...) {
  pairs <- rlang::enquos(...)
  pairs <- lapply(pairs, rlang::eval_tidy)
  rlang::env_bind(distribution$mask, !!!pairs)
  distribution
}


# ### Sample
#
# distribution2(density = \(x) 2 * x, support = c(0, 1))
#
# dst_norm <- function(mu, sigma) distribution2(
#   density = \(x) pnorm(x, mu, sigma),
#   cdf = \(x) pnorm(x, mu, sigma),
#   survival = \(x) pnorm(x, mu, sigma, lower.tail = FALSE),
#   .pkg = "stats"
# )
#
#
# dst_gev <- function(loc, scale, shape) dst_parametric("gev", .pkg = "ismev")
#
# dst_gev <- as_parametric("gev", .pkg = "ismev")
#
# dst_gev <- \(loc, scale, shape) distribution2(
#   density = \(x) devd(x, loc, scale, shape, type = "GEV"),
#   ...
# )
#
# # OR:
# dst_gev <- distribution2(
#   density = \(x) devd(x, loc, scale, shape, type = "GEV"),
#   parameters = parameters(loc, scale > 0, shape >= 0, my_fun(shape) < my_upper_bd)
# )
