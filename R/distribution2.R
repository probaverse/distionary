#' Make a distribution from scratch
#'
#' Initiate a distribution object.
#'
#' @param variable Is this variable continuous, discrete, or mixed?
#' @param parameters Vector of unquoted variable names acting as the
#' distribution's parameters, if relevant.
#' @param env Either the package name where Environment containing objects
#' needed for evaluating the distribution.
#' @details
#' This function sets up a distribution object with high-level
#' information about the distribution. You can add / modify information about
#' the distribution downstream using the `set_*()` family of functions.
#'
#' This becomes the global environment for the function, so that when an object
#' is needed to be evaluated, it ultimately looks here.
#' @return A distribution object with nothing in it.
#' @export
distribution2 <- function(..., .vtype = c("continuous", "discrete", "mixed"),
                          parameters = params()) {
  dots <- rlang::enquos(...)
  # Can't just do rlang::env_bind(e, ...) because functions in ... gets
  #  evaluated enclosing distionary. Want it to enclose the environment
  #  containing other things in ...
  #
  # When calling this function, an environment opens up below the distionary
  #  namespace. I need the enclosing environment to be able to find objects
  #  in ..., but then be able to look in parent environments for operations
  #  like `+` and objects like `pi`. It should not enclose an environment
  #  below the distionary namespace, in case the user wants to reference
  #  an object in Global but gets intercepted by distionary namespace.
  # Conclusion: put the environment below caller environment (because they
  #  may not be calling it from Global.)
  fn_env <- rlang::env(rlang::caller_env())
  param_env <- rlang::env(fn_env)
  # Evaluate ... within e, so that functions that are made enclose e.
  # obs <- with(e, lapply(dots, rlang::eval_tidy))
  mask <- new_data_mask(param_env, fn_env) # Recreate data mask as parameters are resolved.
  obs <- lapply(dots, \(x) rlang::eval_tidy(x, data = mask))
  # Now bind these objects to e.
  rlang::env_bind(mask, !!!obs)
  attr(mask, "vtype") <- match.arg(.vtype)
  attr(mask, "parameters") <- parameters
  class(mask) <- c("dst", class(mask))
  mask
}


eval_density2 <- function(distribution, at) {
  # cll <- rlang::call2("density", at)
  rlang::exec(".density", at, .env = rlang::as_environment(distribution))
}



#' @export
set_params <- function(distribution, ...) {
  pairs <- rlang::enquos(...)
  pairs <- lapply(pairs, rlang::eval_tidy)
  rlang::new_distribution()
  rlang::env_bind(distribution, !!!pairs)
  distribution
}

# distribution3() |> set_density(dnorm)


#' Constructor Function for "dst" Objects
#'
#' @param l List containing the components of a distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#'   or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_distribution <- function(l, variable, ..., class = character()) {
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
  names_ <- names(ell)
  # childenv <- rlang::new_environment(parent = distribution$bottom)
  # distribution$bottom <- childenv
  # mask <- rlang::new_data_mask(distribution$bottom, distribution$top)
  for (i in seq_along(ell)) {
    name_ <- names_[i]
    rlang::eval_tidy(rlang::expr(assign(name_, ell[[i]])), data = distribution$mask)
  }
  check_params(ell)
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
restrict_params <- function(distribution, ..., .env) {
  new_restrictions <- rlang::enquos(...)
  distribution$paramspace <- append(distribution$paramspace, new_restrictions)
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
