#' Make a distribution from scratch
#'
#' Initiate a distribution object by specifying the representation space.
#'
#' @param ... Properties of the distribution as name-value pairs. See details
#' for special property names.
#' @param .vtype Type of random variable. Common options
#' include "continuous", "discrete", or "mixed", but remains open for
#' extensibility.
#' @param .params Vector of unquoted variable names acting as the
#' distribution's parameters, if relevant.
#' @param .parenv Parent environment of objects specified in `...`. For
#' advanced use, as the default option (the caller environment) suffices in
#' most situations. An example is building a subclass of a distribution, and
#' using the larger class's representation environment as the parent.
#' @return A distribution object with nothing in it.
#' @export
distribution <- function(
    ..., .name = NULL, .vtype = NULL, .params = params(),
     .parenv = rlang::caller_env()
) {
  # For functions defined on-the-fly in `...`, need to be deliberate about
  # the enclosing environment. My mental model of the enclosing environment
  # is like an R package's namespace, containing all necessary objects to
  # ensure the function works. This is not trivial if chaining functions;
  # see the examples. By default, capturing functions in `...` with something
  # like `list(...)` or `rlang::list2(...)` evaluates the functions such that
  # they enclose distionary, but this will not allow the functions to refer to
  # other functions defined in `...` that it depends on.
  #
  # When calling this function, an environment opens up below the distionary
  #  namespace. I need the enclosing environment to be able to:
  #
  # - find objects in ... that functions may depend on;
  # - still be able to rely on base objects like `+` and `pi` (so, not have
  #   the Empty env as a parent);
  # - if the user specifies an argument intending to draw from the working
  #   environment, it should not be intercepted by an intermediary env. So,
  #   since the execution environment is a child of distionary, do not use
  #   this execution environment, in case distionary intercepts a call.
  #
  # The strategy is to put the environment below the caller environment; but,
  # to also give the option to change that for advanced users (for example,
  # to make a subclass of distributions, the representation environment of the
  # parent distribution family can be specified here). Note that an
  # environment is a useful implementation in the first place, instead of
  # a list, because it allows for this cross referencing to other
  # homemade functions. In this way, it's as thought the user is making their
  # own R package.
  #
  # Note that there may be potential issues with the mutability
  # of the representation environment: if the environment is changed in one
  # place, all other distributions that rely on it will also be affected.
  # But, this is like meddling with the `+` function: something that is
  # ill-advised and not commonly done. Options:
  #
  # - Instead of the distribution
  #   pointing to the environment, the distribution will contain a list
  #   indicating where to find each representation. This will ensure the
  #   garbage collector does not abolish the environment and will make the
  #   environment less obvious to access. This will also force the user
  #   to specify representations rather than just create a sea of objects
  #   in `...`. Representations will be specially indicated by starting with
  #   a dot `.`.
  # - Consider `rlang::env_lock()` to lock the environment to make it more
  #   like a true namespace. Then the distribution can directly point to
  #   the representation environment, and it can be assumed that
  #   representations are simply present there, instead of deliberately
  #   linking them. The environment can be
  #   stored as an attribute, to abstract away the representations.
  #
  # This setup also means that parameters can act as a data mask.
  repres_env <- rlang::env(parenv)
  bind_to_repres_env(repres_env, ...)
  rlang::env_lock(repres_env)
  res <- list(
    name = .name,
    vtype = .vtype,
    params = .params,
    repres_env = repres_env,
  )
  ## UNLOCKED OPTION
  # nms <- names(repres_env)
  # i_repres <- grep("^\\.", nms)
  # if (length(i_repres) == 0) {
  #   stop(
  #     "Need to specify at least one distributional representation. These ",
  #     "should start with a dot `.`."
  #   )
  # }
  # nms_repres < nms[i_repres]
  # res <- list(
  #   repres = rlang::env_get_list(repres_env, nms = nms_repres),
  #   params = parameters
  # )
  class(res) <- "dst"
  validate_distribution(res)
  res
}

repres_env <- function(distribution) {
  attr(distribution, "repres")
}

bind_to_repres_env <- function(repres_env, ...) {
  dots <- rlang::enexprs(...)
  obs <- lapply(dots, \(x) eval(x, repres_env))
  rlang::env_bind(repres_env, !!!obs)
  invisible()
}


eval_density2 <- function(distribution, at) {
  cll <- rlang::call2(".density", at)
  rlang::eval_tidy(cll, data = distribution, env = repres_env(distribution))
  # rlang::exec(".density", at, .env = repres_env(distribution))
}



#' @export
set_params <- function(distribution, ...) {
  pairs <- rlang::enquos(...)
  pairs <- lapply(pairs, rlang::eval_tidy)
  distribution$params <- append(distribution$params, pairs)
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
