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
#' @details
#' In order for a distribution to be meaningful, it must contain at least one
#' representation from which probabilities and properties can be determined.
#' Here is a complete list of names to specify in `...` that will be
#' recognized. They begin with `.` to avoid duplication with
#' similarly named objects. When defining them, use the names of the
#' distribution parameters (if present) as if they are known constants;
#' they will be found using a data mask (keep reading to find out more).
#'
#' Representations that fully define the distribution:
#'
#' - `.density`
#' - `.cdf`
#' - `.survival`
#' - `.mass`
#' - `.`
#'
#' Properties of the distribution:
#'
#' - `.mean`
#' - `.stdev`
#' - `.skewness`
#'
#' Not all of these need to be specified, but more is generally better, because
#' unspecified quantities will need to be computed, sometimes with an
#' algorithm that may be slower than outright specification.
#'
#' These representations and properties (whenever available) are stored in
#' a special representation environment whose parent is `.parenv`, so that
#' the environment is like the namespace of a mini R package.
#' Like an R package, the representation environment is locked to prevent
#' accidental redefinition of distribution families.
#' Objects in this
#' environment may depend on distribution parameters, which are not defined
#' in the representation environment, and therefore may not evaluate on
#' their own. To evaluate, a data mask is constructed with the parameters
#' fully specified.
#'
#' The representation environment is akin to a distribution family or class
#' of distributions, which cannot be evaluated on their own without specifying
#' parameters.
#' @examples
#' # <standard example: hard-coded distribution with no parameters>
#' # <example with parameters>
#' # <advanced example defining a distribution class with a general function
#' #  g, and then making a sub class using .parenv = representation env>
#' # <advanced example like the above, but perhaps only by specifying a
#' #  parameter, such as getting the Normal family by setting skew = 0 in the
#' #  skew Normal family.>
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
  repres_env <- rlang::env(.parenv)
  env_bind_within_lazy(repres_env, ...)
  rlang::env_lock(repres_env)
  res <- list(
    name = .name,
    vtype = .vtype,
    parameters = .params,
    repres_env = repres_env
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

#' Parameters are a named list, the names being the parameters,
#' and the contents of each parameter entry being expressions defining
#' the parameter space. COMPLICATION: space defined with more than one
#' parameter together.
params <- function(...) {
  ellipsis::check_dots_unnamed() # No assignment allowed.
  rlang::ensyms(..., .named = TRUE)
}




