library(rlang)

#' Takes a quo and parses it into the components of a function
#' (body, arguments, environment); if just a quosure, arguments
#' are an empty pairlist (which is identical to NULL).
deparse_quo_as_function <- function(quo) {
  t <- try(rlang::eval_tidy(quo), silent = TRUE)
  if (is.function(t)) {
    # return(list(
    #   args = formals(t),
    #   body = body(t),
    #   env = environment(t)
    # ))
    return(t)
  } else {
    # return(list(
    #   args = NULL,
    #   body = quo_get_expr(quo),
    #   env = quo_get_env(quo)
    # ))
    return(rlang::new_function(
      args = NULL,
      body = quo_get_expr(quo),
      env = quo_get_env(quo)
    ))
  }
}

#' Add distribution definitions to `definition` environment.
env_bind_definitions <- function(.env, ...) {
  dots <- rlang::enquos(...)
  parsed <- lapply(dots, deparse_quo_as_function)
  rlang::env_bind(.env, !!!parsed)
  invisible(.env)
}

standard_network <- function() {
  c("cdf", "survival", "quantile", "return", "mass", "odds", "density",
    "hazard", "chf")
}

#' @param fn_name Name / expression holding the binding to the function
#' in the definition environment.
#' @param bottom,top Bottom and top environments in the data mask,
#' not including function formals that will be added later. For example,
#' bottom is the parameter environment, and top is the network
#' environment.
#' @param where_fn Environment where the name in `fn_name` is
#' bound to the lower level function; used as the enclosing environment
#' of the higher level (wrapping) function.
#' @details `inject_mask_into_function` ensures a data mask is
#' searched when evaluating a function before its enclosing
#' environment is searched. It does this through tidy evaluation of the
#' function body, constructing the data mask such that an environment
#' containing the arguments goes below the original bottom environment
#' of the data mask.
#'
#' Mask injection also applies to function arguments.
#'
#' The top of the mask is intended to be the network environment,
#' where bindings to these wrapped functions are stored. This allows
#' for distribution definitions to refer to other defined objects.
#' The bottom (prior to inclusion of the arguments environment) of the
#' mask is intended to contain the parameters.
#' @note
#' The body of the lower level function is not injected into
#' the higher level (wrapping) function in case it contains data.
#' This allows multiple instances of a distribution family to be
#' created and point to the same defining environment.
#'
#' An alternative is to simply bind the user-defined values (actively)
#' to an environment, and put that environment along with a
#' parameter environment underneath each function's enclosing
#' environment. This may work, but could result in needing to
#' duplicate environments each time a function has a different
#' enclosing environment.
#'
# inject_mask_into_function <- function(args, body, env, bottom, top,
# wrapping_env = caller_env()) {
inject_mask_into_function <- function(fn_name, bottom, top, where_fn) {
  fmls <- eval_tidy(expr(formals(!!fn_name)), env = where_fn)
  # higher_args <- rlang::call_args(rlang::call_match())
  masked_body <- rlang::expr({
    ## Make data mask
    formals_env <- rlang::new_environment(parent = !!bottom)
    mask <- rlang::new_data_mask(formals_env, top = !!top)
    mask$.dst <- rlang::as_data_pronoun(mask)
    ## Get formals and their bound expressions
    fmls_exprs <- rlang::call_args(rlang::call_match())
    ## Evaluate arguments in mask, and bind to formals environment.
    call_env <- rlang::caller_env()
    fmls_evaled <- lapply(fmls_exprs, function(expr_) {
      rlang::eval_tidy(expr_, data = mask, env = call_env)
    })
    for (j in seq_along(fmls_evaled)) {
      formals_env[[names(fmls_evaled)[j]]] <- fmls_evaled[[j]]
    }
    ## Conduct the calculations under the data mask.
    ## The data mask has the formals already evaluated in the bottom
    ## environment.
    rlang::eval_tidy(
      # !!higher_args$body, data = mask, env = !!env
      body(!!fn_name), data = mask, env = environment(!!fn_name)
    )
  })
  rlang::new_function(
    args = fmls,
    body = masked_body,
    env = where_fn
  )
}

#' Makes a data mask as usual, but endows a `.dst` pronoun
#' over the whole mask (as opposed to `.data`).
#' Note that `.dst` is desirable over `.data` in case
#' quantities are evaluated in a dplyr pipe.
new_distribution_mask <- function(bottom, top = bottom) {
  mask <- rlang::new_data_mask(bottom, top)
  mask$.dst <- rlang::as_data_pronoun(mask)
  mask
}

generate_network_environment <- function(def_env, param_list) {
  nm <- names(def_env)
  net <- standard_network()
  missed <- setdiff(net, nm)
  network_env <- rlang::new_environment()
  param_env <- rlang::as_environment(
    as.list(param_list), parent = network_env
  )
  for (i in seq_along(nm)) {
    fn <- def_env[[nm[i]]]
    fn_name <- rlang::parse_expr(nm[i])
    higher_function <- inject_mask_into_function(
      fn_name, bottom = param_env, top = network_env, where_fn = def_env
    )
    if (is.null(formals(higher_function))) {
      rlang::env_bind_active(
        network_env, !!nm[i] := higher_function
      )
    } else {
      rlang::env_bind(
        network_env, !!nm[i] := higher_function
      )
    }
  }
  if ("cdf" %in% missed) {
    stop("Currently, cdf is required when defining a distribution.")
  }
  if (all(c("density", "mass") %in% missed)) {
    stop(
      "Currently, density or mass function is required when",
      "defining a distribution."
    )
  }
  if ("survival" %in% missed) {
    cdf_fmls <- formals(def_env$cdf)
    cdf_fmls_nms <- rlang::syms(names(cdf_fmls))
    rlang::env_bind(
      network_env,
      survival = rlang::new_function(
        args = formals(fn),
        body = rlang::expr(1 - !!rlang::call2("cdf", !!!cdf_fmls_nms)),
        env = network_env
      )
    )
  }
  network_env
}

eval_representation <- function(distribution, repres, at) {
  if (!inherits(distribution, "dst")) {
    stop("Not a distribution")
  }
  at <- rlang::enexpr(at)
  # repres_expr <- rlang::parse_expr(repres)
  rlang::eval_tidy(
    as.call(list(as.name(repres), at)),
    distribution$network_env
  )
}

eval_density <- function(distribution, at) {
  at <- rlang::enexpr(at)
  cll <- as.call(list(
    rlang::expr(eval_representation),
    distribution, "density", at
  ))
  rlang::eval_tidy(cll)
}

eval_distribution <- function(distribution, x) {
  x <- rlang::enquo(x)
  rlang::eval_tidy(x, distribution$base_mask)
}

distribution <- function(..., .params = rlang::pairlist2()) {
  definition_env <- rlang::new_environment(parent = rlang::caller_env())
  env_bind_definitions(definition_env, ...)
  distribution <- list(
    definition_env = definition_env,
    params = .params
  )
  class(distribution) <- "fam"
  update_dst_class(distribution)
}







# ----- parameters ------

parameters_all_resolved <- function(distribution) {
  params <- parameters(distribution)
  nulls <- vapply(params, is.null, FUN.VALUE = logical(1))
  all(!nulls)
}

#' Resolve Parameters
#'
#' For a given distribution family, specify values for the parameters.
#' This includes freeing up parameters after having been specified.
#' @param distribution Distribution or distribution family.
#' @param ... Name-value pairs that bind a value to distribution
#' parameters. Set to `NULL` to reset a parameter.
#' @param .force Logical; should restrictions imposed by the parameter
#' space be ignored? `FALSE` if not (the default); `TRUE` if so.
#' @examples
#' fam <- dst_norm()
#' d <- resolve_parameters(fam, mu = 5, sigma = 3)
#' resolve_parameters(d, mu = NULL)
#'
#' # A value outside of the parameter space cannot be specified unless
#' # it is forced.
#' resolve_parameters(fam, sigma = -1, .force = TRUE)
#'
resolve_parameters <- function(distribution, ..., .force = FALSE) {
  new_parameters <- update_parameters_list(distribution, ...)
  if (!.force) {
    validate_parameters_bare(new_parameters, distribution$parspace)
  }
  distribution$parameters <- new_parameters
  update_dst_class(distribution)
}

#' Update parameters with specified values. NULL is allowed.
#' The parameter space is not checked. The updated list of
#' parameters is returned, not the distribution object with the
#' parameters updated.
#' As usual, parameters are evaluated under tidy evaluation, but
#' not under the distribution data mask, because presumably values
#' in the data mask are not ready to be called until the distribution
#' is resolved.
update_parameters_list <- function(distribution, ...) {
  dots <- rlang::enquos(...)
  dot_names <- names(dots)
  parameters <- parameters(distribution)
  parnames <- names(parameters)
  for (nm in dot_names) {
    if (nm == "") stop(
      "Resolving parameters requires a name-value pair. ",
      "The following entry is missing a name: ",
      rlang::expr_text(dots[[i]])
    )
    if (!(nm %in% parnames)) stop(
      "Parameter `", nm, "` is not being tracked in this distribution. ",
      "Did you misspell a parameter, or need to redefine the family?"
    )
  }
  dots_evald <- lapply(dots, rlang::eval_tidy)
  for (i in seq_along(dots_evald)) {
    nm <- dot_names[i]
    val <- dots_evald[[i]]
    if (is.null(val)) {
      parameters[nm] <- list(NULL)
    } else {
      parameters[[nm]] <- val
    }
  }
  parameters
}

#' Validate whether parameter values are valid
#'
#' Check to see whether a parameter specification is valid (i.e., falls
#' within the parameter space).
#'
#' Note: if the parameter specification is incomplete and the parameter space
#' has rules containing both specified and unspecified parameters,
#' it is possible for the specification to be deemed valid even if
#' it is mathematically invalid. See examples.
#'
#' @param distribution Distribution or distribution family.
#' @param ... Name-value pairs that bind a value to distribution
#' parameters.
#' @param .error Logical; if `TRUE`, throws an error if parameter
#' values are outside of the parameter space. If `FALSE`, returns
#' a single logical indicating whether parameter values are valid.
#' @returns If `.error` is `TRUE`, either throws an error if
#' parameter values are invalid, or invisibly returns the input
#' `distribution` if valid.
#'
#' If `.error` is `FALSE`, returns `TRUE` if the parameter values
#' are valid, and `FALSE` if not.
#' @details
#' This function first obtains a hypothetical new parameter set based
#' on the specification in `...`, in the same way that
#' `resolve_parameters()` updates the parameter set. But instead
#' of assigning this new parameter set to the distribution,
#' the parameter set is checked against each rule in the parameter
#' space.
#'
#' This means that this function is not vectorised:
#' it does not check whether each specification in `...` is
#' valid.
#'
#' @examples
#' fam <- dst_norm()
#' validate_parameters(fam, mu = 5, sigma = -2, .error = FALSE)
#' validate_parameters(fam)
#'
#' # Validating can still be done after parameters are resolved.
#' d <- dst_norm(0, 1)
#' validate_parameters(d, sigma = -2, .error = TRUE)
#'
#' # An example where a parameter is mistakenly seen as valid,
#' # because of a rule that involves an unspecified and specified
#' # parameter. In this case, the implicit rule `theta >= 0` should
#' # be added to avoid this issue.
#' fam <- distribution(.parameters = params(theta, gamma, theta > gamma^2))
#' validate_parameters(fam, theta = -1, .error = FALSE)
#' @export
validate_parameters <- function(distribution, ..., .error = TRUE) {
  new_parameters <- update_parameters_list(distribution, ...)
  validate_parameters_bare(
    new_parameters, distribution$parspace, .error = .error
  )
}

#' Validate parameters, bare function
#'
#' This internal function is the workhorse behind checking parameter
#' values against the parameter space. It expects a parameter list
#' to be provided to it.
#' @param parameters List of parameters, like the output of `parameters()`.
#' @inheritParams validate_parameters
#' @returns The same as the user-facing `validate_parameters()` function.
validate_parameters_bare(distribution, parameters, .error = TRUE) {
  bottom <- rlang::as_environment(parameters, parent = distribution$bottom)
  dmask <- new_distribution_mask(bottom = bottom, top = distribution$top)
  parspace <- distribution$parspace
  for (i in seq_along(parspace)) {
    passes <- rlang::eval_tidy(parspace[[i]], data = dmask)
    if (length(passes) == 0) passes <- TRUE # NULL in the rule.
    if (!passes) {
      if (.error) {
        stop(
          "Parameter value is invalid. Rule broken: ",
          rlang::expr_text(parspace[[i]])
        )
      } else {
        return(FALSE)
      }
    }
  }
  if (.error) {
    invisible(distribution)
  } else {
    TRUE
  }
}

#' Parameter tracking in a distribution family
#'
#' @param ... Specification of parameters and parameter space. See
#' details.
#' @details
#' The `...` is parsed as follows:
#'
#' - Bare expressions are stored in the parameter space.
#'   For example, `sigma > 0`.
#' - Named bindings are assumed to be parameters.
#'   For example, `mu = 0` sets `mu` as a parameter with the known value 0.
#' - Bare symbols imply that these symbols should be tracked as parameters,
#'   and have lower priority compared to named bindings. For example,
#'   `mu, mu = 0, sigma` is the same as `mu = 0, sigma`.
#'
#' Bare expressions representing the parameter space are stored as quosures
#' and evaluated in the distribution data mask. This means that you can
#' reference distributional representations / properties.
#' @returns A list with two components:
#'
#' - `$parameters`: A named list, whose names are the parameter names,
#'    and values are the parameter values (`NULL` if not yet resolved).
#' - `$parspace`: An unnamed list of rules making up the parameter space.
#'    these rules are quosures, which pair the input expressions with
#'    the environment in which they were created.
#' @examples
#' params(mu = 5, sigma, sigma > 0)
#'
#' # ...is the same as
#' params(mu, mu = 5, sigma, sigma > 0)
#'
#' # This function is intended to be used when specifying a distributon
#' # family.
#' my_fam <- distribution(
#'   cdf = \(x) stats::pnorm(x, mu, sigma),
#'   .parameters = params(mu, sigma, sigma > 0)
#' )
#'
#' # The parameter space is evaluated in the distribution data mask,
#' so distribution properties can also be referenced.
#' params(alpha, beta, alpha > 0, beta > 0, .dst$mean < 0.5)
#'
#' # Parameters need not be numeric.
#' params(rho = diag(3), person, is.character(person))
#' @export
params <- function(...) {
  dots <- rlang::enexprs(...)
  nms <- names(dots)
  is_binding <- nms != ""
  is_sym <- vapply(dots, rlang::is_symbol, FUN.VALUE = logical(1))
  is_to_track <- !is_binding & is_sym
  is_bare_expr <- !is_binding & !is_sym
  parspace <- dots[is_bare_expr]
  dots_w_names <- rlang::quos_auto_name(dots)
  all_param_names <- unique(names(dots_w_names[is_binding | is_to_track]))
  parameters <- rep(list(NULL), length(all_param_names))
  names(parameters) <- all_param_names
  bindings <- lapply(dots[is_binding], rlang::eval_tidy)
  for (i in seq_along(bindings)) {
    nm <- names(bindings)[i]
    parameters[[nm]] <- bindings[[nm]]
  }
  list(
    parameters = parameters,
    parspace = parspace
  )
}

#' Get and set distribution parameters
#'
#' @param distribution A distribution or distribution family object.
#' @seealso [params()] for setting up parameter tracking in a new
#' distribution family.
#' @examples
#' d <- dst_norm(mu = 0)
#' parameters(d)
#' parameters(d)$mu <- 2
#' parameters(d)$sigma <- 3
#' parameters(d)
#' @rdname parameters
#' @export
parameters <- function(distribution) {
  distribution$parameters
}

`parameters<-` <- function(distribution, value) {
  distribution$parameters <- value
  update_dst_class(distribution)
}

#' Add or remove the "dst" class as appropriate
update_dst_class <- function(distribution) UseMethod("update_dst_class")

update_dst_class.dst <- function(distribution) {
  should_be_dst <- parameters_all_resolved(distribution)
  if (!should_be_dst) {
    clss <- class(distribution)
    i <- which(clss == "dst")
    class(distribution) <- clss[(i + 1:length(clss))]
  }
  distribution
}

update_dst_class.fam <- function(distribution) {
  should_be_dst <- parameters_all_resolved(distribution)
  if (should_be_dst) {
    network_env <- generate_network_environment(
      def_env = distribution$definition_env,
      param_list = distribution$params
    )
    distribution$network_env <- network_env
    base_mask <- new_distribution_mask(
      as_environment(distribution$params, parent = network_env),
      top = network_env
    )
    distribution$base_mask <- base_mask
    class(distribution) <- append("dst", class(distribution))
  }
  distribution
}

# ------ parspace ------

parspace <- function(distribution) distribution$parspace

`parspace<-` <- function(distribution, value) {
  distribution$parspace <- value
  distribution
}

constrain_parspace <- function(distribution, ...) {
  constraints <- rlang::enquos(...)
  distribution$parspace <- append(distribution$parspace, constraints)
  distribution
}

# ----- DEMO ------

f <- distribution(
  cdf = \(x) x^2 * lambda,
  density = \(x) foofy * x,
  quantile = \(p) sqrt(p / lambda),
  foofy = lambda * 2
)

d <- resolve_params(f, lambda = 45)
d <- seal_distribution(d)

eval_density(d, at = 5)
eval_density(d, at = .dst$quantile(0.5))
eval_representation(d, "quantile", c(0.5, 1))
# eval_distribution(d, quantile(c(0.5, 1, lambda)))
eval_representation(d, "survival", c(0.5, 1))
eval_distribution(d, lambda)
eval_distribution(d, foofy)

d2 <- resolve_params(f, lambda = 1)
d2 <- seal_distribution(d2)
eval_density(d2, at = 0.4)
eval_density(d2, at = .dst$quantile(0.5))
eval_representation(d2, "quantile", c(0.5, 1, lambda))
eval_distribution(d, lambda)
