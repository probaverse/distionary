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
# inject_mask_into_function <- function(args, body, env, bottom, top, wrapping_env = caller_env()) {
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
  res <- list(
    definition_env = definition_env,
    params = .params
  )
  class(res) <- "fam"
  res
}

#' Parameters are resolved; inherit dst, and generate network env.
seal_distribution <- function(distribution) {
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
  distribution
}

resolve_params <- function(distribution, ...) {
  dots <- list(...)
  distribution$params <- c(distribution$params, dots)
  distribution
}

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
eval_distribution(d, lambda)
eval_distribution(d, foofy)

d2 <- resolve_params(f, lambda = 1)
d2 <- seal_distribution(d2)
eval_density(d2, at = 0.4)
eval_density(d2, at = .dst$quantile(0.5))
eval_representation(d2, "quantile", c(0.5, 1, lambda))
eval_distribution(d, lambda)
