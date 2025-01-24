library(rlang)

#' Takes a quo and parses it into the components of a function
#' (body, arguments, environment); if just a quosure, arguments
#' are an empty pairlist (which is identical to NULL).
deparse_quo_as_function <- function(quo) {
  t <- try(rlang::eval_tidy(quo), silent = TRUE)
  if (is.function(t)) {
    return(t)
  } else {
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





generate_network_environment <- function(def_env, param_list) {
  nm <- names(def_env)
  net <- standard_network()
  missed <- setdiff(net, nm)
  network_env <- rlang::new_environment()
  param_env <- rlang::as_environment(as.list(param_list), parent = network_env)
  for (i in seq_along(nm)) {
    fun <- rlang::parse_expr(nm[i])
    args <- def_env[[nm[i]]]$args
    calc_instructions <- def_env[[nm[i]]]$body
    calc_fallback_env <- def_env[[nm[i]]]$env
    higher_function <- inject_mask_into_function(
      args = args, body = calc_instructions, env = calc_fallback_env,
      bottom = param_env, top = network_env, wrapping_env = def_env
    )
    rlang::env_bind_active(
      network_env,
      !!nm[i] := higher_function
    )
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
    rlang::env_bind_active(
      network_env,
      survival = rlang::new_function(
        args = args,
        body = masked_body,
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
  rlang::eval_tidy(rlang::call2(repres, at), distribution$network_env)
}

distribution <- function(..., .params = rlang::pairlist2()) {
  definition_env <- rlang::new_environment()
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

  distribution$network_env <- network_env
  class(distribution) <- append("dst", class(distribution))
  distribution
}

resolve_params <- function(distribution, ...) {
  dots <- list(...)
  distribution$params <- c(distribution$params, dots)
  distribution
}

f <- distribution(cdf = \(x) x^2 * lambda, density = \(x) 2 * x * lambda, mu = lambda / 2)

d <- resolve_params(f, lambda = 4)
d <- seal_distribution(d)
