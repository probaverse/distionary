#' Evaluate and bind in an environment
#'
#' Like *rlang*'s `env_bind` family of functions, this function also
#' takes named values and binds them to the specified environment, but also
#' evaluates the expressions in the environment. This has implications when
#' defining functions, because the encapsulating environment becomes the
#' input environment, not the caller environment.
#'
#' @param .env An environment.
#' @param ... Named objects to evaluate and bind in `.env`.
#' @returns The updated version of the environment `.env`, invisibly.
#' @examples
#' library(rlang)
#' e1 <- env()
#' e2 <- env()
#' env_bind(e1, f = \(x) x^2)
#' env_eval_and_bind(e2, f = \(x) x^2)
#' fn_env(e1$f)
#' fn_env(e2$f)
env_eval_and_bind <- function(.env, ...) {
  dots <- rlang::enexprs(...)
  obs <- lapply(dots, \(x) eval(x, .env))
  rlang::env_bind(.env, !!!obs)
  invisible(.env)
}
