#' Bind to a representation environment
#'
#' Evaluates everything in ... within the specified environment if possible
#' and if non-NULL. Expressions in ... are allowed to refer to each other.
#'
#' This function is like building an R package, but tries to allow for
#' the specification of constants. This may not be a good idea, for the
#' same reason that
#' "Any R code outside of a function is suspicious and should be
#' carefully reviewed" (https://r-pkgs.org/code.html#sec-code-when-executed).
#' An alternative may be to only use functions, e.g., `g <- function() 9.81`.
#'
#' @details Evaluation occurs in order of appearance in `...`. To allow
#' earlier quantities to refer to later ones, the evaluate-and-bind
#' procedure is looped until there is nothing new to bind.
env_bind_representations <- function(.env, ...) {
  before <- rlang::enexprs(...)
  rlang::env_bind(.env, !!!before)
  changed <- TRUE
  while (any(changed)) {
    after <- lapply(before, \(x) eval_within_if_possible(x, env = .env))
    changed <- !mapply(identical, before, after, SIMPLIFY = TRUE)
    rlang::env_bind(.env, !!!after[changed])
    before <- after
  }
  invisible(.env)
}

#' Try to evaluate an expression within an environment
#'
#' If an expression evaluates within an environment and is not null,
#' this function returns that value. Otherwise, the original expression
#' is returned. Because the evaluation occurs within the environment,
#' the environment becomes the enclosing environment when an expression
#' evaluates to a function.
#' @note This function could have been `eval_within_if_function()`, so that
#' the successful evaluation would also have to be a function in order to
#' be returned, but that would prevent functions from referencing
#' user-defined constants along the lines of Base R's `pi` and `letters`.
#' At the same time, expressions that rely on parameters will not evaluate
#' for `eval_within_if_possible()` because a data mask is not used.
#' @rdname eval_if_possible
eval_within_if_possible <- function(expr, env) {
  t <- try(eval(expr, envir = env), silent = TRUE)
  if (inherits(t, "try-error") || is.null(t)) {
    return(expr)
  } else {
    return(t)
  }
}

#' @rdname eval_if_possible
eval_tidy_if_possible <- function(expr, ...) {
  t <- try(rlang::eval_tidy(expr, ...), silent = TRUE)
  if (inherits(t, "try-error") || is.null(t)) {
    return(expr)
  } else {
    return(t)
  }
}


#' Evaluate and bind in an environment
#'
#' Like **rlang**'s `env_bind` family of functions, this function also
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
#' env_bind_within(e2, f = \(x) x^2)
#' fn_env(e1$f)
#' fn_env(e2$f)
env_bind_within <- function(.env, ...) { # deprecate?
  dots <- rlang::enexprs(...)
  obs <- lapply(dots, \(x) eval(x, .env))
  rlang::env_bind(.env, !!!obs)
  invisible(.env)
}

env_bind_exprs <- function(.env, ...) { # deprecate?
  dots <- rlang::enexprs(...)
  rlang::env_bind(.env, !!!dots)
  invisible(.env)
}

