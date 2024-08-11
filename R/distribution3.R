#' @export
distribution3 <- function(..., #.params = params(),
                          .vtype = c("continuous", "discrete", "mixed"),
                          parenv = rlang::caller_env()) {
  ell <- rlang::enquos(...)
  e <- rlang::new_environment(!!!ell, parent = parenv)
  # d <- rlang::new_data_mask(bottom = e, top = globalenv())
  attr(e, "variable") <- rlang::arg_match(.vtype)
  # attr(e, "ppties") <- e
  attr(e, "class") <- "dst"
  e
  # l$mask <- rlang::as_data_mask(list())
  # l$env <- rlang::new_environment(parent = rlang::caller_env())
  # new_distribution(l, variable = "continuous")
}

set_density3 <- function(distribution, fun) {
  # Eval the function so that it encloses the distribution's env
  f <- eval(rlang::enexpr(fun), envir = distribution$env)
  rlang::env_bind(distribution$env, .density = f)
}

eval_density3 <- function(distribution, at) {
  # cll <- rlang::call2("density", at)
  rlang::exec(".density", at, .env = distribution)
}
