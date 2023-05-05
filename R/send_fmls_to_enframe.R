#' Send arguments to an `enframe_` function
#'
#' When used within a (encapsulating) function, `send_fmls_to_enframe()`
#' sends the inputs of the encapsulating function to either a univariate,
#' bivariate, or multivariate enframer.
#'
#' @param variate One of `"uni"`, `"bi"`, or `"multi"`.
#' @param repres String; suffix of an `eval_` function, specifying the
#' distributional representation to be evaluated; e.g., `"cdf"`.
#' @return Either the `enframe_uni()`, `enframe_bi()`, or `enframe_multi()`
#' function (depending on the `variate` input), evaluated at the
#' encapsulating function's arguments, with the proper `eval_fn` argument
#' also passed.
send_fmls_to_enframe <- function(variate, repres) {
  args_syms <- rlang::fn_fmls_syms(fn = rlang::caller_fn(n = 1))
  ellipsis_position <- names(args_syms) == ""
  args_syms[[ellipsis_position]] <- rlang::expr(list(...))
  enframe_fn <- paste0("enframe_", variate)
  eval_fn <- paste0(variate, "_", eval_fn)
  enframe_call <- rlang::call2(enframe_fn, !!!args_syms, eval_fn = eval_fn)
  rlang::eval_tidy(enframe_call, env = caller_env(n = 1))
}
