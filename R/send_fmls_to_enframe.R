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
  strict_arg <- args_syms["strict"]
  args_syms[["strict"]] <- NULL
  ellipsis_position <- names(args_syms) == ""
  args_syms[[which(ellipsis_position)]] <- rlang::expr(list(...))
  enframe_fn <- paste0("enframe_", variate)
  eval_fn <- paste0("eval_multi_", repres)
  if (repres %in% c("density", "pmf")) {
    enframe_call <- rlang::call2(enframe_fn, !!!args_syms, eval_fn = eval_fn,
                                 fn_args = strict_arg)
  } else {
    enframe_call <- rlang::call2(enframe_fn, !!!args_syms, eval_fn = eval_fn,
                                 fn_args = list())
  }
  rlang::eval_tidy(enframe_call, env = rlang::caller_env(n = 1))
}
