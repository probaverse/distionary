distribution3 <- function(
    ..., parameters = list()
) {
  definitions <- rlang::enquos(...)
  # mask <- rlang::new_data_mask(dst, top = repres)
  # mask$.repres <- rlang::as_data_pronoun(repres)
  # mask$.dst <- rlang::as_data_pronoun(mask)
  nms <- names(definitions)
  representations <- c(
    "cdf", "survival", "density", "pmf", "quantile", "return", "odds",
    "hazard", "chf"
  )
  def_anchors <- definitions[nms %in% representations]
  def_others <- definitions[!nms %in% representations]
  fn_env <- rlang::env()
  param_env <- rlang::new_environment(parent = fn_env)
  mask <- rlang::new_data_mask(param_env, fn_env)
  mask$.dst <- rlang::as_data_pronoun(mask)
  mask$.repres <- rlang::as_data_pronoun(fn_env)
  for (i in seq_along(def_anchors)) {
    nm <- names(def_anchors)[i]
    fn <- rlang::eval_tidy(def_anchors[[i]]) # Becomes a function.
    ## Reconfigure functions to evaluate in distribution data mask.
    bod <- body(fn)
    fn_env <- environment(fn)
    fmls <- formals(fn)
    rlang::env_bind(
      fn_env,
      !!nm := rlang::new_function(
        args = fmls,
        body = rlang::expr({
          rlang::eval_tidy(rlang::expr(!!bod), !!mask, env = !!fn_env)
        }),
        env = param_env
      )
    )
    # fn_env[[nm]] <- rlang::new_function(
    #   args = fmls,
    #   body = rlang::expr({
    #     rlang::eval_tidy(rlang::expr(!!bod), mask, env = fn_env)
    #   }),
    #   env = param_env
    # )
  }
  if (!"survival" %in% nms) {
    fn_env$survival <- rlang::new_function(
      args = rlang::pairlist2(x =),
      body = rlang::expr({
        rlang::eval_tidy(rlang::expr(1 - cdf(x)), mask, env = fn_env)
      }),
      env = param_env
    )
  }
  if (!"quantile" %in% nms) {
    fn_env$quantile <- rlang::new_function(
      args = rlang::pairlist2(x =),
      body = rlang::expr({
        rlang::eval_tidy(
          rlang::expr(
            cat("Still working on it!")
            # eval_quantile_from_cdf(
            #   distribution, at = at, tol = 1e-09, maxiter = 200
            # )
          ),
          mask,
          env = fn_env
        )
      }),
      env = param_env
    )
  }
  if (!"return" %in% nms) {
    fn_env$return <- rlang::new_function(
      args = rlang::pairlist2(x =),
      body = rlang::expr({
        rlang::eval_tidy(
          rlang::expr(quantile(1 - 1 / x)), mask, env = fn_env
        )
      }),
      env = param_env
    )
  }
  if (!"odds" %in% nms) {
    fn_env$odds <- rlang::new_function(
      args = rlang::pairlist2(x =),
      body = rlang::expr({
        rlang::eval_tidy(
          rlang::expr({
            p <- pmf(at)
            p / (1 - p)
          }),
          mask, env = fn_env
        )
      }),
      env = param_env
    )
  }
  if (!"hazard" %in% nms) {
    fn_env$hazard <- rlang::new_function(
      args = rlang::pairlist2(x =),
      body = rlang::expr({
        rlang::eval_tidy(
          rlang::expr({
            sf <- survival(x)
            pdf <- density(x)
            pdf / sf
          }),
          mask, env = fn_env
        )
      }),
      env = param_env
    )
  }
  if (!"chf" %in% nms) {
    fn_env$chf <- rlang::new_function(
      args = rlang::pairlist2(x =),
      body = rlang::expr({
        rlang::eval_tidy(
          rlang::expr(-log(survival(x))), mask, env = fn_env
        )
      }),
      env = param_env
    )
  }
  structure(
    list(
      definitions = list(anchors = def_anchors, others = def_others),
      mask = mask,
      param_env = param_env,
      fn_env = fn_env
    ),
    class = "fam"
  )
}

resolve_params3 <- function(distribution, ...) {
  parameters <- list(...)
  rlang::env_bind(distribution$param_env, !!!parameters)
  def_others <- distribution$definitions$others
  ## Resolve distribution definitions
  for (i in seq_along(def_others)) {
    nm <- names(def_others)[i]
    rlang::env_bind(
      distribution$fn_env,
      !!nm := rlang::eval_tidy(def_others[i], distribution$mask)
    )
  }
  class(distribution) <- append("dst", class(distribution))
  distribution
}

eval_density3 <- function(distribution, at) {
  eval_representation3(distribution, at, repres = "density")
}

eval_representation3 <- function(distribution, at, repres) {
  at <- rlang::enquo(at)
  at <- rlang::eval_tidy(at, distribution$mask)
  distribution$fn_env[[repres]](at)
}

d <- distribution3(
  density = function(x) lambda * x^2,
  cdf = function(x) lambda * x^3 / 3
)

d <- resolve_params3(d, lambda = 5)

eval_density3(d, at = 1:10)
eval_representation3(d, at = 1:10, repres = "cdf")
