test_that("Distribution setting", {
  # 1. Explicitly specify.
  my_dst <- distribution2(.density = \(x) x^2)
  eval_density2(my_dst, at = 1:10)
  # 2. Through a package
  my_dst <- distribution2(.density = extRemes::pevd)
  eval_density2(my_dst, at = 1:10)
  extRemes::pevd(1:10)
  # 3. Homemade functions. This is like prompting the user to make their own
  # R package by curating their own environment.
  my_dst <- distribution2(
    a = function(x, y) x + y,
    b = function(x, y) x - y,
    dfoo = function(x) a(x, 2 * x) / b(3 * x, 1),
    pfoo = function(x) a(x, x),
    .density = \(x) pfoo(x) * dfoo(x),
    .vtype = "continuous"
  )
  eval_density2(my_dst, at = 1:10)
  # 4. As a developer of distionary, I want to be able to use this infrastructure,
  # too.
  dst_norm2_fam <- distribution2(
    .density = function(x) stats::dnorm(x, mean = mu, sd = sd),
    .parameters = c("mu", "sd")
  )
  dst_norm2 <- function(mu = NULL, sd = NULL) {
    set_params(dst_norm2_fam, mu = 3, sd = 1)
  }
  my_dst <- set_params(dst_norm2_fam, mu = 3, sd = 1)
  eval_density2(my_dst, at = 0:6)
  # 5. I've estimated the geomean of a GEV for many gauges using machine
  #    learning; I now want to estimate the remaining parameters by MLE.
  #    Start with the easier option: where the geomean is a parameter in place
  #    of loc.
  library(tidyverse)
  set.seed(3)
  gaugedat <- tibble(
    gauge = 1:6,
    loc = c(50, 30, 100, 40, 20, 50),
    nobs = c(15, 40, 30, 50, 34, 23)
  ) |>
    mutate(x = map2(
      nobs, loc, \(n_, loc_) rlnorm(n_, meanlog = log(loc_))
    ))
  gaugedat |>
    mutate(
      model = map(loc, \(loc_) dst_norm2(mu = loc_)),
      #fit = map2(model, x, \(model_, x_) fit_dst(model_, x_, method = "mle"))
    )
  ## Which is better for refining a distribution?
  dst_norm2(mu = 5)
  map(mu_hat, \(mu_) distribution2(dst_norm2, mu = mu_))
  # 5. The whole point of using environments is to prevent duplicity. I want to
  # fit many foo distributions to each observation, and I have many.

})
