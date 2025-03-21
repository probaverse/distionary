

# --- TEST BLOCK ----
# Some representations are never given for distribution families in
# distionary. For example, no distribution has a hazard function defined.
# Check that the representation is valid based on its definition.
test_that("Representations that are never specified in distionary work.", {
  ## Hazard
  d <- dst_norm(0, 1)
  x <- seq(-4, 4, length.out = 100)
  h1 <- eval_hazard(d, at = x)
  h2 <- dnorm(x) / pnorm(x, lower.tail = FALSE)
  expect_equal(h1, h2)
  ## Hazard on non-continuous distribution
  d <- dst_pois(1)
  expect_error(eval_hazard(d, at = 1))
  ## Odds
  d <- dst_nbinom(10, 0.4)
  p <- eval_pmf(d, at = 0:10)
  o1 <- p / (1 - p)
  o2 <- eval_odds(d, 0:10)
  expect_equal(o1, o2)
  ## Cumulative hazard function
  d <- dst_weibull(3, 2)
  x <- 0:10
  chf1 <- eval_chf(d, at = x)
  haz <- \(t) eval_hazard(d, at = t)
  chf2 <- vapply(
    x, \(x_) integrate(haz, lower = 0, upper = x_)$value,
    FUN.VALUE = numeric(1)
  )
  expect_equal(chf1, chf2)
  ## Cumulative hazard on non-continuous distribution
  d <- dst_pois(1)
  expect_error(eval_chf(d, at = 1))
  ## Return level, continuous
  d <- dst_exp(0.2)
  rp <- c(1, 2, 5, 10, 50, 100)
  l1 <- eval_return(d, at = rp)
  l2 <- eval_quantile(d, at = 1 - 1 / rp)
  expect_equal(l1, l2)
  ## Return level, discrete
  d <- dst_hyper(10, 5, 8)
  rp <- c(1, 2, 5, 10, 50, 100)
  l1 <- eval_return(d, at = rp)
  l2 <- eval_quantile(d, at = 1 - 1 / rp)
  expect_equal(l1, l2)
  ## Median, discrete
  d <- dst_binom(10, 0.3)
  m1 <- median(d)
  m2 <- eval_quantile(d, 0.5)
  expect_equal(m1, m2)
})
