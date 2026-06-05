# Testing the conditional tail expectation representation, in both directions:
#   forward  -- CTE computed from the network (survival + mean),
#   reverse  -- cdf reconstructed from an intrinsic CTE function via
#               mean-residual-life inversion.

test_that("Forward CTE matches closed forms", {
  x <- c(-1, 0, 0.5, 1, 2, 3)
  # Exponential(1) is memoryless: E[X | X > x] = x + 1 for x >= 0, else mean 1.
  expect_equal(
    eval_cte(dst_exp(1), x), ifelse(x < 0, 1, x + 1),
    tolerance = 1e-6
  )
  # Standard normal: E[X | X > x] = dnorm(x) / pnorm(x, lower.tail = FALSE).
  expect_equal(
    eval_cte(dst_norm(0, 1), x),
    dnorm(x) / pnorm(x, lower.tail = FALSE),
    tolerance = 1e-6
  )
})

test_that("CTE below the support equals the mean, and at/above the max is NaN", {
  d <- dst_unif(0, 1)
  expect_equal(eval_cte(d, c(-3, -1)), c(0.5, 0.5), tolerance = 1e-9)
  expect_true(all(is.nan(eval_cte(d, c(1, 2)))))
})

test_that("CTE is at least the mean and is non-decreasing", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (vtype(d) != "continuous") {
        next
      }
      m <- eval_property(d, "mean")
      if (is.null(m) || !is.finite(m)) {
        next
      }
      x <- eval_quantile(d, c(0.1, 0.3, 0.5, 0.7, 0.9))
      cte <- eval_cte(d, x)
      expect_true(all(cte >= m - 1e-6))
      expect_true(all(cte >= x - 1e-6))
      expect_true(all(diff(cte) >= -1e-6))
    }
  }
})

test_that("The cdf is recovered from an intrinsic CTE function", {
  cases <- list(
    list(cte = function(x) ifelse(x < 0, 1, x + 1), p = function(x) pexp(x)),
    list(
      cte = function(x) dnorm(x) / pnorm(x, lower.tail = FALSE),
      p = function(x) pnorm(x)
    ),
    list(
      cte = function(x) ifelse(x < 0, 1, (pmin(x, 2) + 2) / 2),
      p = function(x) punif(x, 0, 2)
    )
  )
  for (case in cases) {
    rev_dst <- suppressWarnings(distribution(
      cte = case$cte, .vtype = "continuous"
    ))
    x <- c(0.2, 0.5, 1, 1.5)
    expect_equal(eval_cdf(rev_dst, x), case$p(x), tolerance = 1e-3)
  }
})

test_that("Forward then reverse round-trips the distribution", {
  base <- dst_gamma(shape = 2, rate = 1)
  cte_fun <- function(x) eval_cte(base, x)
  rev_dst <- suppressWarnings(distribution(
    cte = cte_fun, .vtype = "continuous"
  ))
  x <- c(0.5, 1, 2, 4)
  expect_equal(eval_cdf(rev_dst, x), eval_cdf(base, x), tolerance = 1e-3)
  expect_equal(eval_property(rev_dst, "mean"), 2, tolerance = 1e-4)
})

test_that("eval_cdf_from_network returns NULL without expectile or cte", {
  expect_null(eval_cdf_from_network(dst_norm(0, 1), at = 0))
})

test_that("CTE requires a finite mean", {
  expect_error(eval_cte(dst_cauchy(0, 1), 0))
})

test_that("Numerical CTE refuses non-continuous distributions", {
  expect_error(eval_cte(dst_pois(2), 1), "continuous")
  expect_error(eval_cte(dst_empirical(c(1, 2, 3)), 1), "continuous")
})

test_that("eval_cte handles edge inputs", {
  d <- dst_norm(0, 1)
  expect_equal(eval_cte(d, numeric(0)), numeric(0))
  framed <- enframe_cte(d, at = c(0, 1))
  expect_s3_class(framed, "data.frame")
  expect_equal(nrow(framed), 2)
})
