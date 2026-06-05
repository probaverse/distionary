# Testing the expectile representation, in both directions:
#   forward  -- expectiles computed from the network (survival + mean),
#   reverse  -- cdf reconstructed from an intrinsic expectile function.

test_that("Analytic expectiles agree with the forward network algorithm", {
  tau <- c(0.05, 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9, 0.95)
  for (d in list(dst_unif(-2, 3), dst_exp(1.5), dst_t(2))) {
    analytic <- eval_expectile(d, tau)
    network <- eval_expectile_from_network(d, tau)
    expect_equal(analytic, network, tolerance = 1e-6)
  }
})

test_that("The 1/2-expectile equals the mean", {
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
      expect_equal(eval_expectile(d, 0.5), m, tolerance = 1e-6)
    }
  }
})

test_that("Expectiles are increasing in tau", {
  tau <- 1:19 / 20
  for (d in list(dst_norm(1, 2), dst_unif(0, 1), dst_exp(2), dst_t(2))) {
    xi <- eval_expectile(d, tau)
    expect_true(all(diff(xi) > 0))
  }
})

test_that("The cdf is recovered from an intrinsic expectile function", {
  cases <- list(
    list(d = dst_unif(0, 1), p = function(x) punif(x)),
    list(d = dst_exp(1), p = function(x) pexp(x)),
    list(d = dst_t(2), p = function(x) pt(x, df = 2))
  )
  for (case in cases) {
    rev_dst <- suppressWarnings(distribution(
      expectile = case$d$expectile, .vtype = "continuous"
    ))
    x <- eval_quantile(case$d, c(0.1, 0.3, 0.5, 0.7, 0.9))
    expect_equal(eval_cdf(rev_dst, x), case$p(x), tolerance = 1e-3)
  }
})

test_that("Forward then reverse round-trips the distribution", {
  base <- dst_norm(0, 1)
  xi <- function(tau) eval_expectile(base, tau)
  rev_dst <- suppressWarnings(distribution(
    expectile = xi, .vtype = "continuous"
  ))
  x <- c(-1.5, -0.5, 0.5, 1.5)
  expect_equal(eval_cdf(rev_dst, x), pnorm(x), tolerance = 1e-3)
  expect_equal(eval_property(rev_dst, "mean"), 0, tolerance = 1e-6)
  expect_equal(
    eval_survival(rev_dst, x), pnorm(x, lower.tail = FALSE),
    tolerance = 1e-3
  )
})

test_that("eval_cdf_from_network returns NULL without an expectile function", {
  expect_null(eval_cdf_from_network(dst_norm(0, 1), at = 0))
})

test_that("Expectiles require a finite mean", {
  expect_error(eval_expectile(dst_cauchy(0, 1), 0.5))
})

test_that("Numerical expectiles refuse non-continuous distributions", {
  expect_error(eval_expectile(dst_pois(2), 0.5), "continuous")
  expect_error(
    eval_expectile(dst_empirical(c(1, 2, 3)), c(0.25, 0.75)), "continuous"
  )
})

test_that("eval_expectile handles edge inputs", {
  d <- dst_norm(0, 1)
  expect_equal(eval_expectile(d, numeric(0)), numeric(0))
  framed <- enframe_expectile(d, at = c(0.25, 0.75))
  expect_s3_class(framed, "data.frame")
  expect_equal(nrow(framed), 2)
})
