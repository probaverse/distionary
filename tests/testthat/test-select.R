g <- distribution(
  density = function(x, y) stats::dnorm(x) * stats::dexp(y),
  cdf = function(x, y) stats::pnorm(x) * stats::pexp(y),
  .support = support_product(
    rainfall = continuous(),
    runoff = continuous(c(0, Inf))
  )
)

test_that("tidyselect selections pick and order variables", {
  skip_if_not_installed("tidyselect")
  expect_identical(
    variables(marginal(g, c(runoff, tidyselect::everything()))),
    c("runoff", "rainfall")
  )
  expect_identical(variables(marginal(g, runoff)), "runoff")
  expect_equal(eval_bi_cdf(g, 0.3, 1, given = rainfall), stats::pexp(1))
  # In eval_bi_*(), `x` is also the first argument.
  expect_equal(eval_bi_cdf(g, 0.3, 1, given = x), stats::pexp(1))
  m <- dst_mv_norm(c(gauge1 = 0, gauge2 = 0, flow = 0), diag(3))
  expect_equal(
    eval_mv_cdf(m, list(1, 1, 0), given = tidyselect::starts_with("gauge")),
    0.5
  )
  expect_error(marginal(g, nothere))
})

test_that("strings and positions work without tidyselect", {
  local_mocked_bindings(has_tidyselect = function() FALSE)
  expect_identical(variables(marginal(g, "runoff")), "runoff")
  expect_identical(variables(marginal(g, 2:1)), c("runoff", "rainfall"))
  expect_equal(eval_bi_cdf(g, 0.3, 1, given = "x"), stats::pexp(1))
  expect_error(marginal(g, runoff), "tidyselect")
})

test_that("the argument aliases are reachable only by name", {
  skip_if_not_installed("tidyselect")
  expect_error(eval_bi_cdf(g, 0.3, 1, given = 3))
  expect_equal(eval_bi_cdf(g, 0.3, 1, given = "x"), stats::pexp(1))
})
