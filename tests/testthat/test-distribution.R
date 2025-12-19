#' @srrstats {G5.2} Appropriate error behaviour is tested for all
#' functions explicitly, but warnings are omitted and saved for a future
#' version. --> Copied to test-edge_cases-builtin_dists.R

test_that("Object is a distribution.", {
  expect_true(is_distribution(suppressWarnings(distribution())))
})

test_that("distribution() edge cases satisfied.", {
  suppressWarnings({
    expect_true(is_distribution(distribution()))
    expect_error(distribution(1:10))
    expect_error(distribution(.parameters = "foofy"))
    expect_error(distribution(.parameters = list("foofy")))
    expect_error(distribution(.parameters = c(alpha = 4)))
    expect_error(distribution(.name = c("my", "name", "is")))
    expect_error(distribution(.name = character(0)))
    expect_error(distribution(.vtype = c("my", "name", "is")))
    expect_error(distribution(.vtype = character(0)))
  })
})

test_that("Typo warning works: vtype", {
  expect_warning(
    distribution(cdf = pnorm, density = dnorm, .vtype = "discreet")
  )
  expect_warning(
    distribution(cdf = pnorm, density = dnorm, .vtype = "continus")
  )
  expect_warning(
    distribution(cdf = pnorm, density = dnorm, .vtype = "mixedd")
  )
  expect_warning(
    distribution(cdf = pnorm, density = dnorm, .vtype = "categorcal")
  )
  expect_warning(
    distribution(cdf = pnorm, density = dnorm, .vtype = "ordinale")
  )
  expect_no_warning(
    distribution(cdf = pnorm, density = dnorm, .vtype = "discrete")
  )
  expect_no_warning(
    distribution(cdf = pnorm, density = dnorm, .vtype = "my_type")
  )
})


test_that("Warning when cdf and pmf/density missing.", {
  expect_warning(
    distribution()
  )
  expect_warning(
    distribution(pmf = dpois, quantile = qpois)
  )
  expect_warning(
    distribution(density = dnorm, quantile = qnorm)
  )
  expect_no_warning(
    distribution(cdf = pnorm, density = dnorm)
  )
  expect_no_warning(
    distribution(cdf = pnorm, pmf = dpois)
  )
})
