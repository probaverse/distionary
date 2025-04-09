#' @srrstats {G5.4} Correctness tests are conducted to test that statistical 
#' algorithms produce expected results.
#' @srrstats {G5.4b} Implementations of existing methods are compared against 
#' the stats package where possible.

test_that("Network is invoked in priority: stdev", {
  # First look for `variance`, then invoke algorithm if not found.
  d <- distribution(
    density = \(x) stats::dnorm(x, sd = 3),
    range = c(-Inf, Inf),
    variance = 100, # deliberately incorrect
    .vtype = "continuous"
  )
  expect_equal(stdev(d), 10)
  expect_equal(eval_stdev_from_network(d), 10)
  d$variance <- NULL
  expect_equal(stdev(d), 3)
  expect_equal(eval_stdev_from_network(d), 3)
})

test_that("stdev matches variance.", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "variance") || is_intrinsic(d, "stdev")) {
        expect_equal(sqrt(variance(d)), stdev(d))
      }
    }
  }
})
