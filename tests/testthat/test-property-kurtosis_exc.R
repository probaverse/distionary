#' @srrstats {G5.4} Correctness tests are conducted to test that statistical
#' algorithms produce expected results.
#' @srrstats {G5.4b} Implementations of existing methods are compared against
#' the stats package where possible.
test_that("Network is invoked in priority: kurtosis_exc", {
  # First look for `kurtosis`, then invoke algorithm if not found.
  d <- suppressWarnings(distribution(
    density = stats::dnorm,
    range = c(-Inf, Inf),
    kurtosis = 100, # deliberately incorrect
    .vtype = "continuous"
  ))
  expect_equal(kurtosis_exc(d), 100 - 3)
  expect_equal(eval_kurtosis_exc_from_network(d), 100 - 3)
  d$kurtosis <- NULL
  expect_equal(kurtosis_exc(d), 0)
  expect_equal(eval_kurtosis_exc_from_network(d), 0)
})

test_that("Excess kurtosis matches kurtosis.", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "kurtosis_exc") || is_intrinsic(d, "kurtosis")) {
        expect_equal(kurtosis(d) - 3, kurtosis_exc(d))
      }
    }
  }
})
