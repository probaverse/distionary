# Validate PMF can be recovered from CDF
#
# Systematic testing uses the `test_distributions` object.
# To modify it, see `data-raw/test_distributions.R`.
# To update it, run `Rscript data-raw/test_distributions.R`.


test_that("PMF calculated thru CDF matches known vals.", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "pmf")) {
        expect_false(is_intrinsic(d, "density"))
        p <- 1:99 / 100
        x <- unique(eval_quantile(d, at = p))
        pmf_evald <- eval_cdf(d, at = x) - eval_cdf(d, at = x - 1e-6)
        expect_equal(pmf_evald, eval_pmf(d, at = x))
      }
    }
  }
})


test_that("The strict definition of a pmf needs a discrete variable", {
  expect_equal(
    eval_pmf(dst_pois(5), at = 0:3, definition = "strict"),
    dpois(0:3, 5)
  )
  expect_error(
    eval_pmf(dst_norm(0, 1), at = -2:2, definition = "strict"),
    "no probability mass function in the strict sense"
  )
})

test_that("The extended definition reads a mass off the cdf's jump", {
  # A continuous distribution puts no mass on any point, and says so
  # rather than refusing.
  expect_equal(eval_pmf(dst_norm(0, 1), at = -2:2), rep(0, 5))
  expect_equal(
    eval_pmf(dst_pois(5), at = 0:3, definition = "extended"),
    dpois(0:3, 5)
  )
})

test_that("The definition carries through to `enframe_pmf()`", {
  d <- dst_pois(5)
  expect_equal(
    enframe_pmf(d, at = 0:3, definition = "strict")[["pmf"]],
    eval_pmf(d, at = 0:3)
  )
})
