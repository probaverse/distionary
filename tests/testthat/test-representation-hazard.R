#' Test that the hazard function is correct by comparing it to the limit of
#' the probability of an event happening immediately, given that it hasn't
#' happened already, using the density function.
test_that("Hazard function is validated using the density function.", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (vtype(d) == "continuous") {
        p <- 1:9 / 10
        x <- eval_quantile(d, at = p)
        r <- range(d)
        supposed_hazard <- eval_hazard(d, at = x)
        expect_equal(
          eval_density(d, at = x) / eval_survival(d, at = x),
          supposed_hazard
        )
      }
    }
  }
})
