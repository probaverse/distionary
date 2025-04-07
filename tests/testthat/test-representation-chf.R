#' CHF is never intrinsic, which means there are no known values to compare
#' against. Compare built-in algorithm with another algorithm for deriving
#' CHF: integrating the hazard.
#' This also checks that the hazard function is correct.
test_that("CHF algorithm validated by comparing to a different algorithm.", {
  # - `eval_chf()` goes through the survival function only.
  # - integrating the hazard goes through both the density and survival.
  # i <- 0
  for (item in test_distributions) {
    # i <- i + 1
    # cat("\n---- ", i, "\n")
    # j <- 0
    for (paramset in item$valid) {
      # j <- j + 1
      # cat(j, " ")
      d <- rlang::exec(item$distribution, !!!paramset)
      if (vtype(d) == "continuous") {
        p <- 1:9 / 10
        x <- eval_quantile(d, at = p)
        haz_fun <- representation_as_function(d, "hazard")
        increments <- vapply(
          seq_along(x[-1L]),
          function(t) {
            cubature::hcubature(haz_fun, x[1], x[t + 1])$integral
          },
          FUN.VALUE = numeric(1L)
        )
        chf_derived <- eval_chf(d, at = x[1]) + append(0, (increments))
        expect_equal(chf_derived, eval_chf(d, at = x))
      }
    }
  }
})
