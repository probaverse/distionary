#' @srrstats {G5.9} Noise susceptibility tests have been conducted on
#' distribution parameters and evaluation inputs.
#' @srrstats {G5.9a} Machine tolerance has been added to distribution
#' parameters and evaluation inputs and compared to originals.
test_that("Distributions are not sensitive to machine tolerance.", {
  for (item in test_distributions) {
    paramset_orig <- item$valid[[1]]
    eps <- .Machine$double.eps
    paramset_eps <- lapply(paramset_orig, \(x) x - eps)
    d_orig <- rlang::exec(item$distribution, !!!paramset_orig)
    d_eps <- rlang::exec(item$distribution, !!!paramset_eps)
    p <- 0.4
    ## Quantile. Also use x for downstream tests.
    x <- eval_quantile(d_orig, at = p)
    expect_equal(x, eval_quantile(d_eps, p + eps))
    ## CDF
    expect_equal(
      eval_cdf(d_orig, x),
      eval_cdf(d_eps, x + eps)
    )
    ## Survival
    expect_equal(
      eval_survival(d_orig, x),
      eval_survival(d_eps, x + eps)
    )
    ## Mass
    if (attr(d_orig, "name") != "Degenerate") {
      # Degenerate distribution's x would also need to increase by eps,
      # which is not meaningful to test.
      expect_equal(
        eval_pmf(d_orig, x),
        eval_pmf(d_eps, x)
      )
    }
    ## Density
    if (vtype(d_orig) == "continuous") {
      expect_equal(
        eval_density(d_orig, x),
        eval_density(d_eps, x + eps)
      )
    }
    ## Mean
    expect_equal(
      mean(d_orig),
      mean(d_eps)
    )
    ## Variance
    expect_equal(
      variance(d_orig),
      variance(d_eps)
    )
    ## Standard Deviation
    expect_equal(
      stdev(d_orig),
      stdev(d_eps)
    )
    ## Skewness
    if (pretty_name(d_orig) == "Log Pearson Type III") {
      expect_equal(
        suppressMessages(skewness(d_orig)),
        suppressMessages(skewness(d_eps))
      )
    } else {
      expect_equal(
        skewness(d_orig),
        skewness(d_eps)
      )
    }
    ## Kurtosis
    if (pretty_name(d_orig) == "Log Pearson Type III") {
      expect_equal(
        suppressMessages(kurtosis(d_orig)),
        suppressMessages(kurtosis(d_eps))
      )
    } else {
      expect_equal(
        kurtosis(d_orig),
        kurtosis(d_eps)
      )
    }
    ## Excess Kurtosis
    if (pretty_name(d_orig) == "Log Pearson Type III") {
      expect_equal(
        suppressMessages(kurtosis_exc(d_orig)),
        suppressMessages(kurtosis_exc(d_eps))
      )
    } else {
      expect_equal(
        kurtosis_exc(d_orig),
        kurtosis_exc(d_eps)
      )
    }
  }
})
