#' @srrstats {G5.8} Edge conditions are tested when evaluating
#' representations.

# Make sure that defined distributions evaluate NA inputs properly.
# Make sure they evaluate edge cases properly.
test_that("`eval_()` functions handle edge cases properly", {
  for (item in test_distributions) {
    d <- rlang::exec(item$distribution, !!!item$valid[[1]])
    r <- range(d)
    eps <- .Machine$double.eps
    p <- c(0.4, NA_real_)
    ## Quantile. Also use x for downstream tests.
    x <- eval_quantile(d, at = p)
    expect_true(is.numeric(x[1]))
    expect_true(is.na(x[2]))
    expect_length(eval_quantile(d, at = numeric()), 0)
    expect_equal(eval_quantile(d, at = 0:1), r)
    ## CDF
    y <- eval_cdf(d, at = x)
    expect_true(is.numeric(y[1]))
    expect_true(is.na(y[2]))
    expect_length(eval_cdf(d, at = numeric()), 0)
    if (attr(d, "name") == "Hypergeometric") {
      # The hypergeometric distribution from the stats package
      # needs to move about 1e-7 units to the left of the minimum value
      # in order to register a drop of the CDF to 0. This will be
      # deemed acceptable since it's acceptable in the stats package,
      # although may be up for consideration in a future version of
      # distionary.
      expect_equal(
        eval_cdf(d, at = c(-Inf, r[1] - 1e-6, r[2], Inf)),
        c(0, 0, 1, 1)
      )
    } else {
      expect_equal(
        eval_cdf(d, at = c(-Inf, r[1] - eps, r[2], Inf)),
        c(0, 0, 1, 1)
      )
    }
    ## Survival
    y <- eval_survival(d, at = x)
    expect_true(is.numeric(y[1]))
    expect_true(is.na(y[2]))
    expect_length(eval_survival(d, at = numeric()), 0)
    if (attr(d, "name") == "Hypergeometric") {
      expect_equal(
        eval_survival(d, at = c(-Inf, r[1] - 1e-6, r[2], Inf)),
        c(1, 1, 0, 0)
      )
    } else {
      expect_equal(
        eval_survival(d, at = c(-Inf, r[1] - eps, r[2], Inf)),
        c(1, 1, 0, 0)
      )
    }
    ## Mass
    y <- eval_pmf(d, at = x)
    expect_true(is.numeric(y[1]))
    expect_true(is.na(y[2]))
    expect_length(eval_pmf(d, at = numeric()), 0)
    expect_equal(
      eval_pmf(d, at = c(-Inf, r[1] - 1, r[2] + 1, Inf)),
      rep(0, 4)
    )
    ## Density
    if (vtype(d) == "continuous") {
      y <- eval_density(d, at = x)
      expect_true(is.numeric(y[1]))
      expect_true(is.na(y[2]))
      expect_length(eval_density(d, at = numeric()), 0)
      dens <- eval_density(d, at = c(-Inf, Inf))
      expect_equal(dens, c(0, 0))
      # expect_true(all(is.infinite(dens) | dens == rep(0, 4)))
    }
    ## Realise
    expect_length(realise(d, n = 0), 0)
  }
})
