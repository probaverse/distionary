#' Check network algorithms with known values (i.e., the intrinsically
#' defined ones). Because many distributions are being tested, this also
#' checks that these known values were specified correctly.
test_that("Algorithms invoking the distribution network match known values.", {
  ## Quantile
  check_q <- validate_quantile(d)
  expect_true(check_q || is.na(check_q))

  if (is.null(distribution$quantile)) {
    return(NA)
  }
  qf <- distribution$quantile
  distribution$quantile <- NULL
  p <- 1:49 / 50
  qf_builtin <- qf(p)
  qf_derived <- eval_quantile(distribution, at = p)
  diffs <- abs(qf_builtin - qf_derived)
  if (all(diffs < tol)) {
    return(TRUE)
  } else {
    if (verbose) {
      i <- which(diffs == max(diffs))[1]
      message(
        "Invalid survival function, evaluating to a difference of ",
        signif(max(diffs), 4), " at p = ", p[i], "."
      )
    }
    return(FALSE)
  }



  ## Density
  check_dens <- validate_density(d)
  expect_true(check_dens || is.na(check_dens))
  ## PMF
  check_pmf <- validate_pmf(d)
  expect_true(check_pmf || is.na(check_pmf))
  ## Range
  check_rng <- validate_range(d)
  expect_true(check_rng || is.na(check_rng))
  ## Moments
  if (!attr(d, "name") %in% c("Cauchy", "Degenerate")) {
    ## Mean
    if (!(attr(d, "name") == "Student t" && parameters(d)$df == 1)) {
      check_mean <- validate_mean(d)
      expect_true(check_mean || is.na(check_mean))
    }
    ## Variance
    check_var <- validate_variance(d)
    expect_true(check_var || is.na(check_var))
    ## Standard Deviation
    check_sd <- validate_stdev(d)
    expect_true(check_sd || is.na(check_sd))
    ## Skewness
    if (v == "discrete") {
      check_sk <- validate_skewness(d, tol = 1e-3)
    } else {
      check_sk <- validate_skewness(d)
    }
    expect_true(check_sk || is.na(check_sk))
    ## Kurtosis
    check_kur <- validate_kurtosis(d)
    expect_true(check_kur || is.na(check_kur))
    ## Excess Kurtosis
    check_exc <- validate_kurtosis_exc(d)
    expect_true(check_exc || is.na(check_exc))
  }
}
)
