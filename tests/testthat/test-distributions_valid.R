distributions_list <- list(
  list(
    distribution = dst_bern,
    invalid = list(
      list(prob = -1),
      list(prob = 2)
    ),
    valid = list(
      list(prob = 0.5),
      list(prob = 0.7)
    )
  ),
  list(
    distribution = dst_beta,
    invalid = list(
      list(shape1 = -1, shape2 = 1),
      list(shape1 = 1, shape2 = -1)
    ),
    valid = list(
      list(shape1 = 2, shape2 = 0.5),
      list(shape1 = 0.5, shape2 = 0.5)
    )
  ),
  list(
    distribution = dst_binom,
    invalid = list(
      list(size = -1, prob = 0.5),
      list(size = 4, prob = -1),
      list(size = 4, prob = 2)
    ),
    valid = list(
      list(size = 3, prob = 0.3)
    )
  ),
  list(
    distribution = dst_cauchy,
    invalid = list(
      list(location = 0, scale = -1)
    ),
    valid = list(
      list(location = 0, scale = 1),
      list(location = 2, scale = 0.5)
    )
  ),
  list(
    distribution = dst_chisq,
    invalid = list(
      list(df = -1)
    ),
    valid = list(
      list(df = 1),
      list(df = 10)
    )
  ),
  list(
    distribution = dst_degenerate,
    invalid = list(
      list(location = "a")
    ),
    valid = list(
      list(location = 1),
      list(location = -4)
    )
  ),
  list(
    distribution = dst_exp,
    invalid = list(
      list(rate = -1)
    ),
    valid = list(
      list(rate = 1),
      list(rate = 2.5)
    )
  ),
  list(
    distribution = dst_f,
    invalid = list(
      list(df1 = -1, df2 = 2),
      list(df1 = 3, df2 = -2)
    ),
    valid = list(
      list(df1 = 2, df2 = 1),
      list(df1 = 3, df2 = 3),
      list(df1 = 1.5, df2 = 5),
      list(df1 = 3.5, df2 = 7),
      list(df1 = 2.2, df2 = 9)
    )
  ),
  list(
    distribution = dst_gamma,
    invalid = list(
      list(shape = -1, rate = 1),
      list(shape = 1, rate = -1)
    ),
    valid = list(
      list(shape = 2, rate = 3),
      list(shape = 4, rate = 1.5)
    )
  ),
  list(
    distribution = dst_geom,
    invalid = list(
      list(prob = -1),
      list(prob = 2)
    ),
    valid = list(
      list(prob = 0.3),
      list(prob = 0.5),
      list(prob = 0.8)
    )
  ),
  list(
    distribution = dst_gev,
    invalid = list(
      list(location = 0, scale = -1, shape = 1)
    ),
    valid = list(
      list(location = 0, scale = 1.5, shape = 1.2),
      list(location = 0, scale = 1.5, shape = 0),
      list(location = 0, scale = 1.5, shape = -1.2)
    )
  ),
  list(
    distribution = dst_gpd,
    invalid = list(
      list(scale = -1, shape = 1)
    ),
    valid = list(
      list(scale = 1.5, shape = 1.2),
      list(scale = 1.5, shape = 0),
      list(scale = 1.5, shape = -1.2)
    )
  ),
  list(
    distribution = dst_hyper,
    invalid = list(
      list(m = -2, n = 4, k = 5),
      list(m = 2, n = -4, k = 5),
      list(m = 2, n = 4, k = -5),
      list(m = 2, n = 4, k = 7)
    ),
    valid = list(
      list(m = 8, n = 4, k = 5),
      list(m = 3, n = 4, k = 5),
      list(m = 8, n = 5, k = 3),
      list(m = 2, n = 5, k = 3)
    )
  ),
  list(
    distribution = dst_lnorm,
    invalid = list(
      list(sdlog = -1.2)
    ),
    valid = list(
      list(meanlog = -1, sdlog = 1.2),
      list(meanlog = 0, sdlog = 1.1)
    )
  ),
  list(
    distribution = dst_lp3,
    invalid = list(
      list(meanlog = 0, sdlog = -1, skew = 1)
    ),
    valid = list(
      list(meanlog = 0, sdlog = 1.1, skew = 0.7),
      list(meanlog = -1, sdlog = 0.7, skew = -0.7)
    )
  ),
  list(
    distribution = dst_nbinom,
    invalid = list(
      list(size = -3, prob = 0.4),
      list(size = 3, prob = -1),
      list(size = 3, prob = 2)
    ),
    valid = list(
      list(size = 3, prob = 0.4),
      list(size = 5, prob = 0.8)
    )
  ),
  list(
    distribution = dst_norm,
    invalid = list(
      list(mean = 0, sd = -1)
    ),
    valid = list(
      list(mean = 1.1, sd = 2.2),
      list(mean = -1.5, sd = 3.7)
    )
  ),
  list(
    distribution = dst_pearson3,
    invalid = list(
      list(location = 0, scale = -1, shape = 1),
      list(location = 0, scale = 1, shape = -1)
    ),
    valid = list(
      list(location = 1.1, scale = 2.2, shape = 3.3),
      list(location = 0, scale = 1, shape = 1)
    )
  ),
  list(
    distribution = dst_pois,
    invalid = list(
      list(lambda = -1)
    ),
    valid = list(
      list(lambda = 1),
      list(lambda = 2.2)
    )
  ),
  list(
    distribution = dst_t,
    invalid = list(
      list(df = -2),
      list(df = -1)
    ),
    valid = list(
      list(df = 1),
      list(df = 2),
      list(df = 3),
      list(df = 4),
      list(df = 5)
    )
  ),
  list(
    distribution = dst_unif,
    invalid = list(
      list(min = 9, max = 0)
    ),
    valid = list(
      list(min = 0, max = 1),
      list(min = -2, max = 1)
    )
  ),
  list(
    distribution = dst_weibull,
    invalid = list(
      list(shape = -1, scale = 1),
      list(shape = 1, scale = -1)
    ),
    valid = list(
      list(shape = 0.8, scale = 1.5),
      list(shape = 3.3, scale = 2.4)
    )
  )
)

for (i in seq_along(distributions_list)) {
  item <- distributions_list[[i]]

  # --- TEST BLOCK ----
  test_that(paste("Distribution", i, "invalid parameters check."), {
    for (paramset in item$invalid) {
      expect_error(rlang::exec(item$distribution, !!!paramset))
    }
  })

  # --- TEST BLOCK ----
  test_that(paste("Distribution", i, "parameters require length 1."), {
    paramset <- item$valid[[1]]
    for (i in seq_along(paramset)) {
      this_paramset <- paramset
      this_paramset[[i]] <- rep(paramset[[i]], 2)
      expect_error(rlang::exec(item$distribution, !!!this_paramset))
      this_paramset[[i]] <- numeric(0L)
      expect_error(rlang::exec(item$distribution, !!!this_paramset))
    }
  })

  # --- TEST BLOCK ----
  #' @srrstats {G5.9} Noise susceptibility tests have been conducted on
  #' distribution parameters and evaluation inputs.
  #' @srrstats {G5.9a} Machine tolerance has been added to distribution
  #' parameters and evaluation inputs and compared to originals.
  test_that(paste("Distribution", i, "not sensitive to machine tolerance"), {
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
    expect_equal(
      skewness(d_orig),
      skewness(d_eps)
    )
    ## Kurtosis
    expect_equal(
      kurtosis(d_orig),
      kurtosis(d_eps)
    )
    ## Excess Kurtosis
    expect_equal(
      kurtosis_exc(d_orig),
      kurtosis_exc(d_eps)
    )
  })

  # --- TEST BLOCK ----
  test_that(paste("Distribution", i, "resolves to Null dist with NA param"), {
    paramset <- item$valid[[1]]
    for (i in seq_along(paramset)) {
      this_paramset <- paramset
      this_paramset[[i]] <- NA
      expect_equal(
        rlang::exec(item$distribution, !!!this_paramset),
        dst_null()
      )
    }
  })

  # --- TEST BLOCK ----
  # Make sure that defined distributions evaluate NA inputs properly.
  # Make sure they evaluate edge cases properly.
  #' @srrstats {G5.8} Edge conditions are tested when evaluating
  #' representations
  #' @srrstats {G5.8a} Zero-length data input outputs 0-length vectors.
  #' An error is thrown if zero-length parameters are input into `dst_*()`
  #' functions.
  #' @srrstats {G5.8b} Data of unsupported types does not pass the checkmate
  #' checks on function inputs.
  #' @srrstats {G5.8c} Data with all-`NA` fields or columns or all identical
  #' fields or columns is no different from having some `NA` fields.
  #' @srrstats {G5.8d} Data outside of the scope of the (quantile) algorithm
  #' is only applicable when the quantile probability is outside of [0, 1],
  #' in which case an error is thrown (due to a check for valid function
  #' inputs)
  test_that(
    paste(
      "Defined distributions evaluate edge cases properly: Distribution ", i
    ),
    {
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
  )

  # --- TEST BLOCK ----
  # Make sure there's no density for discrete variables, and no
  # PMF for continuous variables.
  test_that(paste("Distribution", i, "density / mass lineup with vtype"), {
    d <- rlang::exec(item$distribution, !!!item$valid[[1]])
    v <- vtype(d)
    if (v == "discrete") expect_null(d$density)
    if (v == "continuous") expect_null(d$pmf)
  })

  for (paramset in item$valid) {
    d <- rlang::exec(item$distribution, !!!paramset)
    v <- vtype(d)
    prettynm <- pretty_name(d, param_digits = 2)

    # --- TEST BLOCK ----
    test_that("pretty name is pretty.", {
      expect_length(prettynm, 1)
      expect_true(!is.na(prettynm))
    })

    # --- TEST BLOCK ----
    test_that("Object is a distribution.", {
      expect_true(is_distribution(d))
    })

    # --- TEST BLOCK ----
    # Check each representation individually, that it corresponds
    # to a valid distribution, by comparing each representation
    # to another.
    test_that(
      paste(
        "Defined representations correspond to a distribution: ",
        prettynm, "."
      ),
      {
        p <- 1:49 / 50
        x <- eval_quantile(d, at = p)
        r <- range(d)
        ## CDF
        cdf_vals <- eval_cdf(d, at = x)
        if (v == "continuous") expect_gte(cdf_vals[1], 0)
        expect_lte(cdf_vals[49], 1)
        expect_true(all(diff(cdf_vals) >= 0))
        ## Survival
        if (!is.null(d$survival)) {
          surv <- eval_survival(d, at = x)
          if (v == "continuous") expect_lte(surv[1], 1)
          expect_gte(surv[49], 0)
          expect_true(all(diff(surv) <= 0))
        }
        ## Density
        dens_fun <- d$density
        if (!is.null(dens_fun)) {
          a <- eval_quantile(d, at = c(0.005, 0.995))
          dens_vals <- eval_density(d, at = x)
          expect_true(all(dens_vals >= 0))
          int <- integrate(dens_fun, lower = a[1], upper = a[2])
          expect_equal(int$value, 0.99, tolerance = 1e-5)
        }
        ## Mass
        pmf_fun <- d$pmf
        if (!is.null(pmf_fun)) {
          pmf_vals <- eval_pmf(d, at = -40:1000)
          expect_true(all(pmf_vals >= 0))
          expect_gt(sum(pmf_vals), 0.9)
          expect_lt(sum(pmf_vals), 1 + 2 * .Machine$double.eps)
        }
        ## Quantile
        qf <- d$quantile
        if (!is.null(qf)) {
          p <- 0:50 / 50
          x <- eval_quantile(d, at = p)
          expect_true(all(diff(x) >= 0))
          if (v == "continuous") expect_equal(eval_cdf(d, at = x), p)
        }
      }
    )

    # --- TEST BLOCK ----
    # Check that the provided representations link up properly and describe
    # the same distribution. This can be achieved by deriving the
    # representation as if it was absent. Note that some of the previous
    # tests are covered here, but are still included for ease of spotting
    # errors. This also checks that the
    # derivations are correct when a representation is missing -- but
    # not all derivations: for example, distionary never defines a
    # hazard function in its distributions, so that and others will
    # have to be checked another way.
    test_that(
      paste(
        "All representations describe the same distribution: ",
        prettynm, "."
      ),
      {
        ## Quantile
        check_q <- validate_quantile(d)
        expect_true(check_q || is.na(check_q))
        ## Survival
        check_s <- validate_survival(d)
        expect_true(check_s || is.na(check_s))
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
    # --- End parameter set ---
  }
  # --- End distribution ---
}

# --- TEST BLOCK ----
# Some representations are never given for distribution families in
# distionary. For example, no distribution has a hazard function defined.
# Check that the representation is valid based on its definition.
test_that("Representations that are never specified in distionary work.", {
  ## Hazard
  d <- dst_norm(0, 1)
  x <- seq(-4, 4, length.out = 100)
  h1 <- eval_hazard(d, at = x)
  h2 <- dnorm(x) / pnorm(x, lower.tail = FALSE)
  expect_equal(h1, h2)
  ## Hazard on non-continuous distribution
  d <- dst_pois(1)
  expect_error(eval_hazard(d, at = 1))
  ## Odds
  d <- dst_nbinom(10, 0.4)
  p <- eval_pmf(d, at = 0:10)
  o1 <- p / (1 - p)
  o2 <- eval_odds(d, 0:10)
  expect_equal(o1, o2)
  ## Cumulative hazard function
  d <- dst_weibull(3, 2)
  x <- 0:10
  chf1 <- eval_chf(d, at = x)
  haz <- \(t) eval_hazard(d, at = t)
  chf2 <- vapply(
    x, \(x_) integrate(haz, lower = 0, upper = x_)$value,
    FUN.VALUE = numeric(1)
  )
  expect_equal(chf1, chf2)
  ## Cumulative hazard on non-continuous distribution
  d <- dst_pois(1)
  expect_error(eval_chf(d, at = 1))
  ## Return level, continuous
  d <- dst_exp(0.2)
  rp <- c(1, 2, 5, 10, 50, 100)
  l1 <- eval_return(d, at = rp)
  l2 <- eval_quantile(d, at = 1 - 1 / rp)
  expect_equal(l1, l2)
  ## Return level, discrete
  d <- dst_hyper(10, 5, 8)
  rp <- c(1, 2, 5, 10, 50, 100)
  l1 <- eval_return(d, at = rp)
  l2 <- eval_quantile(d, at = 1 - 1 / rp)
  expect_equal(l1, l2)
  ## Median, discrete
  d <- dst_binom(10, 0.3)
  m1 <- median(d)
  m2 <- eval_quantile(d, 0.5)
  expect_equal(m1, m2)
})

rm(list = c(
  "distributions_list", "d", "item", "i", "paramset", "prettynm", "v"
))
