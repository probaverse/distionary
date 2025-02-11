stats_distributions <- list(
  list(
    distribution = dst_bern,
    invalid = list(
      c(prob = -1),
      c(prob = 2)
    ),
    valid = list(
      c(prob = 0.5),
      c(prob = 0.7)
    )
  ),
  list(
    distribution = dst_beta,
    invalid = list(
      c(shape1 = -1, shape2 = 1),
      c(shape1 = 1, shape2 = -1)
    ),
    valid = list(
      c(shape1 = 2, shape2 = 0.5),
      c(shape1 = 0.5, shape2 = 0.5)
    )
  ),
  list(
    distribution = dst_binom,
    invalid = list(
      c(size = -1, prob = 0.5),
      c(size = 4, prob = -1),
      c(size = 4, prob = 2)
    ),
    valid = list(
      c(size = 3, prob = 0.3)
    )
  ),
  list(
    distribution = dst_cauchy,
    invalid = list(
      c(location = 0, scale = -1)
    ),
    valid = list(
      c(location = 0, scale = 1),
      c(location = 2, scale = 0.5)
    )
  ),
  list(
    distribution = dst_chisq,
    invalid = list(
      c(df = -1)
    ),
    valid = list(
      c(df = 1),
      c(df = 10)
    )
  ),
  list(
    distribution = dst_degenerate,
    invalid = list(
      c(location = "a")
    ),
    valid = list(
      c(location = 1),
      c(location = -4)
    )
  ),
  list(
    distribution = dst_exp,
    invalid = list(
      c(rate = -1)
    ),
    valid = list(
      c(rate = 1),
      c(rate = 2.5)
    )
  ),
  list(
    distribution = dst_f,
    invalid = list(
      c(df1 = -1, df2 = 2),
      c(df1 = 3, df2 = -2)
    ),
    valid = list(
      c(df1 = 2, df2 = 1),
      c(df1 = 3, df2 = 3),
      c(df1 = 1.5, df2 = 5),
      c(df1 = 3.5, df2 = 7),
      c(df1 = 2.2, df2 = 9)
    )
  ),
  list(
    distribution = dst_gamma,
    invalid = list(
      c(shape = -1, rate = 1),
      c(shape = 1, rate = -1)
    ),
    valid = list(
      c(shape = 2, rate = 3),
      c(shape = 4, rate = 1.5)
    )
  ),
  list(
    distribution = dst_geom,
    invalid = list(
      c(prob = -1),
      c(prob = 2)
    ),
    valid = list(
      c(prob = 0.3),
      c(prob = 0.5),
      c(prob = 0.8)
    )
  ),
  list(
    distribution = dst_gev,
    invalid = list(
      c(location = 0, scale = -1, shape = 1)
    ),
    valid = list(
      c(location = 0, scale = 1, shape = 1.2),
      c(location = 0, scale = 1, shape = 0),
      c(location = 0, scale = 1, shape = -1.2)
    )
  ),
  list(
    distribution = dst_gpd,
    invalid = list(
      c(scale = -1, shape = 1)
    ),
    valid = list(
      c(scale = 1, shape = 1.2),
      c(scale = 1, shape = 0),
      c(scale = 1, shape = -1.2)
    )
  ),
  list(
    distribution = dst_hyper,
    invalid = list(
      c(K = -2, N = 4, n = 5),
      c(K = 2, N = -4, n = 5),
      c(K = 2, N = 4, n = -5)
    ),
    valid = list(
      c(K = 2, N = 4, n = 5),
      c(K = 4, N = 2, n = 1)
    )
  )
)

for (i in seq_along(stats_distributions)) {
  item <- stats_distributions[[i]]

  test_that(paste("Distribution", i, "invalid parameters check."), {
    for (paramset in item$invalid) {
      expect_error(rlang::exec(item$distribution, !!!paramset))
    }
  })

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

    # Check each representation individually, that it corresponds
    # to a valid distribution.
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
          dens_vals <- eval_density(d, at = x)
          expect_true(all(dens_vals >= 0))
          int <- integrate(dens_fun, lower = r[1], upper = r[2])
          expect_equal(int$value, 1, tolerance = 1e-6)
        }
        ## Mass
        pmf_fun <- d$pmf
        if (!is.null(pmf_fun)) {
          pmf_vals <- eval_pmf(d, at = -40:1000)
          expect_true(all(pmf_vals >= 0))
          expect_gt(sum(pmf_vals), 0.9)
          expect_lte(sum(pmf_vals), 1)
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
          check_mean <- validate_mean(d)
          expect_true(check_mean || is.na(check_mean))
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
