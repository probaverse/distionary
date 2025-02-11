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

    # Need to make sure that provided representations all describe the
    # same distribution. This can be achieved by deriving the
    # representation as if it was absent. This also checks that the
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
        check_sk <- validate_skewness(d, T)
        expect_true(check_sk || is.na(check_sk))
        ## Kurtosis
        check_kur <- validate_kurtosis(d)
        expect_true(check_kur || is.na(check_kur))
        ## Excess Kurtosis
        check_exc <- validate_kurtosis_exc(d)
        expect_true(check_exc || is.na(check_exc))
        ## Range
        check_rng <- validate_range(d)
        expect_true(check_rng || is.na(check_rng))
      }
    )

    # For the representations built in to the distribution, make sure that
    # they correspond to a valid distribution. This doesn't check
    # whether the representations point to the *same* distribution,
    # but that they are each valid (e.g., density integrates to 1).
    test_that(
      paste(
        "Defined representations correspond to a distribution: ",
        prettynm, "."
      ),
      {
        p <- 0:50 / 50
        x <- eval_quantile(d, at = p)
        ## CDF
        cdf_vals <- eval_cdf(d, at = x)
        if (v == "continuous") expect_equal(cdf_vals[1], 0)
        expect_equal(cdf_vals[51], 1)
        expect_true(all(diff(cdf_vals) >= 0))
        ## Survival
        if (!is.null(d$survival)) {
          surv <- eval_survival(d, at = x)
          if (v == "continuous") expect_equal(surv[1], 1)
          expect_equal(surv[51], 0)
          expect_true(all(diff(surv) <= 0))
        }
        ## Density
        dens_fun <- d$density
        if (!is.null(dens_fun)) {
          dens_vals <- eval_density(d, at = x)
          expect_true(all(dens_vals >= 0))
          int <- integrate(dens_fun, lower = x[1], upper = x[51])
          expect_equal(int$value, 1)
        }
        ## Mass
        pmf_fun <- d$pmf
        if (!is.null(pmf_fun)) {
          pmf_vals <- eval_pmf(d, at = 0:1000)
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
    # --- End parameter set ---
  }
  # --- End distribution ---
}
