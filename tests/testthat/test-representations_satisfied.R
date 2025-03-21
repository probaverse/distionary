#' Check that each representation satisfies the properties of that
#' representation. For example, a CDF should be non-decreasing and
#' evaluate between 0 and 1, reaching 0 and 1 at the endpoints.
test_that("Each representation satisfies its requirements.", {
  for (i in seq_along(test_distributions)) {
    item <- test_distributions[[i]]
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      v <- vtype(d)
      p <- 0:50 / 50
      x <- eval_quantile(d, at = p)
      r <- range(d)
      ## CDF should increase from y=0 to y=1.
      cdf_vals <- eval_cdf(d, at = x)
      if (v == "continuous") {
        expect_equal(cdf_vals[1], 0)
      } else {
        expect_gt(cdf_vals[1], 0)
      }
      expect_lte(cdf_vals[49], 1)
      expect_true(all(diff(cdf_vals) >= 0))
      ## Survival
      if (!is.null(d$survival)) {
        surv <- eval_survival(d, at = x)
        if (v == "continuous") {
          expect_lte(surv[1], 1)
        }
        expect_gte(surv[49], 0)
        expect_true(all(diff(surv) <= 0))
      }
      ## Density
      if (v == "continuous") {
        a <- eval_quantile(d, at = c(0.005, 0.995))
        dens_vals <- eval_density(d, at = x[!is.infinite(x)])
        expect_true(all(dens_vals >= 0))
        dens_fun <- representation_as_function(d, "density")
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
        expect_true(all(diff(x) >= 0))
        if (v == "continuous") {
          expect_equal(eval_cdf(d, at = x), p)
        }
      }
      ## Hazard
      if (v == "continuous") {
        expect_true(all(eval_hazard(d, at = x[-c(1, 51)]) >= 0))
      }
      ## CHF
      if (v == "continuous") {
        chf <- eval_chf(d, at = x[-51])
        expect_true(all(diff(chf) >= 0))
      }
      ## PMF
      if (v == "discrete" && pretty_name(d) != "Degenerate") {
        if (is.infinite(r[2])) {
          xx <- 0:99
        } else {
          xx <- r[1]:r[2]
        }
        ## From CDF
        pmf_evald <- eval_pmf(d, at = xx)
        pmf_derived <- prob_left(d, of = xx, inclusive = TRUE) -
          prob_left(d, of = xx, inclusive = FALSE)
        expect_equal(pmf_derived, pmf_evald)
        ## Sum to 1
        pmf_sum <- sum(pmf_evald)
        diff_from_one <- 1 - pmf_sum
        while (diff_from_one > 1e-9) {
          xx <- 100 + xx
          pmf_evald <- eval_pmf(d, at = xx)
          pmf_sum <- pmf_sum + sum(pmf_evald)
          diff_from_one <- 1 - pmf_sum
        }
      }
    }
  }
})
