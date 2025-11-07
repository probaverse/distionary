#' @srrstats {G5.5} Correctness tests are run with four fixed random seeds,
#' applicable for testing the `realise()` function (same as G5.6b). --> Copied
#' to `test-realise.R`.
#' @srrstats {G5.6b} Parameter recovery tests are run with four fixed random
#' seeds, applicable for testing the `realise()` function. --> Copied to
#' `test-realise.R`.
#' @srrstats {G5.9b} Four different seeds are chosen, and the results do
#' not differ: the data still come from the specified distribution.

test_that("Random number generation works", {
  # Strategy:
  # Generate more and more data until the p-value is > 0.05.
  # If the distribution is not the correct specification, the p-value
  # will get closer and closer to 0. If the distribution is correct,
  # then p ~ Unif(0,1), and if p < 0.05 (unlucky), extending the
  # sample will eliminate the luck factor.
  # If the number generator is *not* correct, then the large sample
  # size drawn (1000) is likely to trigger a small p-value; to mitigate
  # the risk that a large p-value is obtained anyway, the process is
  # repeated 4 times with different seeds.
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (vtype(d) == "continuous") {
        cdf <- representation_as_function(d, "cdf")
        for (sd in 1:4) {
          set.seed(sd)
          x <- realise(d, n = 1000)
          pval <- ks.test(x, cdf)$p.value
          maxiter <- 10L
          i <- 1
          while (pval < 0.05 && i < maxiter) {
            x <- append(x, realise(d, n = 1000))
            pval <- ks.test(x, cdf)$p.value
            i <- i + 1
          }
          expect_gt(pval, 0.05)
        }
      } else if (pretty_name(d) != "Degenerate") {
        for (sd in 1:4) {
          # For tapering distributions like the Poisson,
          # generate data only within a finite range encompassing most of the
          # possibilities, and compare to the distribution trimmed to those
          # values. This is because generating more and more data will
          # result in more and more new (large) values with low frequency,
          # throwing off the chi-squared test.
          set.seed(sd)
          x <- realise(d, n = 1000)
          tbl <- table(x)
          bnds <- range(x)
          if (pretty_name(d) == "Finite") {
            rng <- parameters(d)$outcomes
            rng <- rng[rng >= bnds[1] & rng <= bnds[2]]
          } else {
            rng <- bnds[1]:bnds[2]
          }
          freq <- rep(0, length(rng))
          names(freq) <- rng
          freq[names(tbl)] <- unname(unclass(tbl))
          p_orig <- eval_pmf(d, at = rng)
          p <- p_orig / sum(p_orig)
          pval <- suppressWarnings(chisq.test(freq, p = p)$p.value)
          maxiter <- 10L
          i <- 1
          while (pval < 0.05 && i < maxiter) {
            x <- append(x, realise(d, n = 10000))
            x <- x[x >= bnds[1] & x <= bnds[2]]
            tbl <- table(x)
            freq <- rep(0, length(rng))
            names(freq) <- rng
            freq[names(tbl)] <- unname(unclass(tbl))
            pval <- suppressWarnings(chisq.test(freq, p = p)$p.value)
            i <- i + 1
          }
          if (pval < 0.05) {
            print(pval)
            print(sd)
            print(d)
          }
          expect_gt(pval, 0.05)
        }
      } else if (pretty_name(d) == "Degenerate") {
        for (sd in 1:4) {
          set.seed(sd)
          x <- realise(d, n = 1000)
          expect_identical(unique(x), parameters(d)[[1]])
        }
      }
    }
  }
})
