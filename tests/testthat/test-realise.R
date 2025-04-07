#' @srrstats {G5.5} Correctness tests are run with four fixed random seeds,
#' applicable for testing the `realise()` function (same as G5.6b). --> Copied
#' to `test-realise.R`.
#' @srrstats {G5.6b} Parameter recovery tests are run with four fixed random
#' seeds, applicable for testing the `realise()` function. --> Copied to
#' `test-realise.R`.
test_that("Random number generation works", {
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
        # print(d)
        for (sd in 1:4) {
          set.seed(sd)
          # print(sd)
          x <- realise(d, n = 1000)
          bnds <- range(x)
          tbl <- table(x)
          rng <- bnds[1]:bnds[2]
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
      } else {
        for (sd in 1:4) {
          # Degenerate
          set.seed(sd)
          x <- realise(d, n = 1000)
          expect_identical(unique(x), parameters(d)[[1]])
        }
      }
    }
  }
})
