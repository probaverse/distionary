#' @srrstats {G5.2} Appropriate error behaviour is tested for all
#' functions explicitly, but warnings are omitted and saved for a future
#' version.
#' @srrstats {G5.2a} While messages produced within R code by `stop()`,
#' `warning()`, `message()`, or equivalent are not unique (e.g.,
#' `dst_norm()`, `dst_pois()`, etc. all have the same `length != 1`
#' error message), they are unique enough to allow the user to debug.
#' @srrstats {G5.2b} Explicit tests trigger the `stop()` calls in this
#' version of distionary.
#' @srrstats {G5.8} Edge conditions are tested when evaluating
#' representations.

test_that("Built-in distributions resolve to Null dist with NA param.", {
  for (item in test_distributions) {
    paramset <- item$valid[[1]]
    for (i in seq_along(paramset)) {
      this_paramset <- paramset
      this_paramset[[i]] <- NA
      expect_equal(
        rlang::exec(!!item$distribution, !!!this_paramset),
        dst_null()
      )
    }
  }
})

test_that("Built-in distributions error with non-numeric input.", {
  for (item in test_distributions) {
    paramset <- item$valid[[1]]
    for (i in seq_along(paramset)) {
      this_paramset <- paramset
      this_paramset[[i]] <- "peekaboo"
      expect_error(rlang::exec(!!item$distribution, !!!this_paramset))
      this_paramset[[i]] <- TRUE
      expect_error(rlang::exec(!!item$distribution, !!!this_paramset))
      this_paramset[[i]] <- list(5)
      expect_error(rlang::exec(!!item$distribution, !!!this_paramset))
      this_paramset[[i]] <- factor("peekaboo")
      expect_error(rlang::exec(!!item$distribution, !!!this_paramset))
    }
  }
})

test_that("Built-in distributions require parameters of length 1.", {
  for (item in test_distributions) {
    paramset <- item$valid[[1]]
    for (i in seq_along(paramset)) {
      this_paramset <- paramset
      this_paramset[[i]] <- rep(paramset[[i]], 2)
      expect_error(rlang::exec(!!item$distribution, !!!this_paramset))
      this_paramset[[i]] <- numeric(0L)
      expect_error(rlang::exec(!!item$distribution, !!!this_paramset))
    }
  }
})

test_that("Some built-in distributions have invalid parameter ranges.", {
  for (item in test_distributions) {
    for (paramset in item$invalid) {
      expect_error(rlang::exec(item$distribution, !!!paramset))
    }
  }
})
