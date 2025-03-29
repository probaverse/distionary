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
