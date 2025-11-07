#' @srrstats {PD4.2} All distributions are tested using at least two
#' valid parameter sets, and at least one invalid parameter set.
test_that("Parameters are specified and retrieved correctly w/ parameters()", {
  param_retrieved <- list()
  # Remove the finite and empirical distributions, which are special.
  test_suite <- test_distributions
  test_suite[["empirical"]] <- NULL
  test_suite[["finite"]] <- NULL
  for (i in seq_along(test_suite)) {
    item <- test_suite[[i]]
    paramset <- item$valid[[1]]
    d <- rlang::exec(item$distribution, !!!paramset)
    param_retrieved[[i]] <- parameters(d)
    expect_identical(param_retrieved[[!!i]], !!paramset)
    parameters(d)[[1]] <- "hello"
    expect_identical(parameters(d)[[1]], "hello")
    parameters(d)[[1]] <- NaN
    expect_identical(parameters(d)[[1]], NaN)
    parameters(d)[[1]] <- data.frame(x = 1)
    expect_identical(parameters(d)[[1]], data.frame(x = 1))
  }
})

test_that("Empirical distribution parameters() are correct.", {
  item <- test_distributions[["empirical"]]
  dst_finite_paramnames <- names(parameters(dst_finite(1:2, c(0.5, 0.5))))
  for (paramset in item$valid) {
    d <- rlang::exec(item$distribution, !!!paramset)
    param_retrieved <- parameters(d)
    if (length(paramset) == 1) {
      paramset[[2]] <- rep(1, length(paramset[[1]]))
    }
    names(paramset) <- dst_finite_paramnames
    i_mask <- paramset[[2]] != 0
    paramset[[1]] <- paramset[[1]][i_mask]
    paramset[[2]] <- paramset[[2]][i_mask]
    paramset[[2]] <- paramset[[2]][order(paramset[[1]])]
    paramset[[1]] <- sort(paramset[[1]])
    paramset[[2]] <- paramset[[2]] / sum(paramset[[2]])
    expect_identical(param_retrieved, paramset)
  }
})

test_that("Finite distribution parameters() are correct.", {
  item <- test_distributions[["finite"]]
  for (paramset in item$valid) {
    d <- rlang::exec(item$distribution, !!!paramset)
    if (length(paramset[[1]]) == 1) {
      expect_identical(d, dst_degenerate(paramset[[1]]))
      next
    }
    param_retrieved <- parameters(d)
    i_mask <- paramset[[2]] != 0
    paramset[[1]] <- paramset[[1]][i_mask]
    paramset[[2]] <- paramset[[2]][i_mask]
    paramset[[2]] <- paramset[[2]][order(paramset[[1]])]
    paramset[[1]] <- sort(paramset[[1]])
    expect_identical(param_retrieved, paramset)
  }
})
