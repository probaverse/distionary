test_that("Parameters are specified and retrieved correctly w parameters()", {
  param_retrieved <- list()
  for (i in seq_along(test_distributions)) {
    item <- test_distributions[[i]]
    paramset <- item$valid[[1]]
    d <- rlang::exec(item$distribution, !!!paramset)
    param_retrieved[[i]] <- parameters(d)
    expect_identical(param_retrieved[[!!i]], !!paramset)
  }
})
