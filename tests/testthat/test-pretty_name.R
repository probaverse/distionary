#' @srrstats {G5.3} Functions that are expected to return objects containing no
#' missing (`NA`) or undefined (`NaN`, `Inf`) values are tested.
test_that("Pretty name is pretty.", {
  for (item in test_distributions) {
    d <- rlang::exec(item$distribution, !!!item$valid[[1]])
    prettynm <- pretty_name(d)
    expect_length(prettynm, 1)
    expect_true(!is.na(prettynm))
    prettynm <- pretty_name(d, 3)
    expect_length(prettynm, 1)
    expect_true(!is.na(prettynm))
  }
  expect_equal(pretty_name(dst_null()), "Null")
  expect_equal(pretty_name(dst_null(), 3), "Null")
})
