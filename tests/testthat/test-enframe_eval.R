#' @srrstats {G5.4} Correctness tests are conducted to test that
#' statistical algorithms (calculating properties from other distribution
#' properties) produce expected results to test distributions with set
#' parameters.
#' @srrstats {G5.8} Edge conditions are tested when evaluating
#' representations.
#' @srrstats {G5.8a} Zero-length data input outputs 0-length vectors.
#' An error is thrown if zero-length parameters are input into `dst_*()`
#' functions.
#' @srrstats {G5.8b} Data of unsupported types does not pass the checkmate
#' checks on function inputs.
#' @srrstats {G5.8c} Data with all-`NA` fields or columns or all identical
#' fields or columns is no different from having some `NA` fields.

test_that("enframe matches eval", {
  d <- dst_norm(0, 1)
  p <- 1:9 / 10
  x <- -3:3
  expect_equal(enframe_cdf(d, at = x)$cdf, eval_cdf(d, at = x))
  expect_equal(enframe_density(d, at = x)$density, eval_density(d, at = x))
  expect_equal(
    enframe_pmf(d, at = x)$pmf,
    eval_pmf(d, at = x)
  )
  expect_equal(
    enframe_odds(d, at = x)$odds,
    eval_odds(d, at = x)
  )
  expect_equal(enframe_survival(d, at = x)$survival, eval_survival(d, at = x))
  expect_equal(enframe_hazard(d, at = x)$hazard, eval_hazard(d, at = x))
  expect_equal(enframe_chf(d, at = x)$chf, eval_chf(d, at = x))
  expect_equal(
    enframe_quantile(d, at = p)$quantile,
    eval_quantile(d, at = p)
  )
})

test_that("enframe throws error if an ellipsis entry is not a distribution", {
  d <- dst_norm(0, 1)
  expect_error(enframe_cdf(5, at = 1:10))
  expect_error(enframe_cdf(d, 5, at = 1:10))
  expect_error(enframe_cdf(at = 1:10))
})

test_that("column names match the function, by default.", {
  d <- dst_norm(0, 1)
  expect_equal(names(enframe_cdf(d, at = 0))[2L], "cdf")
  expect_equal(names(enframe_pmf(d, at = 0))[2L], "pmf")
  expect_equal(names(enframe_odds(d, at = 0))[2L], "odds")
  expect_equal(names(enframe_density(d, at = 0))[2L], "density")
  expect_equal(names(enframe_quantile(d, at = 0.1))[2L], "quantile")
  expect_equal(names(enframe_chf(d, at = 0))[2L], "chf")
  expect_equal(names(enframe_hazard(d, at = 0))[2L], "hazard")
  expect_equal(names(enframe_survival(d, at = 0))[2L], "survival")
})

test_that("Manual column names work.", {
  d <- dst_norm(0, 1)
  x <- 1:3
  df <- enframe_cdf(d, at = x, arg_name = "one", fn_prefix = "two")
  expect_equal(sort(c("one", "two")), sort(names(df)))
  df <- enframe_cdf(
    mod1 = d, mod2 = d, at = x, arg_name = "one", fn_prefix = "two", sep = "."
  )
  expect_equal(sort(c("one", "two.mod1", "two.mod2")), sort(names(df)))
  df <- enframe_cdf(
    mod1 = d, d, at = x, arg_name = "one", fn_prefix = "two", sep = "."
  )
  expect_equal(sort(c("one", "two.mod1", "two.d")), sort(names(df)))
})

test_that("Dimensions and type are as expected.", {
  d <- dst_norm(0, 1)
  x <- -3:3
  df <- enframe_cdf(d, at = x, arg_name = "one", fn_prefix = "two")
  expect_true(is.data.frame(df))
  expect_equal(dim(df), c(7, 2))
  df <- enframe_cdf(mod1 = d, mod2 = d, at = x)
  expect_equal(dim(df), c(7, 3))
  df <- enframe_cdf(d, at = numeric())
  expect_equal(nrow(df), 0)
  expect_length(eval_cdf(d, at = numeric()), 0)
})
