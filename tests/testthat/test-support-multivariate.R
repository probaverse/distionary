test_that("support_product() names, counts, and types its variables", {
  s <- support_product(a = continuous(), b = discrete(0:3))
  expect_true(is_support(s))
  expect_identical(dimension(s), 2L)
  expect_identical(variables(s), c("a", "b"))
  expect_identical(vtype_of_support(s), "mixed")
  expect_identical(
    vtype_of_support(support_product(continuous(), continuous())),
    "continuous"
  )
  expect_identical(
    vtype_of_support(support_product(discrete(1), discrete(natural0()))),
    "discrete"
  )
})

test_that("unnamed variables are named by position", {
  s <- support_product(continuous(), b = continuous(), continuous())
  expect_identical(variables(s), c("x1", "b", "x3"))
  expect_error(support_product(a = continuous(), a = continuous()), "twice")
})

test_that("products of products flatten", {
  xy <- support_product(x = continuous(), y = continuous())
  s <- support_product(xy, z = discrete(1:2))
  expect_identical(variables(s), c("x", "y", "z"))
  expect_error(support_product(w = xy, z = continuous()), "cannot rename")
})

test_that("a product of one support is that support", {
  expect_identical(support_product(continuous()), continuous())
})

test_that("discrete() takes points as a data frame or matrix", {
  s <- discrete(data.frame(x = c(1, 2, 2, 1), y = c(5, 5, 6, 5)))
  expect_s3_class(s, "support_points")
  expect_identical(dimension(s), 2L)
  expect_identical(nrow(s[["points"]]), 3L) # duplicate dropped
  expect_identical(vtype_of_support(s), "discrete")
  m <- discrete(cbind(1:3, 4:6))
  expect_identical(variables(m), c("x1", "x2"))
  # One column is univariate.
  expect_identical(dimension(discrete(data.frame(x = 1:3))), 1L)
  expect_error(discrete(data.frame(x = 1, y = NA_real_)), "NA")
  expect_error(discrete(data.frame(x = 1, y = "a")), "numeric")
})

test_that("empty multivariate supports are recognised", {
  expect_true(is_empty_support(support_product(continuous(), discrete())))
  no_points <- data.frame(x = numeric(), y = numeric())
  expect_true(is_empty_support(discrete(no_points)))
  expect_false(is_empty_support(support_product(continuous(), continuous())))
  expect_error(
    distribution(.support = support_product(continuous(), discrete())),
    "empty"
  )
})

test_that("univariate accessors refuse multivariate supports", {
  s <- support_product(continuous(), continuous())
  expect_error(atoms(s), "marginal")
  expect_error(regions(s), "marginal")
  expect_error(range(s), "marginal")
})

test_that("support_marginal() projects products and point sets", {
  s <- support_product(
    a = continuous(c(0, 1)),
    discrete(data.frame(b = c(1, 1, 2), c = c(3, 4, 4))),
    d = discrete(0:2)
  )
  expect_identical(support_marginal(s, 1L), continuous(c(0, 1)))
  expect_identical(support_marginal(s, 4L), discrete(0:2))
  bc <- support_marginal(s, 2:3)
  expect_s3_class(bc, "support_points")
  expect_identical(nrow(bc[["points"]]), 3L)
  b <- support_marginal(s, 2L)
  expect_identical(
    discretes::get_discretes_in(atoms(b)),
    c(1, 2)
  )
  expect_identical(variables(support_marginal(s, c(4L, 1L))), c("d", "a"))
})

test_that("enumerate_points() lists finite supports and only those", {
  s <- support_product(a = discrete(1:2), b = discrete(c(5, 6, 7)))
  pts <- enumerate_points(s)
  expect_identical(nrow(pts), 6L)
  expect_identical(names(pts), c("a", "b"))
  expect_null(enumerate_points(support_product(continuous(), discrete(1))))
  expect_null(enumerate_points(
    support_product(discrete(natural0()), discrete(1))
  ))
})

test_that("dimension() and variables() of distributions", {
  expect_identical(dimension(dst_norm(0, 1)), 1L)
  expect_null(variables(dst_norm(0, 1)))
  expect_identical(dimension(dst_null()), NA_integer_)
  d <- dst_bi_norm(c(a = 0, b = 0), sd = c(1, 1), cor = 0)
  expect_identical(dimension(d), 2L)
  expect_identical(variables(d), c("a", "b"))
  # A distribution has length 1 whatever its dimension.
  expect_identical(length(d), 1L)
})
