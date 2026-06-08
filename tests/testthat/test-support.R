test_that("continuous() builds a continuous support, defaulting to the line.", {
  s <- continuous()
  expect_true(is_support(s))
  expect_equal(vtype_of_support(s), "continuous")
  expect_equal(unname(continuous_part(s)), matrix(c(-Inf, Inf), nrow = 1))
})

test_that("continuous() canonicalizes: sorts and merges touching/overlapping.", {
  expect_equal(
    unname(continuous_part(continuous(c(3, 4), c(0, 1), c(0.5, 2)))),
    matrix(c(0, 3, 2, 4), nrow = 2)
  )
  # Touching closed intervals merge.
  expect_equal(
    unname(continuous_part(continuous(c(0, 1), c(1, 2)))),
    matrix(c(0, 2), nrow = 1)
  )
})

test_that("continuous() rejects malformed intervals.", {
  expect_error(continuous(c(1, 0)), "lower <= upper")
  expect_error(continuous(c(0, NA)), "NA")
  expect_error(continuous(c(0, 1, 2)), "length-2")
})

test_that("discrete() accepts discretes objects and numeric vectors.", {
  expect_equal(vtype_of_support(discrete(discretes::natural0())), "discrete")
  s <- discrete(c(3.5, 1.2, 6.7))
  expect_equal(vtype_of_support(s), "discrete")
  expect_equal(as.double(atoms(s)), c(1.2, 3.5, 6.7))
})

test_that("discrete() requires at least one atom.", {
  expect_error(discrete(numeric(0)), "at least one atom")
})

test_that("mixed() requires both parts and accepts terse or rich continuous.", {
  s <- mixed(atoms = 0, continuous = c(0, Inf))
  expect_equal(vtype_of_support(s), "mixed")
  expect_equal(as.double(atoms(s)), 0)
  expect_equal(unname(continuous_part(s)), matrix(c(0, Inf), nrow = 1))
  # Rich continuous via continuous().
  s2 <- mixed(atoms = discretes::natural0(), continuous = continuous(c(0, 1), c(3, 4)))
  expect_equal(nrow(continuous_part(s2)), 2)
})

test_that("mixed() errors when either part is empty.", {
  expect_error(mixed(atoms = numeric(0), continuous = c(0, 1)), "atomic part")
  expect_error(mixed(atoms = 0, continuous = numeric(0)), "continuous part")
})

test_that("mixed() rejects a non-continuous support as its continuous part.", {
  expect_error(
    mixed(atoms = 0, continuous = discrete(c(1, 2))),
    "purely continuous"
  )
})

test_that(".support drives vtype and range on distribution().", {
  d_c <- distribution(
    cdf = function(x) x, density = function(x) 1,
    .support = continuous(c(0, Inf))
  )
  expect_equal(vtype(d_c), "continuous")
  expect_equal(range(d_c), c(0, Inf))
  expect_true(is_support(support(d_c)))

  d_d <- distribution(
    cdf = function(x) x, pmf = function(x) 1,
    .support = discrete(discretes::natural0())
  )
  expect_equal(vtype(d_d), "discrete")
  expect_equal(range(d_d), c(0, Inf))
})

test_that("A bare discretes object is accepted as a discrete .support.", {
  d <- distribution(
    cdf = function(x) x, pmf = function(x) 1,
    .support = discretes::natural0()
  )
  expect_equal(vtype(d), "discrete")
  expect_true(is_support(support(d)))
  expect_equal(as.double(atoms(d)[1:3]), c(0, 1, 2))
})

test_that("A bare numeric .support is rejected (range vs atoms ambiguity).", {
  expect_error(
    distribution(cdf = function(x) x, density = function(x) 1, .support = c(0, Inf)),
    "ambiguous"
  )
})

test_that("A support object passed to legacy .vtype is bridged to .support.", {
  d <- distribution(
    cdf = function(x) x, pmf = function(x) 1,
    .vtype = discrete(discretes::natural0())
  )
  expect_equal(vtype(d), "discrete")
  expect_false(is.null(support(d)))
})

test_that("Legacy .vtype strings still work and carry no structured support.", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  d <- distribution(
    cdf = function(x) x, density = function(x) 1, .vtype = "continuous"
  )
  expect_equal(vtype(d), "continuous")
  expect_null(support(d))
})

test_that(".vtype is soft-deprecated in favour of .support.", {
  lifecycle::expect_deprecated(
    distribution(cdf = function(x) x, density = function(x) 1, .vtype = "continuous")
  )
  # A support object passed to .vtype is bridged and does not warn.
  expect_no_warning(
    distribution(cdf = function(x) x, pmf = function(x) 1, .vtype = discrete(0))
  )
})

test_that("Support accessors error on legacy distributions and non-supports.", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  d <- distribution(
    cdf = function(x) x, density = function(x) 1, .vtype = "continuous"
  )
  expect_error(atoms(d), "no structured support")
  expect_error(atoms(1:10), "support object or a distribution")
})
