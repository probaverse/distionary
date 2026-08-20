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
  expect_error(continuous(c(1, 0)))
  expect_error(continuous(c(0, NA)))
  expect_error(continuous(c(0, 1, 2)))
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

test_that("A support object passed to legacy .vtype is rejected.", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    distribution(
      cdf = function(x) x, pmf = function(x) 1,
      .support = discrete(1:3),
      .vtype = discrete(discretes::natural0())
    ),
    "Pass support objects to `.support`"
  )
  expect_error(
    distribution(
      cdf = function(x) x, pmf = function(x) 1,
      .support = discrete(1:3),
      .vtype = discretes::natural0()
    ),
    "Pass support objects to `.support`"
  )
})

test_that("Every distribution built by distribution() carries a support.", {
  d <- distribution(
    cdf = function(x) x, density = function(x) 1,
    .support = continuous(c(0, 1))
  )
  expect_false(is.null(support(d)))
  expect_equal(vtype(d), "continuous")
})

test_that(".vtype is soft-deprecated in favour of .support.", {
  lifecycle::expect_deprecated(
    distribution(
      cdf = function(x) x, density = function(x) 1,
      .support = continuous(), .vtype = "continuous"
    )
  )
})

test_that("Support accessors error on Null and on non-supports.", {
  # The Null distribution is the only one without a support to take apart.
  expect_error(atoms(dst_null()), "no structured support")
  expect_error(atoms(1:10), "support object or a distribution")
})

test_that("empty_support() is empty, and knows it.", {
  e <- empty_support()
  expect_true(is_support(e))
  expect_true(is_empty_support(e))
  expect_equal(discretes::num_discretes(atoms(e)), 0)
  expect_equal(nrow(continuous_part(e)), 0)
})

test_that("the empty support's variable type is 'empty', not 'unknown'.", {
  expect_equal(distionary:::vtype_of_support(empty_support()), "empty")
  expect_output(print(empty_support()), "empty")
})

test_that("is_empty_support() is FALSE for non-empty supports and non-supports.", {
  expect_false(is_empty_support(continuous(c(0, 1))))
  expect_false(is_empty_support(discrete(1:3)))
  expect_false(is_empty_support(mixed(atoms = 0, continuous = c(0, 1))))
  expect_false(is_empty_support(1:10))
  expect_false(is_empty_support(NULL))
  expect_false(is_empty_support(dst_norm(0, 1)))
})

test_that("continuous() with no intervals gives the empty support.", {
  expect_true(is_empty_support(continuous(numeric(0))))
  # But `continuous()` with no arguments at all is the whole real line.
  expect_false(is_empty_support(continuous()))
})

test_that("a distribution cannot be given an empty support.", {
  expect_error(
    distribution(cdf = stats::pnorm, .support = empty_support()),
    "cannot have an empty support"
  )
  expect_error(
    distribution(cdf = stats::pnorm, .support = continuous(numeric(0))),
    "cannot have an empty support"
  )
})

test_that("range() of a support gives its outermost points.", {
  expect_equal(range(continuous(c(0, 1), c(3, 4))), c(0, 4))
  expect_equal(range(mixed(atoms = -1, continuous = c(0, Inf))), c(-1, Inf))
  expect_equal(range(discrete(c(2, 5, 9))), c(2, 9))
  expect_equal(range(continuous()), c(-Inf, Inf))
})

test_that("range() of the empty support is NA, without warning.", {
  expect_equal(range(empty_support()), c(NA_real_, NA_real_))
  expect_silent(range(empty_support()))
})

test_that("range() of a support rejects stray arguments.", {
  expect_error(range(continuous(c(0, 1)), 5), "expecting no arguments")
})

test_that("range() of a support agrees with range() of its distribution.", {
  d <- dst_pois(3)
  expect_equal(range(support(d)), range(d))
  d2 <- dst_unif(2, 7)
  expect_equal(range(support(d2)), range(d2))
})

test_that("The Null distribution has no support, and says so.", {
  n <- dst_null()
  expect_null(support(n))
  expect_identical(vtype(n), NA_character_)
  expect_equal(range(n), c(NA_real_, NA_real_))
  # It is the missing value of the distribution world: every query is NA.
  expect_identical(mean(n), NA_real_)
  expect_identical(eval_cdf(n, at = 1:3), rep(NA_real_, 3))
})

test_that("The Null distribution is built without `distribution()`.", {
  # It cannot declare a support, so it bypasses the user-facing constructor.
  # The bypass is internal, so `distribution()` remains the only public way in.
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_no_warning(dst_null())
  # Two Nulls are the same object, which is what lets verbs compare against it.
  expect_equal(dst_null(), dst_null())
})
