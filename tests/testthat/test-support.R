test_that("continuous() builds a continuous support, defaulting to the line.", {
  s <- continuous()
  expect_true(is_support(s))
  expect_equal(vtype_of_support(s), "continuous")
  expect_equal(unname(regions(s)), matrix(c(-Inf, Inf), nrow = 1))
})

test_that("continuous() canonicalizes: sorts and merges touching/overlapping.", {
  expect_equal(
    unname(regions(continuous(c(3, 4), c(0, 1), c(0.5, 2)))),
    matrix(c(0, 3, 2, 4), nrow = 2)
  )
  # Touching closed intervals merge.
  expect_equal(
    unname(regions(continuous(c(0, 1), c(1, 2)))),
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

test_that("all three constructors give the empty support from nothing.", {
  # None of them insists on being handed something: describing nothing is a
  # coherent thing to describe, and they agree on what it is.
  expect_identical(discrete(numeric(0)), empty_support())
  expect_identical(discrete(), empty_support())
  expect_identical(continuous(numeric(0)), empty_support())
  expect_identical(mixed(), empty_support())
  # `continuous()` with no argument is still the whole line, not nothing.
  expect_false(is_empty_support(continuous()))
})

test_that("mixed() builds a mixed support from terse or rich parts.", {
  s <- mixed(discrete = 0, continuous = c(0, Inf))
  expect_equal(vtype_of_support(s), "mixed")
  expect_equal(as.double(atoms(s)), 0)
  expect_equal(unname(regions(s)), matrix(c(0, Inf), nrow = 1))
  # Rich continuous via continuous().
  s2 <- mixed(
    discrete = discretes::natural0(),
    continuous = continuous(c(0, 1), c(3, 4))
  )
  expect_equal(nrow(regions(s2)), 2)
})

test_that("mixed() builds whatever the parts describe, empty halves and all.", {
  # The variable type is derived, so there is nothing to insist on. An empty
  # half just means the support does not have that kind of part, which is
  # what makes `mixed()` usable when the parts are computed rather than typed.
  expect_identical(mixed(continuous = continuous()), continuous())
  expect_identical(mixed(discrete = discrete(1:3)), discrete(1:3))
  expect_identical(mixed(discrete = numeric(0), continuous = c(0, 1)),
                   continuous(c(0, 1)))
  expect_identical(mixed(discrete = 1:3, continuous = numeric(0)),
                   discrete(1:3))
  # Neither half is the empty support, which is legal to build and refused
  # only when handed to a distribution.
  expect_true(is_empty_support(mixed()))
})

test_that("mixed() takes either half as raw parts or as a support.", {
  # The two arguments accept the same kinds of thing, each mirroring its own
  # constructor, so neither half is second class.
  built <- mixed(
    discrete = discrete(c(0, 5)),
    continuous = continuous(c(0, 10))
  )
  raw <- mixed(discrete = c(0, 5), continuous = c(0, 10))
  expect_equal(built, raw)
  expect_equal(
    mixed(discrete = discrete(c(0, 5)), continuous = c(0, 10)), raw
  )
})

test_that("mixed() rejects a support of the wrong kind for either half.", {
  expect_error(
    mixed(discrete = 0, continuous = discrete(c(1, 2))),
    "purely continuous"
  )
  expect_error(
    mixed(discrete = continuous(c(0, 1)), continuous = c(0, 1)),
    "purely discrete"
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

test_that("Every distribution built by distribution() carries a support.", {
  d <- distribution(
    cdf = function(x) x, density = function(x) 1,
    .support = continuous(c(0, 1))
  )
  expect_false(is.null(support(d)))
  expect_equal(vtype(d), "continuous")
})

test_that(".vtype is defunct in favour of .support.", {
  lifecycle::expect_defunct(
    distribution(
      cdf = function(x) x, density = function(x) 1,
      .support = continuous(), .vtype = "continuous"
    )
  )
})

test_that("Support accessors answer for Null rather than refusing.", {
  # The Null distribution is the only one without a support. Asking about its
  # parts is a fair question with a known answer -- nothing is known -- so it
  # answers, as `range()` does with `c(NA, NA)` and `mean()` with `NA`.
  n <- dst_null()
  expect_null(atoms(n))
  expect_null(regions(n))
  expect_equal(support_contains(n, 1:3), rep(NA, 3))
  expect_equal(support_has_atom(n, 1:3), rep(NA, 3))
  # Something that is not a support or a distribution is still an error: that
  # is a wrong argument, not an unanswerable question.
  expect_error(atoms(1:10), "support object or a distribution")
  expect_error(regions("x"), "support object or a distribution")
})

test_that("An empty support answers definitely, not with NA.", {
  # It is a support that says there is nowhere to place probability, which is
  # a claim. The Null distribution makes no claim at all, and the two must
  # not read the same.
  e <- empty_support()
  expect_equal(discretes::num_discretes(atoms(e)), 0)
  expect_equal(nrow(regions(e)), 0)
  expect_equal(support_contains(e, 1:2), c(FALSE, FALSE))
  expect_false(is.null(atoms(e)))
})

test_that("The algebra carries a missing support rather than refusing.", {
  # Restricting with no bounds is the identity, and the identity applied to a
  # Null distribution should give back what a Null distribution has: nothing.
  # The rest follow it.
  n <- dst_null()
  expect_null(support_restrict(n))
  expect_null(support_restrict(n, from = 0))
  expect_null(support_shift(n, by = 1))
  expect_null(support_scale(n, by = 2))
  expect_null(support_reciprocal(n))
  expect_null(support_add_atoms(n, 1))
  expect_null(support_drop_atoms(n, 1))
  # Absent anywhere in a union means absent overall: the answer cannot be
  # known if one of the things being combined is not.
  expect_null(support_union(n, continuous()))
})

test_that("A bare NULL carries too, so a chain does not break at step two.", {
  # `support()` gives `NULL` for a Null distribution, and feeding that onward
  # has to work or the absence is useless.
  expect_null(support_shift(support(dst_null()), by = 3))
  expect_null(support_union(NULL, continuous()))
  # A wrong argument is still a wrong argument.
  expect_error(support_shift("banana", by = 1), "support object or a")
})

test_that("empty_support() is empty, and knows it.", {
  e <- empty_support()
  expect_true(is_support(e))
  expect_true(is_empty_support(e))
  expect_equal(discretes::num_discretes(atoms(e)), 0)
  expect_equal(nrow(regions(e)), 0)
})

test_that("the empty support's variable type is 'empty', not 'unknown'.", {
  expect_equal(distionary:::vtype_of_support(empty_support()), "empty")
  expect_output(print(empty_support()), "empty")
})

test_that("is_empty_support() is FALSE for non-empty supports and non-supports.", {
  expect_false(is_empty_support(continuous(c(0, 1))))
  expect_false(is_empty_support(discrete(1:3)))
  expect_false(is_empty_support(mixed(discrete = 0, continuous = c(0, 1))))
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
  expect_equal(range(mixed(discrete = -1, continuous = c(0, Inf))), c(-1, Inf))
  expect_equal(range(discrete(c(2, 5, 9))), c(2, 9))
  expect_equal(range(continuous()), c(-Inf, Inf))
})

test_that("range() of the empty support follows R's own convention.", {
  # `range(numeric(0))` is `c(Inf, -Inf)`, and an empty support reaches
  # nothing in just the same way. Built rather than computed by `min()` of
  # nothing, so it does not carry that call's warnings.
  expect_equal(range(empty_support()), c(Inf, -Inf))
  expect_silent(range(empty_support()))
  # Reversed on purpose: it leaves another range alone when the two combine.
  other <- range(continuous(c(3, 9)))
  e <- range(empty_support())
  expect_equal(c(min(other[1], e[1]), max(other[2], e[2])), other)
})

test_that("An empty support and the Null distribution give different ranges.", {
  # Reaching nothing is a claim; the Null distribution makes none.
  expect_equal(range(empty_support()), c(Inf, -Inf))
  expect_equal(range(dst_null()), c(NA_real_, NA_real_))
  expect_false(identical(range(empty_support()), range(dst_null())))
})

test_that("range() of a support rejects stray arguments.", {
  expect_error(range(continuous(c(0, 1)), 5), "expecting no arguments")
})

test_that("range() of a support reaches the ends the support declares.", {
  # Asking a distribution and asking its support are the same call underneath,
  # both reading the hull, so comparing the two proves nothing. Compare
  # against the ends themselves.
  expect_equal(range(support(dst_pois(3))), c(0, Inf))
  expect_equal(range(support(dst_unif(2, 7))), c(2, 7))
  expect_equal(range(support(dst_norm(0, 1))), c(-Inf, Inf))
  expect_equal(range(support(dst_binom(5, 0.3))), c(0, 5))
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

test_that("`range` is not a name distionary recognises.", {
  # It is read from the support, so nothing consults a `range` entry. It is
  # kept, as any unrecognised name is, and is no more meaningful than one the
  # user invented.
  d <- distribution(
    cdf = function(x) stats::punif(x),
    density = stats::dunif,
    range = c(-99, 99),
    my_object = 42,
    .support = continuous(c(0, 1))
  )
  expect_true(is_distribution(d))
  expect_equal(range(d), c(0, 1))
  expect_equal(eval_property(d, "range"), c(-99, 99))
  expect_equal(eval_property(d, "my_object"), 42)
})

test_that("range() reads the support, and agrees with the quantiles.", {
  d <- distribution(
    cdf = function(x) stats::punif(x),
    density = stats::dunif,
    .support = continuous(c(0, 1))
  )
  expect_equal(range(d), c(0, 1))
  expect_equal(range(dst_norm(0, 1)), c(-Inf, Inf))
  # `eval_quantile()` settles 0 and 1 off the hull as well, so it is the same
  # call underneath and agreement with `range()` is guaranteed rather than
  # tested. Check it lands on the right values instead.
  expect_equal(eval_quantile(d, at = c(0, 1)), c(0, 1))
  expect_equal(eval_quantile(dst_pois(3), at = c(0, 1)), c(0, Inf))
})

test_that("range() of the Null distribution is NA.", {
  expect_equal(range(dst_null()), c(NA_real_, NA_real_))
})

test_that("`range` is derived, not a property, as `vtype` is.", {
  # Neither is reachable through the property network; both come off the
  # support instead.
  d <- dst_norm(0, 1)
  expect_null(eval_property(d, "range"))
  expect_null(eval_property(d, "vtype"))
})

test_that("A malformed interval says which kind of malformed it is.", {
  expect_error(continuous(c(3, 1)), "runs backwards")
  expect_error(continuous(c(2, 2)), "single point")
  # An endpoint computed from parameters can overflow to `Inf`, collapsing
  # both ends onto it. Worth its own message: nothing is backwards, and it is
  # not a point either.
  expect_error(continuous(c(Inf, Inf)), "same infinity")
  expect_error(continuous(c(-Inf, -Inf)), "same infinity")
  expect_error(dst_lp3(1000, 0.1, 4), "same infinity")
})
