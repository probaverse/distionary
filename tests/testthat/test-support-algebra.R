test_that("support_union() merges continuous parts into canonical form.", {
  expect_equal(
    continuous_part(support_union(continuous(c(0, 1)), continuous(c(0.5, 3)))),
    continuous_part(continuous(c(0, 3)))
  )
  # Touching intervals merge; disjoint ones do not.
  expect_equal(nrow(continuous_part(
    support_union(continuous(c(0, 1)), continuous(c(1, 2)))
  )), 1)
  expect_equal(nrow(continuous_part(
    support_union(continuous(c(0, 1)), continuous(c(2, 3)))
  )), 2)
})

test_that("support_union() combines the two kinds of part.", {
  s <- support_union(discrete(c(1, 2)), continuous(c(5, 6)))
  expect_true(support_has_atom(s, 1))
  expect_equal(continuous_part(s), continuous_part(continuous(c(5, 6))))
})

test_that("support_union() keeps an atom lying inside a continuous part.", {
  # Atoms and densities carry different probability; neither absorbs the other.
  s <- support_union(discrete(3), continuous(c(0, 10)))
  expect_true(support_has_atom(s, 3))
  expect_equal(continuous_part(s), continuous_part(continuous(c(0, 10))))
})

test_that("the empty support is the identity for union.", {
  expect_true(is_empty_support(support_union()))
  expect_true(is_empty_support(support_union(empty_support())))
  s <- continuous(c(0, 1))
  expect_equal(support_union(empty_support(), s), s)
  expect_equal(support_union(s, empty_support()), s)
})

test_that("support_union() accepts a list, and accepts distributions.", {
  from_list <- support_union(list(continuous(c(0, 1)), continuous(c(2, 3))))
  from_dots <- support_union(continuous(c(0, 1)), continuous(c(2, 3)))
  expect_equal(from_list, from_dots)
  expect_equal(support_union(dst_pois(3)), support(dst_pois(3)))
})

test_that("support_restrict() clips the continuous part.", {
  expect_equal(
    support_restrict(continuous(c(0, 10)), from = 3, to = 6),
    continuous(c(3, 6))
  )
  # Restricting wider than the support changes nothing.
  s <- continuous(c(0, 1))
  expect_equal(support_restrict(s, from = -5, to = 5), s)
})

test_that("support_restrict() honours include_from and include_to on atoms.", {
  kept <- support_restrict(discrete(natural0()), to = 4)
  expect_true(support_has_atom(kept, 4))
  dropped <- support_restrict(discrete(natural0()), to = 4, include_to = FALSE)
  expect_false(support_has_atom(dropped, 4))
  expect_true(support_has_atom(dropped, 3))
})

test_that("support_restrict() out of reach gives the empty support.", {
  expect_true(is_empty_support(
    support_restrict(continuous(c(0, 1)), from = 5, to = 6)
  ))
  expect_true(is_empty_support(
    support_restrict(discrete(c(1, 2)), from = 10, to = 20)
  ))
})

test_that("support_restrict() handles a mixed support.", {
  s <- support_restrict(
    mixed(atoms = c(0, 7), continuous = c(0, 10)),
    from = 1, to = 8
  )
  expect_false(support_has_atom(s, 0))
  expect_true(support_has_atom(s, 7))
  expect_equal(continuous_part(s), continuous_part(continuous(c(1, 8))))
})

test_that("support_shift() moves a support without reshaping it.", {
  expect_equal(support_shift(continuous(c(0, 1)), by = 5), continuous(c(5, 6)))
  s <- support_shift(discrete(natural0()), by = 2)
  expect_true(support_has_atom(s, 2))
  expect_false(support_has_atom(s, 1))
})

test_that("support_scale() reverses the interval for a negative factor.", {
  expect_equal(
    support_scale(continuous(c(1, 2)), by = -1),
    continuous(c(-2, -1))
  )
  expect_equal(support_scale(continuous(c(1, 2)), by = 3), continuous(c(3, 6)))
  s <- support_scale(discrete(natural0()), by = 2)
  expect_true(support_has_atom(s, 4))
  expect_false(support_has_atom(s, 3))
})

test_that("support_scale() by zero is an error.", {
  expect_error(support_scale(continuous(c(0, 1)), by = 0), "zero")
})

test_that("support_reciprocal() maps each side of zero separately.", {
  # 1/x on [-2, 4] gives (-Inf, -1/2] and [1/4, Inf).
  expect_equal(
    support_reciprocal(continuous(c(-2, 4))),
    continuous(c(-Inf, -0.5), c(0.25, Inf))
  )
  # Wholly positive stays in one piece.
  expect_equal(
    support_reciprocal(continuous(c(2, 4))),
    continuous(c(0.25, 0.5))
  )
})

test_that("support_reciprocal() rejects an atom at zero.", {
  expect_error(
    support_reciprocal(mixed(atoms = 0, continuous = c(1, 2))),
    "atom at zero"
  )
  # Zero inside a continuous part is fine: a point carries no mass there.
  expect_no_error(support_reciprocal(continuous(c(-1, 1))))
})

test_that("support_transform() applies a general monotonic map.", {
  expect_equal(
    support_transform(
      continuous(c(0, Inf)),
      fun = exp, inv = log, domain = c(0, Inf), range = c(1, Inf)
    ),
    continuous(c(1, Inf))
  )
})

test_that("support_add_atoms() adds without touching the continuous part.", {
  s <- support_add_atoms(continuous(c(0, Inf)), 0)
  expect_true(support_has_atom(s, 0))
  expect_equal(continuous_part(s), continuous_part(continuous(c(0, Inf))))
  # Adding an atom that is already there changes the set not at all, though
  # the series records it as a union.
  again <- support_add_atoms(discrete(c(1, 2)), 2)
  expect_equal(support_has_atom(again, c(1, 2, 3)), c(TRUE, TRUE, FALSE))
})

test_that("support_drop_atoms() removes atoms and leaves intervals alone.", {
  s <- support_drop_atoms(discrete(c(1, 2, 3)), 2)
  expect_false(support_has_atom(s, 2))
  expect_true(support_has_atom(s, c(1)))
  # The continuous part is untouched, so a mixed support can become continuous.
  m <- support_drop_atoms(mixed(atoms = 0, continuous = c(0, 1)), 0)
  expect_true(is_support(m))
  expect_equal(continuous_part(m), continuous_part(continuous(c(0, 1))))
  # Removing an absent atom leaves the set alone.
  intact <- support_drop_atoms(discrete(c(1, 2)), 9)
  expect_equal(support_has_atom(intact, c(1, 2)), c(TRUE, TRUE))
})

test_that("support_drop_atoms() refuses infinitely many atoms.", {
  expect_error(
    support_drop_atoms(discrete(natural0()), natural0()),
    "infinitely many"
  )
})

test_that("support_contains() covers intervals too, has_atom only atoms.", {
  s <- mixed(atoms = 0, continuous = c(2, 5))
  expect_equal(support_contains(s, at = c(0, 1, 3, 9)),
               c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(support_has_atom(s, at = c(0, 1, 3, 9)),
               c(TRUE, FALSE, FALSE, FALSE))
  # Interval endpoints are contained, but are not atoms.
  expect_true(support_contains(s, at = 2))
  expect_false(support_has_atom(s, at = 2))
})

test_that("membership tests work on distributions and on empty supports.", {
  expect_equal(support_has_atom(dst_pois(3), at = c(-1, 0, 2.5, 4)),
               c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(support_contains(empty_support(), at = c(0, 1)),
               c(FALSE, FALSE))
  expect_equal(support_has_atom(empty_support(), at = c(0, 1)),
               c(FALSE, FALSE))
})

test_that("the algebra is closed: every operation returns a support.", {
  e <- empty_support()
  expect_true(is_support(support_shift(e, by = 3)))
  expect_true(is_empty_support(support_shift(e, by = 3)))
  expect_true(is_empty_support(support_reciprocal(e)))
  expect_true(is_empty_support(support_add_atoms(e, numeric(0))))
  expect_true(is_empty_support(support_restrict(e, from = 0, to = 1)))
})

test_that("the algebra rejects stray arguments and non-supports.", {
  expect_error(support_restrict(continuous(c(0, 1)), 5), "must be empty")
  expect_error(
    support_transform(continuous(c(0, 1)), fun = exp, inv = log, TRUE),
    "must be empty"
  )
  expect_error(support_union(continuous(c(0, 1)), 1:5), "Expected a support")
})

test_that("the algebra refuses a distribution with no structured support.", {
  # Only the Null distribution qualifies now that a support is required.
  expect_error(support_restrict(dst_null(), from = 0), "no structured support")
})

test_that("shifting a support agrees with shifting its range.", {
  s <- mixed(atoms = -1, continuous = c(0, 4))
  expect_equal(range(support_shift(s, by = 10)), range(s) + 10)
})
