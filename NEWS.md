# distionary 0.2.0

This cycle adds support objects: a distribution now says where it places its
probability, and the routines computing from it --- quantiles and moments ---
use that to handle atoms exactly rather than approximately. Code that uses the
built-in `dst_*()` distributions is unaffected. Most of the breaking changes
are in `distribution()`, so they reach only distributions built by hand; the
exception is `length()` and `is.na()`, which now answer about the distribution
itself.

## Breaking changes

- `distribution()` now requires a `.support`, saying where the distribution
  places probability. See `?distribution`, and the "The Support of a
  Distribution" vignette for why it is asked for.

- `.vtype` is defunct, and errors with a message pointing at `.support`. The
  variable type is derived from the support.

- `range` and `vtype` are derived from the support rather than stated, and
  `distribution()` refuses them as entries. Both remain properties, reachable
  through `eval_property()` like any other.

- `length()`, `is.na()` and `as.list()` answer about the distribution rather
  than about the list of properties it is built from: `length()` gives 1, not
  11, and `is.na()` gives a single logical, `TRUE` for the Null distribution.
  The properties are unchanged; see `?length.dst`.

## Supports

- New support objects say where a distribution places probability, tracking
  its atoms explicitly: `discrete()`, `continuous()`, and
  `mixed(discrete =, continuous =)`. Retrieve one with `support()`, its two
  parts with `atoms()` and `regions()`, and test one with `is_support()`.

- `empty_support()`, with `is_empty_support()`. No distribution has an empty
  support, but operations on supports need something to return.

- Supports can now be manipulated, not only built: `support_union()`,
  `support_restrict()` and `support_transform()`, with `support_shift()`,
  `support_scale()` and `support_reciprocal()` for the common maps;
  `support_add_atoms()` and `support_drop_atoms()`; and `support_contains()`
  and `support_has_atom()`. There is deliberately no intersection.

- All built-in `dst_*()` families carry a support, so their atoms are known
  explicitly --- `dst_pois()` reports the atoms `0, 1, 2, ...`.

- Re-exported the `discretes` constructors used to describe atomic supports,
  so they work without attaching that package: `natural0()`, `natural1()`,
  `integers()`, `arithmetic()` and `as_discretes()`.

## Evaluation

- Moments (`mean()`, `variance()`, `stdev()`, `skewness()`, `kurtosis()`) are
  computed numerically for discrete and mixed distributions, not only
  continuous ones: a sum over the atoms plus integration over the regions.
  Infinite atomic supports are walked outward, and a moment that never settles
  returns `NaN`.

- Quantiles derived through the property network are considerably faster,
  solving every requested probability in one vectorised bisection rather than
  a separate search for each. A probability landing inside an atom's jump
  returns that atom exactly, and the 0- and 1-quantiles come from the support,
  so an unbounded distribution gives `-Inf` and `Inf`. Discrete and mixed
  distributions raised an error on this path before.

## Distributions

- `dst_pearson3()`, and the underlying `ppearson3()`, `dpearson3()`,
  `qpearson3()` and `rpearson3()`, accept a negative `shape`: the Pearson
  Type III reflected about `location`, giving the negatively-skewed,
  upper-bounded form.

- `dst_lp3()` supports negative skew on the log scale. Zero skew is treated as
  a log-normal distribution.

## Documentation

- New vignette, "The Support of a Distribution".

# distionary 0.1.1

* Replaced usage of the deprecated `ellipsis` package with `rlang`
  for checking expected use of ellipsis (thanks to @olivroy, PR #44).

* `dst_lp3()` now prints the created distribution object upon creation,
  matching the behaviour of other `dst_*()` functions.

# distionary 0.1.0

- Initial CRAN release.
