# Changelog

## distionary 0.2.0

This cycle adds support objects: a distribution now says where it places
its probability, and the routines computing from it — quantiles and
moments — use that to handle atoms exactly rather than approximately.
Code that uses the built-in `dst_*()` distributions is unaffected. Most
of the breaking changes are in
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md),
so they reach only distributions built by hand; the exception is
[`length()`](https://rdrr.io/r/base/length.html) and
[`is.na()`](https://rdrr.io/r/base/NA.html), which now answer about the
distribution itself.

### Breaking changes

- [`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
  now requires a `.support`, saying where the distribution places
  probability. See
  [`?distribution`](https://distionary.probaverse.com/reference/distribution.md),
  and the “The Support of a Distribution” vignette for why it is asked
  for.

- `.vtype` is defunct, and errors with a message pointing at `.support`.
  The variable type is derived from the support.

- `range` and `vtype` are derived from the support rather than stated,
  and
  [`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
  refuses them as entries. Both remain properties, reachable through
  [`eval_property()`](https://distionary.probaverse.com/reference/eval_property.md)
  like any other.

- [`length()`](https://rdrr.io/r/base/length.html),
  [`is.na()`](https://rdrr.io/r/base/NA.html) and
  [`as.list()`](https://rdrr.io/r/base/list.html) answer about the
  distribution rather than about the list of properties it is built
  from: [`length()`](https://rdrr.io/r/base/length.html) gives 1, not
  11, and [`is.na()`](https://rdrr.io/r/base/NA.html) gives a single
  logical, `TRUE` for the Null distribution. The properties are
  unchanged; see
  [`?length.dst`](https://distionary.probaverse.com/reference/scalar.md).

### Supports

- New support objects say where a distribution places probability,
  tracking its atoms explicitly:
  [`discrete()`](https://distionary.probaverse.com/reference/support-construction.md),
  [`continuous()`](https://distionary.probaverse.com/reference/support-construction.md),
  and `mixed(discrete =, continuous =)`. Retrieve one with
  [`support()`](https://distionary.probaverse.com/reference/support.md),
  its two parts with
  [`atoms()`](https://distionary.probaverse.com/reference/atoms.md) and
  [`regions()`](https://distionary.probaverse.com/reference/atoms.md),
  and test one with
  [`is_support()`](https://distionary.probaverse.com/reference/is_support.md).

- [`empty_support()`](https://distionary.probaverse.com/reference/empty_support.md),
  with
  [`is_empty_support()`](https://distionary.probaverse.com/reference/is_support.md).
  No distribution has an empty support, but operations on supports need
  something to return.

- Supports can now be manipulated, not only built:
  [`support_union()`](https://distionary.probaverse.com/reference/support_union.md),
  [`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md)
  and
  [`support_transform()`](https://distionary.probaverse.com/reference/support_transform.md),
  with
  [`support_shift()`](https://distionary.probaverse.com/reference/support_transform.md),
  [`support_scale()`](https://distionary.probaverse.com/reference/support_transform.md)
  and
  [`support_reciprocal()`](https://distionary.probaverse.com/reference/support_transform.md)
  for the common maps;
  [`support_add_atoms()`](https://distionary.probaverse.com/reference/support_add_atoms.md)
  and
  [`support_drop_atoms()`](https://distionary.probaverse.com/reference/support_add_atoms.md);
  and
  [`support_contains()`](https://distionary.probaverse.com/reference/support_contains.md)
  and
  [`support_has_atom()`](https://distionary.probaverse.com/reference/support_contains.md).
  There is deliberately no intersection.

- All built-in `dst_*()` families carry a support, so their atoms are
  known explicitly —
  [`dst_pois()`](https://distionary.probaverse.com/reference/dst_pois.md)
  reports the atoms `0, 1, 2, ...`.

- Re-exported the `discretes` constructors used to describe atomic
  supports, so they work without attaching that package:
  [`natural0()`](https://discretes.netlify.app/reference/integers.html),
  [`natural1()`](https://discretes.netlify.app/reference/integers.html),
  [`integers()`](https://discretes.netlify.app/reference/integers.html),
  [`arithmetic()`](https://discretes.netlify.app/reference/arithmetic.html)
  and
  [`as_discretes()`](https://discretes.netlify.app/reference/as_discretes.html).

### Evaluation

- Moments ([`mean()`](https://rdrr.io/r/base/mean.html),
  [`variance()`](https://distionary.probaverse.com/reference/moments.md),
  [`stdev()`](https://distionary.probaverse.com/reference/moments.md),
  [`skewness()`](https://distionary.probaverse.com/reference/moments.md),
  [`kurtosis()`](https://distionary.probaverse.com/reference/moments.md))
  are computed numerically for discrete and mixed distributions, not
  only continuous ones: a sum over the atoms plus integration over the
  regions. Infinite atomic supports are walked outward, and a moment
  that never settles returns `NaN`.

- Quantiles derived through the property network are considerably
  faster, solving every requested probability in one vectorised
  bisection rather than a separate search for each. A probability
  landing inside an atom’s jump returns that atom exactly, and the 0-
  and 1-quantiles come from the support, so an unbounded distribution
  gives `-Inf` and `Inf`. Discrete and mixed distributions raised an
  error on this path before.

### Distributions

- [`dst_pearson3()`](https://distionary.probaverse.com/reference/dst_pearson3.md),
  and the underlying
  [`ppearson3()`](https://distionary.probaverse.com/reference/pearson3_raw.md),
  [`dpearson3()`](https://distionary.probaverse.com/reference/pearson3_raw.md),
  [`qpearson3()`](https://distionary.probaverse.com/reference/pearson3_raw.md)
  and
  [`rpearson3()`](https://distionary.probaverse.com/reference/pearson3_raw.md),
  accept a negative `shape`: the Pearson Type III reflected about
  `location`, giving the negatively-skewed, upper-bounded form.

- [`dst_lp3()`](https://distionary.probaverse.com/reference/dst_lp3.md)
  supports negative skew on the log scale. Zero skew is treated as a
  log-normal distribution.

### Documentation

- New vignette, “The Support of a Distribution”.

## distionary 0.1.1

CRAN release: 2026-04-27

- Replaced usage of the deprecated `ellipsis` package with `rlang` for
  checking expected use of ellipsis (thanks to
  [@olivroy](https://github.com/olivroy), PR
  [\#44](https://github.com/probaverse/distionary/issues/44)).

- [`dst_lp3()`](https://distionary.probaverse.com/reference/dst_lp3.md)
  now prints the created distribution object upon creation, matching the
  behaviour of other `dst_*()` functions.

## distionary 0.1.0

CRAN release: 2025-12-01

- Initial CRAN release.
