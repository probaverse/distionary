# distionary (development version)

- `dst_pearson3()` (and the underlying `ppearson3()`, `dpearson3()`,
  `qpearson3()`, `rpearson3()`) now accept a negative `shape`, giving the
  Pearson Type III distribution reflected about `location` --- the
  negatively-skewed, upper-bounded form.

- **Breaking:** `range` can no longer be given as a distribution property. It
  is read from the support, so a stated one could only disagree with it --- and
  it would win, since a stated property is consulted before a derived one, so
  `range()` and `eval_quantile()` at 0 and 1 could report different endpoints
  for the same distribution. `distribution()` now rejects the name.

  `range()` and `vtype()` are now alike: both read off the support rather than
  going through the property network, and neither is reachable through
  `eval_property()`.

- **Breaking:** `distribution()` now requires a `.support`. A distribution has
  to say where it places probability, because that is the one thing distionary
  cannot work out from the representations it is given: a CDF says how much
  probability lies below a point, but not where the atoms are, nor where the
  distribution ends. Without it, quantiles at probability 0 and 1 had to be
  found by searching into the numerical tail --- returning a large finite
  number where the answer was infinite --- atoms could not be located at all,
  and moments could not be decomposed. It is the same bargain as declaring
  atoms: a little more to say up front, in exchange for exact answers instead
  of approximate ones.

  The one distribution without a support is `dst_null()`, which has nothing to
  place anywhere; `support()` returns `NULL` for it and nothing else.

  As a consequence there is now a single quantile algorithm rather than two.
  The separate routine for distributions lacking a support is gone, along with
  its restriction to continuous distributions, and the inverter itself no
  longer reaches into a distribution: it takes the function to invert and the
  facts it needs about the answer's shape. The 0- and 1-quantiles are settled
  by `eval_quantile()` from the support and never reach an algorithm at all.

- New support objects describe where a distribution places probability,
  tracking discrete atoms explicitly: `discrete()`, `continuous()` (a union of
  intervals), and `mixed()`. Pass one to `distribution()` via the new
  `.support` argument; the variable type (`vtype()`) and `range()` are derived
  from it. Retrieve a distribution's support with `support()`, and its parts
  with `atoms()` and `continuous_part()`. Test an object with `is_support()`,
  and get a support's outermost points with `range()`. There is also an
  `empty_support()`, tested by `is_empty_support()`: no distribution has one,
  and `distribution()` rejects it, but it exists so that operations on supports
  always have something to return. Its variable type is `"empty"`, which is a
  different claim from `"unknown"` --- empty says there is nowhere to place
  probability, unknown says nobody specified where. The `.vtype` argument is
  defunct, and errors with a message pointing at `.support`. A variable type
  cannot stand in for a support: `"discrete"` does not say *which* points carry
  mass and `"continuous"` does not say over what region, so there is no way to
  translate one into the other, and guessing would give quietly wrong answers
  rather than an error. The argument is kept in the signature only so that old
  code gets that message rather than `unused argument`. Its typo detection has
  gone with it, there being nothing left to typo.

- Supports can now be manipulated, not only built. `support_union()`,
  `support_restrict()`, and `support_transform()` combine, cut down, and map a
  support, with `support_shift()`, `support_scale()`, and
  `support_reciprocal()` covering the common maps without having to supply an
  inverse by hand. `support_add_atoms()` and `support_drop_atoms()` edit the
  atomic part, and `support_contains()` and `support_has_atom()` test whether a
  value belongs to a support at all or carries positive probability
  specifically. Every operation returns a support, so they compose: one that
  removes everything gives `empty_support()`. There is deliberately no
  intersection, which has not been needed.

- All built-in `dst_*()` families now carry a structured `.support` (replacing
  their `range`/`.vtype` specification), so `support()` works on them and their
  atoms are known explicitly (e.g. `dst_pois()` reports the atoms `0, 1, 2, ...`).

- Moments (`mean()`, `variance()`, `stdev()`, `skewness()`, `kurtosis()`) are
  now computed numerically for discrete and mixed distributions, not only
  continuous ones, when not supplied analytically. The computation uses the
  support: a sum over the atoms plus integration of the density over the
  continuous part. Infinite atomic supports are walked in batches until the
  tail is negligible, partitioning at any finite accumulation points so that
  atoms on the far side of an accumulation point are still counted. A moment
  that does not converge returns `NaN`.

- Quantiles computed through the network --- that is, for a distribution with
  no quantile function of its own --- are considerably faster, now solving
  every requested probability in one vectorised bisection rather than running
  a separate search for each. They also use the support: a probability landing
  inside an atom's jump returns that atom *exactly*, and `p = 0` and `p = 1`
  are read from the ends of the support, so an unbounded distribution gives
  `-Inf` and `Inf` rather than a large finite number from the numerical tail.
  Discrete and mixed distributions raised an error on this path before, since
  their atoms could not be located; they are now exact. Distributions given a
  `.vtype` string and no support keep the previous per-probability algorithm,
  which remains restricted to continuous distributions and approximate at
  `p = 0` and `p = 1`. Internally the algorithm can now take either inverse of
  the CDF, the right as well as the left; `eval_quantile()` continues to give
  the left inverse, and the choice is not yet exposed there.

- Re-exported the `discretes` series constructors used to specify atomic
  supports, so they are available without attaching the package: `natural0()`,
  `natural1()`, `integers()`, `arithmetic()`, and `as_discretes()`.

- `dst_lp3()` now supports negative skew on the log scale.
  Zero skew is treated as a log-normal distribution.

- New vignette, "The Support of a Distribution", covering what a support is,
  how to build, inspect, and manipulate one, and what tracking atoms buys when
  evaluating a distribution.

# distionary 0.1.1

* Replaced usage of the deprecated `ellipsis` package with `rlang`
  for checking expected use of ellipsis (thanks to @olivroy, PR #44).

* `dst_lp3()` now prints the created distribution object upon creation,
  matching the behaviour of other `dst_*()` functions.

# distionary 0.1.0

- Initial CRAN release.
