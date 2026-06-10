# distionary (development version)

- `dst_pearson3()` (and the underlying `ppearson3()`, `dpearson3()`,
  `qpearson3()`, `rpearson3()`) now accept a negative `shape`, giving the
  Pearson Type III distribution reflected about `location` --- the
  negatively-skewed, upper-bounded form.

- New support objects describe where a distribution places probability,
  tracking discrete atoms explicitly: `discrete()`, `continuous()` (a union of
  intervals), and `mixed()`. Pass one to `distribution()` via the new
  `.support` argument; the variable type (`vtype()`) and `range()` are derived
  from it. Retrieve a distribution's support with `support()`, and its parts
  with `atoms()` and `continuous_part()`. The `.vtype` argument is
  soft-deprecated in favour of `.support`: passing a string to it still works
  but now signals a (soft) deprecation warning when used directly.

- All built-in `dst_*()` families now carry a structured `.support` (replacing
  their `range`/`.vtype` specification), so `support()` works on them and their
  atoms are known explicitly (e.g. `dst_pois()` reports the atoms `0, 1, 2, ...`).

- Moments (`mean()`, `variance()`, `stdev()`, `skewness()`, `kurtosis()`) are
  now computed numerically for discrete and mixed distributions, not only
  continuous ones, when not supplied analytically. The computation uses the
  support: a sum over the atoms plus integration of the density over the
  continuous part. Infinite atomic supports are summed by walking outward until
  the tail is negligible (returning `NaN` if the moment does not converge).

- Re-exported the `discretes` series constructors used to specify atomic
  supports, so they are available without attaching the package: `natural0()`,
  `natural1()`, `integers()`, `arithmetic()`, and `as_discretes()`.

- `dst_lp3()` now supports negative skew on the log scale.
  Zero skew is treated as a log-normal distribution.

# distionary 0.1.1

* Replaced usage of the deprecated `ellipsis` package with `rlang`
  for checking expected use of ellipsis (thanks to @olivroy, PR #44).

* `dst_lp3()` now prints the created distribution object upon creation,
  matching the behaviour of other `dst_*()` functions.

# distionary 0.1.0

- Initial CRAN release.
