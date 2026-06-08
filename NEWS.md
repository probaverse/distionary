# distionary (development version)

- New support objects describe where a distribution places probability,
  tracking discrete atoms explicitly: `discrete()`, `continuous()` (a union of
  intervals), and `mixed()`. Pass one to `distribution()` via the new
  `.support` argument; the variable type (`vtype()`) and `range()` are derived
  from it. Retrieve a distribution's support with `support()`, and its parts
  with `atoms()` and `continuous_part()`. The `.vtype` argument is
  soft-deprecated in favour of `.support`: passing a string to it still works
  but now signals a (soft) deprecation warning when used directly.

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
