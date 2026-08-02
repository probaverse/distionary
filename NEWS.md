# distionary (development version)

- `dst_lp3()` now supports negative skew on the log scale.
  Zero skew is treated as a log-normal distribution.

# distionary 0.1.1

* Replaced usage of the deprecated `ellipsis` package with `rlang`
  for checking expected use of ellipsis (thanks to @olivroy, PR #44).

* `dst_lp3()` now prints the created distribution object upon creation,
  matching the behaviour of other `dst_*()` functions.

# distionary 0.1.0

- Initial CRAN release.
