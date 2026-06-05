# distionary (development version)

- Added the expectile function as a distributional representation, accessed
  with `eval_expectile()` and `enframe_expectile()`. Expectiles can be
  computed from a distribution's survival function and mean, and a
  distribution can now be defined through its expectile function alone, from
  which the cdf and the rest of the network are recovered. `dst_unif()`,
  `dst_exp()`, and `dst_t()` with 2 degrees of freedom carry their
  closed-form expectile functions.
- Added the conditional tail expectation `E[X | X > x]` as a distributional
  representation, accessed with `eval_cte()` and `enframe_cte()`. It can be
  computed from a distribution's survival function and mean, and a
  distribution can be defined through its conditional tail expectation alone,
  with the cdf and the rest of the network recovered by mean-residual-life
  inversion.
- `dst_lp3()` now supports negative skew on the log scale.
  Zero skew is treated as a log-normal distribution.

# distionary 0.1.1

* Replaced usage of the deprecated `ellipsis` package with `rlang`
  for checking expected use of ellipsis (thanks to @olivroy, PR #44).

* `dst_lp3()` now prints the created distribution object upon creation,
  matching the behaviour of other `dst_*()` functions.

# distionary 0.1.0

- Initial CRAN release.
