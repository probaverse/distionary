# distionary (development version)

# distionary 0.1.1


Breaking changes:

- `dst_lp3()` no longer allows a negative skew parameter; the previous version
  resulted in nonsensical distributions.

Minor:

- `dst_lp3()` previously failed to print when called; this is fixed.
- p/d/q/r functions for the Pearson Type III and Log Pearson Type III
  distributions have been added; for example, `plp3()` and `rpearson3()`.

# distionary 0.1.0

- Initial CRAN release.
