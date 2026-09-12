## R CMD check results

0 errors | 0 warnings | 0 notes

* This release adds support objects: a distribution now carries where it places
  its probability, and quantiles and moments use that to handle atoms exactly.
  It also makes breaking changes to `distribution()`, which now requires a
  `.support` and no longer accepts the defunct `.vtype`. See NEWS.md.

Checked with `R CMD check --as-cran` locally (macOS, R 4.6.0), and on GitHub
Actions (macOS, Windows and Ubuntu on R release; Ubuntu on R devel and
oldrel-1).

## Reverse dependencies

I checked the 3 reverse dependencies with revdepcheck. famish and probaverse
are unaffected. distplyr 0.2.0 is newly broken, with errors in its examples,
tests and vignettes: it builds distributions with the `.vtype` argument of
`distribution()`, which this release makes defunct.

I maintain distplyr. Its replacement, distplyr 0.3.0, uses the new `.support`
argument and passes its checks against this version. Because it declares
`distionary (>= 0.2.0)`, it cannot pass incoming checks until distionary 0.2.0
is published, so I will submit it as soon as this version is available on CRAN.
