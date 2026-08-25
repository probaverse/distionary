# distionary (development version)

This cycle adds support objects: a distribution now says where it places its
probability, and the routines computing from it --- quantiles and moments ---
use that to handle atoms exactly rather than approximately. It also lets a
representation come in more than one *variant*, so that questions with more
than one reasonable answer --- which inverse of the CDF a quantile is, whether
a probability counts the point it is taken at --- can be asked either way.
Code that uses the built-in `dst_*()` distributions is unaffected, and every
variant defaults to the reading its function has always had. The breaking
changes are in `distribution()`, so they reach only distributions built by
hand, and in the two `prob_` functions.

## Breaking changes

- `distribution()` now requires a `.support`, saying where the distribution
  places probability. See `?distribution`, and the "The Support of a
  Distribution" vignette for why it is asked for.

- `.vtype` is defunct, and errors with a message pointing at `.support`. The
  variable type is derived from the support.

- `range` and `vtype` are derived from the support rather than stated, and
  `distribution()` refuses them as entries. Both remain properties, reachable
  through `eval_property()` like any other.

- `prob_left()` and `prob_right()` are deprecated in favour of
  `eval_prob_left()` and `eval_prob_right()`, which join the rest of the
  `eval_` family: they take `at` rather than `of`, they have `enframe_`
  counterparts, and they say which inequality they mean in words rather than
  as a logical `inclusive`. They are the CDF and the survival function under
  names that say which way they point.

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

## Variants

- A representation can now be evaluated in more than one *variant*: the
  `eval_` and `enframe_` functions take arguments for the versions of a
  representation that answer a different question about the same
  distribution. Each defaults to the reading it has always had.

  - `eval_quantile()` takes `side`, choosing which inverse of the CDF to
    take. The left inverse, the usual quantile function, remains the default;
    the right inverse is the far end of a stretch the CDF is flat over, which
    is where the two differ.
  - `eval_cdf()` and `eval_survival()` take `inequality`, choosing whether the
    point itself is counted. The defaults are the conventional ones --- the
    CDF weak, the survival function strict --- so the two continue to sum
    to 1. They differ only at an atom.
  - `eval_return()` takes `event`, choosing whether the event of interest is
    an exceedance of the return level or a shortfall below it, and
    `obs_per_period`, for quoting return periods in something other than
    observations of the variable: 365 to quote them in years for a variable
    observed daily. The rescaling counts an event occurring somewhere within a
    period and assumes the observations are independent of one another; it is
    not a conversion of the variable itself to a coarser time scale.
  - `eval_density()` and `eval_pmf()` take `definition`, choosing whether the
    representation must exist in the full sense (`"strict"`, which only a
    continuous or a discrete distribution respectively can satisfy) or may be
    read off the CDF (`"extended"`, the default).

- New `variants()` declares the variants a distribution provides itself, for
  use in `distribution()`:

  ``` r
  quantile = variants(function(p) qpois(p, 5), right = my_right_inverse)
  ```

  A representation given as a plain function provides the canonical variant,
  and only that. Any other variant is derived from the distribution's other
  representations, so every variant is available on every distribution, and
  declaring one says only that there is a better route to it than the one
  distionary would find on its own. The levels are checked as the distribution
  is built, against the `eval_` function's own signature, so a variant that
  does not exist is an error rather than a function nothing ever calls.

- `eval_property()` gains a `variant` argument, holding whatever departs from
  the canonical representation, such as `list(side = "right")`. Its `...` is
  unchanged, and still forwards to the representation as it always did.

- `eval_density()` of a distribution with nothing but atoms returns the
  derivative of its CDF --- 0 between the atoms, `NaN` on them --- where it
  previously refused for want of a density function. This is the extended
  reading described above, and mirrors what `eval_pmf()` has always done for a
  continuous distribution. A mixed distribution with no density supplied still
  refuses, since the height it spreads probability at over its regions is
  genuinely unknown.

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
