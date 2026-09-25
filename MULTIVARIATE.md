# Multivariate distributions: working notes

Branch `feature/multivariate`, started 2026-09-24 from `main`. It supersedes
`add_multivariate`, which predates supports.

This file is where work pauses and resumes. Read it first when picking up.

## Status

All distionary-level work agreed on 2026-09-24 is built. The full suite is
green, `R CMD check` is clean (0/0/0), and `pkgdown::check_pkgdown()`
passes.

| Piece | Where |
|---|---|
| `support_product()`: expand-grid supports | `R/support-multivariate.R` |
| `discrete(<data frame>)`: finite point sets | same |
| `support_map()`, `support_affine()`: supports as images of maps | `R/support-map.R` |
| `"singular"` variable type (spans fewer dimensions than variables) | same, and `vtype_of_support_mv()` |
| `dimension()`, `variables()` | `R/support-multivariate.R` |
| `eval_{bi,mv}_{cdf,survival,density,pmf}()`, with `given =` | `R/eval_mv.R` |
| Multivariate property network (`eval_mv_*_from_network`) | `R/eval_mv_network.R` |
| `marginal()` | `R/marginal.R` |
| `conditional` property network (the verb is distplyr's `conditional()`) | `R/conditional.R` |
| `prob_bi()`, `prob_mv()` | `R/prob_mv.R` |
| `dst_mv_norm()` (incl. singular cov), `dst_bi_norm()` | `R/dst_mv_norm.R` |
| `dst_mv_empirical()`, `dst_bi_empirical()` | `R/dst_mv_empirical.R` |
| `dst_mv_t()`, `dst_bi_t()` (any df, singular scale OK) | `R/dst_mv_t.R` |
| `dst_t()` gains `location`, `scale` (for the t's marginals) | `R/dst_t.R` |
| Vectorised bivariate Normal CDF `pbinorm()` (Sheppard-Drezner + GL) | `R/dst_mv_norm.R` |
| Vignette "Multivariate Distributions" (river-slice example) | `vignettes/multivariate.Rmd` |

## Decisions taken

- **`dimension()`, not `dim()`.** `dim()` has to agree with `length()`,
  which is 1. Dimension = number of variables, degenerate or not.
- **Representations take one argument per variable** (`function(x, y)`, or
  `function(...)`), called like `pmap()`, in `variables()` order.
- **Variable names live on the support.** Unnamed variables are `x1, x2,
  ...`, except in `dst_bi_*()`, which use `x, y` (Vincenzo, 2026-09-24).
- **Survival is P(all exceed)**, not 1 - CDF.
- **`given`** names the variables to the right of the bar. Argument order
  never changes. `"x"`/`"y"` are argument aliases in `eval_bi_*()`, but
  variable names win.
- **`prob_bi()` / `prob_mv()`** (`ineq` mandatory). Named after
  `prob_left()`/`prob_right()`, not "orthant": Vincenzo, 2026-09-25, said
  nobody knows that word.
- **Intrinsic `marginal = function(which)` and
  `conditional = function(given, at)`**, each returning a distribution.
  Otherwise the network works them out: finite by enumeration; continuous
  by densities, integrating when one variable is left.
- **Non-product supports are images of maps** (agreed). `support_affine()`
  derives margins exactly (a Minkowski sum of the scaled regions) and uses
  the matrix rank as the spanned dimension. `support_map()` needs
  `margins` stated, and assumes spanned dim = min(in, out).
- **Slices are conditionals on a derived variable.** Slicing (X, Y) at
  X + Y = s means conditioning (X, Y, S) on S. For Normals this is exact
  via singular covariance. The result stays bivariate (singular);
  `marginal()` reduces it.
- **Singular MVN**: rank from eigenvalues (tol: scale * p * sqrt(eps)).
  Rank 0 gives a point mass. The CDF uses mvtnorm GenzBretz under
  `with_fixed_seed()`, which restores the caller's RNG. Conditioning uses a
  pseudo-inverse; values off the support give `dst_null()`.
- **mvtnorm is in Suggests** (only the MVN CDF/survival needs it).
- `eval_property()` now calls network functions through a local name, so
  errors no longer print the whole distribution as the call.

- **Multivariate t CDF** is a 1D integral over the chi-square mixing
  variable of Normal CDFs, because `mvtnorm::pmvt()` needs integer df. It
  agrees with high-precision `pmvt()` to ~1e-10. Bivariate is fast thanks to
  `pbinorm()`, which is exact to 2e-16 for |rho| < 0.925; beyond that, and
  for p >= 3, it goes through `pmvnorm()`, which is slower (~0.1 s/point).
- **`dst_t()` parameters** list `location`/`scale` only when not 0/1, so
  the standard t is unchanged.

## Open questions

0. ~~Move `conditional()` to distplyr?~~ **Done 2026-09-25.** The verb
   now lives in distplyr (branch `feature/multivariate`) and calls
   `eval_property(d, "conditional", idx, at)`. distionary keeps the
   property and its network, which `given` evaluation needs. distionary
   tests reach it through `helper-conditional.R`.

1. **Independence binding** belongs in `couple` (agreed); distionary
   supplies `support_product()`.
2. **The slice verb in distplyr.** Given a linear map `A`, distplyr could
   offer a verb that appends derived variables (MVN: `A mu`, `A S A'`) and
   then conditions. For non-Normal joints, the slice density is
   `f(x, s - x) / f_S(s)`. That needs a general linear-transform verb.
3. **Mixed products** (count x amount): the support exists, but there is
   no joint density/PMF representation. Possibly a flavours question.
4. **`vtype` of affine images with a constant coordinate** (e.g. a segment
   with z fixed) reports `"singular"`. That is correct (no joint density),
   though one could argue for `"mixed"`.
5. **Conditional support from the network** is the product of the
   remaining margins, which may be larger than needed. It is documented.
6. **Not built**: moments beyond mean/covariance, `enframe_bi_*()`,
   plotting, event-conditioning (use ratios of `prob_mv()`), membership
   tests for map supports.

## Next steps

- Vincenzo's review.
- distplyr: the slice/linear-transform verb; mv-aware verbs.
- couple: binding with copulas (independence included).
