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
| `prob(d, ..., given = )`: one way in for probabilities | `R/prob.R`, `R/prob-events.R`, `R/prob-evaluate.R` |
| `variables<-`; univariate distributions named (`x` by default) | `R/support-multivariate.R` |
| Reordering: `marginal()` with every variable, exact via `permute_distribution()` | `R/marginal.R` |
| `dst_mv_norm()` (incl. singular cov), `dst_bi_norm()` | `R/dst_mv_norm.R` |
| `dst_mv_empirical()`, `dst_bi_empirical()` | `R/dst_mv_empirical.R` |
| `dst_mv_t()`, `dst_bi_t()` (any df, singular scale OK) | `R/dst_mv_t.R` |
| One variable of a t: internal `univariate_t()`, "Location-Scale Student t" (`dst_t()` unchanged) | `R/dst_mv_t.R` |
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
- **`prob(d, event, given = )`** replaced `prob_bi()`/`prob_mv()`
  (2026-09-25). Events are `filter()`-style expressions: `&`, `|`, `!`, and
  unmentioned variables are free. The design is to evaluate the
  expression symbolically. Each variable is bound to a term (a linear form,
  or a non-linear function). A comparison becomes a condition on one
  quantity (`lhs - rhs`, put in canonical form), and an event becomes a
  union of boxes in those quantities (DNF, then inclusion-exclusion). What
  can be evaluated is exactly what joint distributions of the quantities
  can be found:
  - finite supports: anything, evaluated on the points (literally filter);
  - univariate discrete with infinitely many atoms: anything, by walking
    the atoms (`expect_over_support()` of the indicator);
  - a stated `linear` property (MVN, t): sums of multiples of variables;
  - otherwise, the variables themselves (marginals and the CDF).
  Anything else is refused with the quantity named. That answers
  Vincenzo's "false promise" worry: the rule is stated, and exactness is
  never traded away silently.
  `given`: `var == v` conditions on a value; `combo == v` (e.g.
  `r1 + r2 == s`) slices, via `linear` plus `conditional`; anything else
  is an event, divided out. The event may not mention a variable fixed
  by `given`.
- **Every distribution names its variables**, like a data frame's columns.
  A univariate one defaults to `x` (stored in attr `variable`, not on the
  support, since a univariate support describes values). `marginal()`,
  `prob()`'s conditionals and distplyr's `conditional()` keep names.
  `mean()`/`variance()`/`stdev()` take names from the distribution, so a
  renaming is not undone by a family's closures. `parameters()` are left
  as built. There are no purrr-style positional pronouns (`.x`, `..1`):
  names always exist, as in `filter()`.
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
  mvtnorm's `seed` argument (>= 1.2-0), which restores the caller's RNG itself. Conditioning uses a
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

- **`prob()` takes conditions in `...`**, joined by `&` as in `filter()`
  (2026-09-25). A named argument is refused with a `==` hint.

- **Self-review fixes (2026-09-25)**, after Vincenzo asked for a review
  as if by an outside reviewer:
  - `prob()` tracks every read of a variable (active bindings in the data
    mask). A variable that goes missing from the event means a function
    swallowed it (`is.na()`, `%in%` before it was supported, ...), so the
    condition is refused, naming the function. `%in%` is now supported.
  - Univariate speed is back level with main: `is_multivariate()` reads
    the support's class directly.
  - `realise(d, 0)` works. Products carry an `order`, so any reordering
    works (no more "paired variables" refusal). Reordering keeps unknown
    properties.
  - tidyselect was reverted (ed58e2f): standard evaluation only.
  - The evaluators' `given` became `known`; `prob()` keeps `given`.
  - `dst_t()` is back to df only. The bi families report mv parameters.
    `dst_mv_empirical(..., weights, data, na_action_y, na_action_w)` takes
    variables data-masked, and splices lists as `mix()` does. A named `l`
    in `eval_mv_*()` is matched by name, ignoring extras.
  - mvtnorm's own `seed` argument replaces `with_fixed_seed()`.
  - `==`/`!=` on atomless quantities are settled before DNF expansion.
  - distplyr: every verb and operator refuses multivariate input.

## Open questions

- **EARMARKED (Vincenzo, 2026-09-25): `parameters()` vs variable names.**
  `variables<-` renames variables but leaves `parameters()` as built (e.g.
  `dst_bi_norm()`'s `mean` stays named `x`, `y`). This matters once
  parameters form a data mask (families Phase 2). Options: variable names
  and parameter names are separate namespaces (the likely answer, since
  parameters describe the family, not the variables); or name-bearing
  parameters get renamed too, which can't be done generally.
- **The distionary/distplyr split** (Vincenzo raised it 2026-09-25). My
  recommendation was to fold distplyr's verbs into distionary as part of
  the families refactor, with distplyr as a re-export shim. Undecided.

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
   plotting, non-linear events on continuous distributions, membership
   tests for map supports.

## Next steps

- Vincenzo's review.
- distplyr: the slice/linear-transform verb; mv-aware verbs.
- couple: binding with copulas (independence included).
