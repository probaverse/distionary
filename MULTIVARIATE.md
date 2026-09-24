# Multivariate distributions: working notes

Branch `feature/multivariate`, started 2026-09-24 from `main`. It supersedes
`add_multivariate`, which predates supports. Nothing is taken from that branch
except the idea of `bi`/`mv` evaluators.

This file is where work pauses and resumes. Read it first when picking up.

## Status

Built, tested (full suite green), and `R CMD check` clean:

| Piece | Where |
|---|---|
| `support_product()`: expand-grid supports, variables named by argument | `R/support-multivariate.R` |
| `discrete(<data frame>)`: finite point sets (needed by empirical) | same |
| `dimension()`, `variables()` | same |
| `eval_{bi,mv}_{cdf,survival,density,pmf}()`, with `given =` | `R/eval_mv.R` |
| Property network for multivariate distributions (`eval_mv_*_from_network`) | `R/eval_mv_network.R` |
| `marginal()` (stated, or worked out) | `R/marginal.R` |
| `prob_{bi,mv}_orthant()` | `R/prob_orthant.R` |
| `dst_mv_norm()`, `dst_bi_norm()` (mvtnorm in Suggests) | `R/dst_mv_norm.R` |
| `dst_mv_empirical()`, `dst_bi_empirical()` | `R/dst_mv_empirical.R` |
| Univariate-only functions refuse multivariate input, naming what to use | `assert_univariate()` in `R/utils.R` |
| `realise()` returns a data frame (tibble if installed), usable as `l` | `R/realise.R` |

## Decisions taken (review these)

- **`dimension()`, not `dim()`.** `dim()` means array extents, and it has to
  agree with `length()`, which is 1 (see scalar hygiene). `NROW()` would call a
  bivariate distribution two rows. Dimension = number of variables, which is
  the standard meaning for a p-dimensional random vector, degenerate or not.
- **Representations take one argument per variable**: `cdf = function(x, y)`.
  General-p families write `function(...)`. The evaluator calls them
  like `pmap()` does. Arguments are matched by position, in `variables()` order.
- **Variable names live on the support** (the coordinates are the support's
  axes). Unnamed variables default to `x1, x2, ...` in every dimension,
  including the `bi` constructors.
- **Survival is P(all exceed)**, not 1 - CDF. That is the standard
  "joint survival function"; the two coincide only in 1D.
- **`given`** names the variables to the right of the bar, by name or
  position. Argument order never changes. In `eval_bi_*()`, `"x"`/`"y"` also
  work as argument aliases, but variable names win if they clash.
- **Orthants** (`prob_*_orthant()`, with `ineq` mandatory, like `inclusive`
  in `prob_left()`). "Orthant" is the technical name for these regions.
- **Intrinsic `marginal` and `conditional` properties**: a distribution may
  state `marginal = function(which)` and
  `conditional = function(given, at)`. Each returns a distribution. MVN states
  both. Otherwise the network works them out.
- **`vtype()` of a multivariate distribution describes the joint**:
  continuous / discrete / mixed ("neither").
- **mvtnorm is in Suggests.** It is only needed for the MVN CDF/survival.
  Deterministic algorithms are used (TVPACK for p <= 3, Miwa for p <= 20).
- **Singular covariances are refused for now.** They are the natural home of
  slices (see below).

## Deviation from the brief

The brief was to build only expand-grid supports. The empirical distribution
cannot live on one: n points in p dimensions would become a grid of n^p.
So `discrete()` also accepts a data frame of points. That is the minimum;
nothing else non-product is built.

## Open questions, for discussion

1. **Non-product supports (triangles, slices).** See the chat reply of
   2026-09-24. In short: constraints on a product (`y <= x`) are easy to state
   but opaque (membership only: no marginals, no integration). A
   parametrisation / pushforward (the support of `(X, X*Z)` is the image of
   a product under a map) covers both the triangle and the slice. It also
   says what dimension the support has. That is the recommendation.
2. **Slicing `x + y = s`.** Recommend: the result stays bivariate
   (`dimension()` 2); internally a 1D distribution plus an affine map.
   Reducing it is just `marginal()`. For Gaussians, this is a singular MVN.
3. **Independence binding.** Recommend `couple` (independence copula as an
   explicit choice), not distionary. distionary supplies `support_product()`.
4. **Default variable names.** `x1, x2` everywhere, or `x, y` for bivariate?
5. **Mixed products** (discrete x continuous): the support exists, but there
   is no "density" for the joint yet. It is a density w.r.t. counting x
   Lebesgue. This is where flavours might come in.
6. **Moments beyond mean and covariance** for multivariate: not built.
   Neither are `enframe_bi_*()`, plotting, or event-conditioning (`X > x`;
   use ratios of orthants).

## Next steps (in order)

1. Vincenzo reviews names and the decisions above.
2. A vignette: "Multivariate distributions".
3. Slices of an MVN via singular covariance (closed form, and the river
   use case).
4. The pushforward support representation, if item 1 of the open questions
   lands that way.
