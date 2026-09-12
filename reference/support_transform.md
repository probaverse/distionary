# Transform a Support

Push a support through a strictly monotonic map, giving the support of
the transformed variable.

## Usage

``` r
support_transform(
  support,
  fun,
  inv,
  ...,
  increasing = TRUE,
  domain = c(-Inf, Inf),
  range = c(-Inf, Inf)
)

support_shift(support, by)

support_scale(support, by)

support_reciprocal(support)
```

## Arguments

- support:

  A support object, or a distribution.

- fun, inv:

  The map and its inverse. Both must be vectorised, and `fun` must be
  strictly monotonic on the support.

- ...:

  Not used; must be empty. Present so that the arguments below are
  matched by name.

- increasing:

  Whether `fun` is increasing. `FALSE` for a decreasing map, which
  reverses each interval's endpoints.

- domain, range:

  The domain and range of `fun`, needed to transform an atomic part that
  is described rather than enumerated.

- by:

  For `support_shift()` and `support_scale()`, the amount to shift or
  scale by.

## Value

A support object.

## Details

`support_shift()`, `support_scale()`, and `support_reciprocal()` are the
common cases, and avoid having to supply an inverse, a domain, and a
range by hand.

`support_reciprocal()` maps each side of zero separately, since `1 / x`
is monotonic on each side but not across the two. A support with an atom
at zero has no reciprocal, and is an error. Zero lying inside a region
is fine: a single point carries no probability there.

Scaling by zero sends every value to 0. Density that was spread over a
region is compressed onto that single point, and density compressed onto
a point is mass. So whatever the support was, the result has a mass at 0
and density nowhere: `discrete(0)`. Only an empty support, having
nothing to compress, stays empty.

It only works in that direction. A mass sits on one point and lands on
one point, so mass stays mass.

A strictly monotonic map stretches and shifts regions but never squashes
one down to a point, so density stays density and mass stays mass. That
is why `support_transform()` asks for a monotonic map, and why scaling
by zero — which is not one — is handled separately.

## See also

Other Support algebra:
[`support_add_atoms()`](https://distionary.probaverse.com/reference/support_add_atoms.md),
[`support_contains()`](https://distionary.probaverse.com/reference/support_contains.md),
[`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md),
[`support_union()`](https://distionary.probaverse.com/reference/support_union.md)

## Examples

``` r
support_shift(continuous(c(0, 1)), by = 5)
#> <support: continuous>
#> -- continuous --
#> [5, 6]
support_scale(discrete(natural0()), by = 2)
#> <support: discrete>
#> -- atoms --
#> Transformed series of length Inf:
#> 0, 2, 4, 6, 8, 10, ...

# A decreasing map reverses the region.
support_scale(continuous(c(1, 2)), by = -1)
#> <support: continuous>
#> -- continuous --
#> [-2, -1]

# Scaling by zero collapses everything onto a single atom.
support_scale(continuous(c(1, 2)), by = 0)
#> <support: discrete>
#> -- atoms --
#> Numeric vector series of length 1:
#> 0

# Reciprocal of a support spanning zero.
support_reciprocal(continuous(c(-2, 4)))
#> <support: continuous>
#> -- continuous --
#> [-Inf, -0.5] U [0.25, Inf]

# The general form.
support_transform(
  continuous(c(0, Inf)),
  fun = exp, inv = log,
  domain = c(0, Inf), range = c(1, Inf)
)
#> <support: continuous>
#> -- continuous --
#> [1, Inf]
```
