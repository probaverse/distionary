# Test Membership of a Support

Is a value in the support at all, or an atom of it specifically?

## Usage

``` r
support_contains(support, at)

support_has_atom(support, at)
```

## Arguments

- support:

  A support object, or a distribution.

- at:

  Values to test. Vectorised.

## Value

A logical vector the same length as `at`.

## Details

`support_contains()` is `TRUE` for a value that is either an atom or
inside one of the continuous intervals. `support_has_atom()` is `TRUE`
only for the atoms, and so is the one to reach for when what matters is
whether a point carries positive probability.

Continuous intervals count their endpoints as contained. Those endpoints
carry no probability, so a value can be contained in a support without
being a point of positive mass — which is exactly the distinction
between these two functions.

## See also

Other Support algebra:
[`support_add_atoms()`](https://distionary.probaverse.com/reference/support_add_atoms.md),
[`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md),
[`support_transform()`](https://distionary.probaverse.com/reference/support_transform.md),
[`support_union()`](https://distionary.probaverse.com/reference/support_union.md)

## Examples

``` r
s <- mixed(discrete = 0, continuous = c(2, 5))
support_contains(s, at = c(0, 1, 3, 9))
#> [1]  TRUE FALSE  TRUE FALSE
support_has_atom(s, at = c(0, 1, 3, 9))
#> [1]  TRUE FALSE FALSE FALSE

support_has_atom(dst_pois(3), at = c(-1, 0, 2.5, 4))
#> [1] FALSE  TRUE FALSE  TRUE
```
