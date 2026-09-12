# Restrict a Support to an Interval

Cut a support down to the part of it lying within `[from, to]`.

## Usage

``` r
support_restrict(
  support,
  ...,
  from = -Inf,
  to = Inf,
  include_from = TRUE,
  include_to = TRUE
)
```

## Arguments

- support:

  A support object, or a distribution.

- ...:

  Not used; must be empty. Present so that the arguments below are
  matched by name.

- from, to:

  Endpoints of the interval to restrict to.

- include_from, include_to:

  Whether the endpoints themselves are kept.

## Value

A support object.

## Details

The `include_*` flags apply to the atoms only. An endpoint of a
continuous interval carries no probability either way, so including or
excluding it makes no difference to the continuous part.

Restricting to a region the support does not reach gives
[`empty_support()`](https://distionary.probaverse.com/reference/empty_support.md),
which is the reason that object exists.

## See also

[`support_union()`](https://distionary.probaverse.com/reference/support_union.md)
to combine supports instead.

Other Support algebra:
[`support_add_atoms()`](https://distionary.probaverse.com/reference/support_add_atoms.md),
[`support_contains()`](https://distionary.probaverse.com/reference/support_contains.md),
[`support_transform()`](https://distionary.probaverse.com/reference/support_transform.md),
[`support_union()`](https://distionary.probaverse.com/reference/support_union.md)

## Examples

``` r
support_restrict(continuous(c(0, 10)), from = 3, to = 6)
#> <support: continuous>
#> -- continuous --
#> [3, 6]
support_restrict(discrete(natural0()), to = 4)
#> <support: discrete>
#> -- atoms --
#> Subset series of length 5:
#> 0, 1, 2, 3, 4

# Excluding an endpoint drops the atom sitting on it.
support_restrict(discrete(natural0()), to = 4, include_to = FALSE)
#> <support: discrete>
#> -- atoms --
#> Subset series of length 4:
#> 0, 1, 2, 3

# Restricting out of reach gives the empty support.
support_restrict(continuous(c(0, 1)), from = 5, to = 6)
#> <support: empty>
```
