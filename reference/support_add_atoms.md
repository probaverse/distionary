# Add or Remove Atoms

Add atoms to a support, or take them away.

## Usage

``` r
support_add_atoms(support, atoms)

support_drop_atoms(support, atoms)
```

## Arguments

- support:

  A support object, or a distribution.

- atoms:

  Atoms to add or remove: a numeric vector, or a `discretes` object.
  Removing requires finitely many atoms, since they have to be
  enumerated; adding does not.

## Value

A support object.

## Details

Adding an atom that is already there changes nothing. Removing one that
is not there changes nothing either. Removing an atom does not disturb
the continuous part, so removing an atom sitting on an interval leaves
the interval whole.

## See also

Other Support algebra:
[`support_contains()`](https://distionary.probaverse.com/reference/support_contains.md),
[`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md),
[`support_transform()`](https://distionary.probaverse.com/reference/support_transform.md),
[`support_union()`](https://distionary.probaverse.com/reference/support_union.md)

## Examples

``` r
support_add_atoms(continuous(c(0, Inf)), 0)
#> <support: mixed>
#> -- atoms --
#> Numeric vector series of length 1:
#> 0
#> -- continuous --
#> [0, Inf]
support_drop_atoms(discrete(c(1, 2, 3)), 2)
#> Loading required namespace: testthat
#> <support: discrete>
#> -- atoms --
#> Union series of length 2:
#> 1, 3

# Removing an atom leaves the continuous part alone.
support_drop_atoms(mixed(discrete = 0, continuous = c(0, 1)), 0)
#> <support: continuous>
#> -- continuous --
#> [0, 1]
```
