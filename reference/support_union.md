# Combine Supports

The union of two or more supports: everything covered by any of them.

## Usage

``` r
support_union(...)
```

## Arguments

- ...:

  Supports to combine, or a single list of them. Distributions are
  accepted in place of supports. With no arguments, the result is
  [`empty_support()`](https://distionary.probaverse.com/reference/empty_support.md),
  which is the identity for this operation.

  If any of them has no support — a `NULL`, or
  [`dst_null()`](https://distionary.probaverse.com/reference/dst_null.md)
  — the result is `NULL`. A union cannot be known when one of the things
  being combined is not.

## Value

A support object.

## Details

The atomic parts are unioned as series, and the continuous parts are
pooled and merged back into canonical form, so touching or overlapping
intervals come out as one.

An atom that falls inside another support's continuous part stays an
atom. The two carry different kinds of probability, and one does not
absorb the other.

## See also

[`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md)
to cut a support down instead.

Other Support algebra:
[`support_add_atoms()`](https://distionary.probaverse.com/reference/support_add_atoms.md),
[`support_contains()`](https://distionary.probaverse.com/reference/support_contains.md),
[`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md),
[`support_transform()`](https://distionary.probaverse.com/reference/support_transform.md)

## Examples

``` r
support_union(continuous(c(0, 1)), continuous(c(0.5, 3)))
#> <support: continuous>
#> -- continuous --
#> [0, 3]
support_union(discrete(c(1, 2)), continuous(c(5, 6)))
#> <support: mixed>
#> -- atoms --
#> Numeric vector series of length 2:
#> 1, 2
#> -- continuous --
#> [5, 6]

# The identity.
support_union()
#> <support: empty>
```
