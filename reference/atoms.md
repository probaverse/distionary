# What a Support Is Made Of

Take a support apart: `atoms()` gives the points it places mass on, and
`regions()` gives the intervals it spreads mass across. Each accepts a
support object or a distribution.

## Usage

``` r
atoms(x)

regions(x)
```

## Arguments

- x:

  A support object or a distribution.

## Value

For `atoms()`, a `discretes` object. For `regions()`, a two-column
numeric matrix of intervals (`lower`, `upper`), one row each.

## Details

These are the inverses of the constructors.
[`discrete()`](https://distionary.probaverse.com/reference/support-construction.md)
builds a support out of atoms and `atoms()` gives them back;
[`continuous()`](https://distionary.probaverse.com/reference/support-construction.md)
builds one out of regions and `regions()` gives those back. So the
discrete part of a support is `discrete(atoms(x))`, and its continuous
part is `continuous(regions(x))`.

A support with no atoms gives an empty `discretes` object rather than
nothing, and one with no regions gives a matrix of no rows, so neither
has to be guarded against before being used. That is a definite answer:
the support says there is no part of that kind.

[`dst_null()`](https://distionary.probaverse.com/reference/dst_null.md)
is different. It has no support at all, so there is nothing to take
apart and nothing is known — both give `NULL`, as
[`support()`](https://distionary.probaverse.com/reference/support.md)
does for it, rather than claiming it has no atoms. This mirrors
[`range()`](https://rdrr.io/r/base/range.html), which answers
`c(NA, NA)` for it instead of refusing.

## See also

Other Support:
[`empty_support()`](https://distionary.probaverse.com/reference/empty_support.md),
[`is_support()`](https://distionary.probaverse.com/reference/is_support.md),
[`support()`](https://distionary.probaverse.com/reference/support.md),
[`support-construction`](https://distionary.probaverse.com/reference/support-construction.md)

## Examples

``` r
atoms(mixed(discrete = 0, continuous = c(0, Inf)))
#> Numeric vector series of length 1:
#> 0
regions(continuous(c(0, 1), c(3, 4)))
#>      lower upper
#> [1,]     0     1
#> [2,]     3     4

# Either part can be put back together into a support of its own.
s <- mixed(discrete = c(0, 5), continuous = c(0, 10))
discrete(atoms(s))
#> <support: discrete>
#> -- atoms --
#> Numeric vector series of length 2:
#> 0, 5
continuous(regions(s))
#> <support: continuous>
#> -- continuous --
#> [0, 10]
```
