# Range of Distribution

Range returns a vector of length two, with the minimum and maximum
values of the (support of the) distribution.

The `support` method gives the smallest and largest values the support
reaches — its two outermost points, taking the atoms and the continuous
regions together. Gaps in between are not represented.

An empty support reaches nothing, and its range is `c(Inf, -Inf)` — what
R gives for the range of nothing, and reversed on purpose, being the
identity for combining ranges.

## Usage

``` r
# S3 method for class 'dst'
range(distribution, ...)

# S3 method for class 'support'
range(support, ...)
```

## Arguments

- distribution:

  Distribution to compute range from.

- ...:

  Not used; vestige of the
  [`base::range()`](https://rdrr.io/r/base/range.html) S3 generic.

- support:

  A support object.

## Value

Vector of length two, containing the minimum and maximum values of a
distribution.

## Details

The range is read from the distribution's support (see
[`support()`](https://distionary.probaverse.com/reference/support.md)),
which is where a distribution says what values it reaches. In this it
behaves like
[`vtype()`](https://distionary.probaverse.com/reference/vtype.md):
derived, not declared.

It is still a property, and
[`eval_property()`](https://distionary.probaverse.com/reference/eval_property.md)
reaches it like any other, so code walking a list of property names need
not know which are stored and which are worked out. What it cannot be is
*stated*:
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
refuses a `range` entry, since a stated one would be consulted ahead of
the derived value and could disagree with it.

The Null distribution is a different case: it has no support at all, so
neither end is *known*, and its range is `c(NA, NA)` — still a vector of
length two, rather than a single `NA`. An empty support says there is
nothing to reach; the Null distribution says nothing at all.

## Examples

``` r
a <- dst_gp(1, 0.5)
b <- dst_unif(0, 1)
c <- dst_norm(3, 4)
range(a)
#> [1]   0 Inf
range(b)
#> [1] 0 1
range(c)
#> [1] -Inf  Inf
range(continuous(c(0, 1), c(3, 4)))
#> [1] 0 4
range(mixed(discrete = -1, continuous = c(0, Inf)))
#> [1]  -1 Inf
range(empty_support())
#> [1]  Inf -Inf
```
