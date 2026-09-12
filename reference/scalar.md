# A Distribution has Length 1

A distribution object is one distribution, so
[`length()`](https://rdrr.io/r/base/length.html) gives 1 and
[`is.na()`](https://rdrr.io/r/base/NA.html) gives a single logical.
[`as.list()`](https://rdrr.io/r/base/list.html) wraps the distribution
in a list of one.

## Usage

``` r
# S3 method for class 'dst'
length(x)

# S3 method for class 'dst'
is.na(x)

# S3 method for class 'dst'
as.list(x, ...)
```

## Arguments

- x:

  A distribution object.

- ...:

  Not used.

## Value

For [`length()`](https://rdrr.io/r/base/length.html), the number 1. For
[`is.na()`](https://rdrr.io/r/base/NA.html), a single logical. For
[`as.list()`](https://rdrr.io/r/base/list.html), a list containing the
one distribution.

## Details

A distribution is built out of a list of its properties — a CDF, a
density, a mean — and without these methods base R reports on that list
rather than on the distribution.
[`length()`](https://rdrr.io/r/base/length.html) counted the properties
and [`is.na()`](https://rdrr.io/r/base/NA.html) tested each one, so
`dst_norm(0, 1)` answered with eleven `FALSE`s. Neither answer was about
the distribution.

[`is.na()`](https://rdrr.io/r/base/NA.html) is `TRUE` for the Null
distribution
([`dst_null()`](https://distionary.probaverse.com/reference/dst_null.md))
and `FALSE` for every other. The Null distribution is the missing value
of the distribution world, so it is the one that
[`is.na()`](https://rdrr.io/r/base/NA.html) finds.

Note that the properties are still reachable, and are still what the
object is made of: `x[["cdf"]]` and `names(x)` are unchanged, and
[`eval_property()`](https://distionary.probaverse.com/reference/eval_property.md)
is the supported way to get at them. Only the questions asked of the
distribution *as a whole* now answer about the whole.

To hold several distributions, put them in a list; in a data frame, that
is a list-column. A distribution does not have length beyond one.

## Examples

``` r
d <- dst_norm(0, 1)
length(d)
#> [1] 1
is.na(d)
#> [1] FALSE

# The Null distribution is the missing one.
is.na(dst_null())
#> [1] TRUE

# Several distributions go in a list.
ds <- list(dst_norm(0, 1), dst_null(), dst_pois(3))
vapply(ds, is.na, logical(1))
#> [1] FALSE  TRUE FALSE
```
