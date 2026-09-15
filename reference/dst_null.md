# Null Distribution

Sometimes it's convenient to work with a distribution object that is
akin to a missing value. This is especially true when programmatically
outputting distributions, such as when a distribution fails to fit to
data. This function makes such a distribution object. It always
evaluates to `NA`.

## Usage

``` r
dst_null()
```

## Value

A Null distribution.

## Details

The Null distribution is the missing value of the distribution world,
and every query about it answers `NA` in whatever type that query
returns: `NA_real_` from [`mean()`](https://rdrr.io/r/base/mean.html)
and the `eval_*()` functions, `NA_character_` from
[`vtype()`](https://distionary.probaverse.com/reference/vtype.md),
`c(NA, NA)` from [`range()`](https://rdrr.io/r/base/range.html), and no
support at all —
[`support()`](https://distionary.probaverse.com/reference/support.md)
returns `NULL`, R's absent-object value. It is also the one distribution
that [`is.na()`](https://rdrr.io/r/base/NA.html) finds; see
[`length.dst()`](https://distionary.probaverse.com/reference/scalar.md).

Because of that it is assembled with the package's low-level constructor
rather than through
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md).
A Null distribution cannot satisfy what
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
asks of a real one, since it has nothing to declare; building it here
keeps that bypass internal, so a distribution with no support cannot be
made through the front door.

## Examples

``` r
x <- dst_null()
mean(x)
#> [1] NA
eval_pmf(x, at = 1:10)
#>  [1] NA NA NA NA NA NA NA NA NA NA

# It is the distribution that `is.na()` finds.
is.na(x)
#> [1] TRUE
is.na(dst_norm(0, 1))
#> [1] FALSE

# Everything about it is missing, including its support.
vtype(x)
#> [1] NA
range(x)
#> [1] NA NA
support(x)
#> NULL
```
