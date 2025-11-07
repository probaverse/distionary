# Probability Density Function

Access a distribution's probability density function (pdf).

## Usage

``` r
eval_density(distribution, at)

enframe_density(..., at, arg_name = ".arg", fn_prefix = "density", sep = "_")
```

## Arguments

- distribution, ...:

  A distribution, or possibly multiple distributions in the case of
  `...`.

- at:

  Vector of values to evaluate the representation at.

- arg_name:

  For `enframe_`, name of the column containing the function arguments.
  Length 1 character vector.

- fn_prefix:

  For `enframe_`, name of the function to appear in the column(s).
  Length 1 character vector.

- sep:

  When `enframe`'ing more than one distribution, the character that will
  be separating the `fn_name` and the distribution name. Length 1
  character vector.

## Value

The evaluated representation in vector form (for `eval_`) with length
matching the length of `at`, and data frame or tibble form (for
`enframe_`) with number of rows matching the length of `at`. The `at`
input occupies the first column, named `.arg` by default, or the
specification in `arg_name`; the evaluated representations for each
distribution in `...` go in the subsequent columns (one column per
distribution). For a single distribution, this column is named according
to the representation by default (cdf, survival, quantile, etc.), or the
value in `fn_prefix`. For multiple distributions, unnamed distributions
are auto-named, and columns are named
`<fn_prefix><sep><distribution_name>` (e.g., `cdf_distribution1`).

## See also

Other distributional representations:
[`eval_cdf()`](https://distionary.probaverse.com/reference/cdf.md),
[`eval_chf()`](https://distionary.probaverse.com/reference/chf.md),
[`eval_hazard()`](https://distionary.probaverse.com/reference/hazard.md),
[`eval_odds()`](https://distionary.probaverse.com/reference/odds.md),
[`eval_pmf()`](https://distionary.probaverse.com/reference/pmf.md),
[`eval_quantile()`](https://distionary.probaverse.com/reference/quantile.md),
[`eval_return()`](https://distionary.probaverse.com/reference/return.md),
[`eval_survival()`](https://distionary.probaverse.com/reference/survival.md)

## Examples

``` r
d <- dst_unif(0, 4)
eval_density(d, at = 0:4)
#> [1] 0.25 0.25 0.25 0.25 0.25
enframe_density(d, at = 0:4)
#> # A tibble: 5 × 2
#>    .arg density
#>   <int>   <dbl>
#> 1     0    0.25
#> 2     1    0.25
#> 3     2    0.25
#> 4     3    0.25
#> 5     4    0.25
```
