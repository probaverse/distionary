# Probability Mass Function

Access a distribution's probability mass function (pmf).

## Usage

``` r
eval_pmf(distribution, at)

enframe_pmf(..., at, arg_name = ".arg", fn_prefix = "pmf", sep = "_")
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
[`eval_density()`](https://distionary.probaverse.com/reference/density.md),
[`eval_hazard()`](https://distionary.probaverse.com/reference/hazard.md),
[`eval_odds()`](https://distionary.probaverse.com/reference/odds.md),
[`eval_quantile()`](https://distionary.probaverse.com/reference/quantile.md),
[`eval_return()`](https://distionary.probaverse.com/reference/return.md),
[`eval_survival()`](https://distionary.probaverse.com/reference/survival.md)

## Examples

``` r
d <- dst_pois(5)
eval_pmf(d, at = c(1, 2, 2.5))
#> [1] 0.03368973 0.08422434 0.00000000
enframe_pmf(d, at = 0:4)
#> # A tibble: 5 × 2
#>    .arg     pmf
#>   <int>   <dbl>
#> 1     0 0.00674
#> 2     1 0.0337 
#> 3     2 0.0842 
#> 4     3 0.140  
#> 5     4 0.175  
eval_pmf(dst_norm(0, 1), at = -3:3)
#> [1] 0 0 0 0 0 0 0
```
