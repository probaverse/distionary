# Return Level Function

Compute return levels (quantiles) from a distribution by inputting
return periods. The return periods correspond to events that are
*exceedances* of a quantile, not non-exceedances.

## Usage

``` r
eval_return(distribution, at)

enframe_return(..., at, arg_name = ".arg", fn_prefix = "return", sep = "_")
```

## Arguments

- distribution, ...:

  A distribution, or possibly multiple distributions in the case of
  `...`.

- at:

  Vector of return periods \>=1.

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

## Details

This function is simply the quantile function evaluated at `1 - 1 / at`.

## See also

Other distributional representations:
[`eval_cdf()`](https://distionary.probaverse.com/reference/cdf.md),
[`eval_chf()`](https://distionary.probaverse.com/reference/chf.md),
[`eval_density()`](https://distionary.probaverse.com/reference/density.md),
[`eval_hazard()`](https://distionary.probaverse.com/reference/hazard.md),
[`eval_odds()`](https://distionary.probaverse.com/reference/odds.md),
[`eval_pmf()`](https://distionary.probaverse.com/reference/pmf.md),
[`eval_quantile()`](https://distionary.probaverse.com/reference/quantile.md),
[`eval_survival()`](https://distionary.probaverse.com/reference/survival.md)

## Examples

``` r
d <- dst_gp(24, 0.3)
eval_return(d, at = c(2, 25, 100, 200))
#> [1]  18.49155 130.12222 238.48574 312.10194
```
