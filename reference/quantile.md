# Distribution Quantiles

Access a distribution's quantiles.

## Usage

``` r
eval_quantile(distribution, at)

enframe_quantile(..., at, arg_name = ".arg", fn_prefix = "quantile", sep = "_")
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

## Details

The 0- and 1-quantiles are the ends of the distribution's support: the
0-quantile is its lower end and the 1-quantile its upper end. They are
read from the support (see
[`support()`](https://distionary.probaverse.com/reference/support.md))
rather than computed, so an unbounded distribution gives `-Inf` and
`Inf` instead of a large finite number found by searching into the tail.

When a quantile function does not exist, the remaining probabilities are
found by inverting the CDF by bisection: an interval known to contain
the solution is progressively cut in half, moving into whichever half
still contains it. The whole vector is solved together — one vectorized
CDF evaluation per step rather than one per probability — so evaluating
many quantiles at once is considerably faster than one at a time.
Because the support says where the atoms (discrete mass points) are, a
probability landing inside an atom's jump in the CDF is returned as that
atom exactly, rather than approximately. Tolerance is roughly 1e-9 in
the quantile value, unless the maximum number of iterations (200) is
reached.

## See also

Other distributional representations:
[`eval_cdf()`](https://distionary.probaverse.com/reference/cdf.md),
[`eval_chf()`](https://distionary.probaverse.com/reference/chf.md),
[`eval_density()`](https://distionary.probaverse.com/reference/density.md),
[`eval_hazard()`](https://distionary.probaverse.com/reference/hazard.md),
[`eval_odds()`](https://distionary.probaverse.com/reference/odds.md),
[`eval_pmf()`](https://distionary.probaverse.com/reference/pmf.md),
[`eval_return()`](https://distionary.probaverse.com/reference/return.md),
[`eval_survival()`](https://distionary.probaverse.com/reference/survival.md)

## Examples

``` r
d <- dst_unif(0, 4)
eval_quantile(d, at = 1:9 / 10)
#> [1] 0.4 0.8 1.2 1.6 2.0 2.4 2.8 3.2 3.6
enframe_quantile(d, at = 1:9 / 10)
#> # A tibble: 9 × 2
#>    .arg quantile
#>   <dbl>    <dbl>
#> 1   0.1      0.4
#> 2   0.2      0.8
#> 3   0.3      1.2
#> 4   0.4      1.6
#> 5   0.5      2  
#> 6   0.6      2.4
#> 7   0.7      2.8
#> 8   0.8      3.2
#> 9   0.9      3.6
```
