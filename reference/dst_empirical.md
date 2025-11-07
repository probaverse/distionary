# Empirical Distribution

An empirical distribution is a non-parametric way to estimate a
distribution using data. By default, it assigns equal probability to all
observations (this can be overridden with the `weights` argument).
Identical to
[`dst_finite()`](https://distionary.probaverse.com/reference/dst_finite.md)
with NA handling and with weights not needing to add to 1.

## Usage

``` r
dst_empirical(
  y,
  weights = 1,
  data = NULL,
  na_action_y = c("null", "drop", "fail"),
  na_action_w = c("null", "drop", "fail")
)
```

## Arguments

- y:

  \<`data-masking`\> Numeric vector representing the potential outcomes
  of the distribution.

- weights:

  \<`data-masking`\> Numeric vector of weights corresponding to to the
  outcomes `y`. These will be scaled so that they add up to 1.

- data:

  Optionally, a data frame to compute `y` and `weights` from. `NULL` if
  data are not coming from a data frame (the default).

- na_action_y, na_action_w:

  What should be done with `NA` entries in `y` and `w`eights? Character
  vector of length 1: one of `"fail"`, `"null"` (default), or `"drop"`.
  See details.

## Value

A finite distribution. If only one outcome, returns a degenerate
distribution. Returns a Null distribution if `NA` values are present and
`"null"` is specified as an NA action.

## Details

`y` and `weights` are recycled to have the same length, but only if one
of them has length 1 (via
[`vctrs::vec_recycle_common()`](https://vctrs.r-lib.org/reference/vec_recycle.html)).

`na_action_y` and `na_action_w` specify the NA action for `y` and
`weights`. Options are, in order of precedence:

- `"fail"`: Throw an error in the presence of `NA`.

- `"null"`: Return a Null distribution
  ([`dst_null()`](https://distionary.probaverse.com/reference/dst_null.md))
  in the presence of `NA`.

- `"drop"` (the default for `na_action_w`): Remove outcome-weight pairs
  having an `NA` value in the specified vector.

## See also

[`dst_finite()`](https://distionary.probaverse.com/reference/dst_finite.md)

## Examples

``` r
t <- -2:7
dst_empirical(t)
#> Finite distribution (discrete) 
#> --Parameters--
#> # A tibble: 10 × 2
#>    outcomes probs
#>       <int> <dbl>
#>  1       -2   0.1
#>  2       -1   0.1
#>  3        0   0.1
#>  4        1   0.1
#>  5        2   0.1
#>  6        3   0.1
#>  7        4   0.1
#>  8        5   0.1
#>  9        6   0.1
#> 10        7   0.1

# Using a data frame
df <- data.frame(time = c(NA, NA, t))
dst_empirical(time * 60, data = df)  # Null, since `NA` in `time`.
#> Null distribution (NA) 

# Drop NA `time` values.
dst_empirical(time * 60, data = df, na_action_y = "drop")
#> Finite distribution (discrete) 
#> --Parameters--
#> # A tibble: 10 × 2
#>    outcomes probs
#>       <dbl> <dbl>
#>  1     -120   0.1
#>  2      -60   0.1
#>  3        0   0.1
#>  4       60   0.1
#>  5      120   0.1
#>  6      180   0.1
#>  7      240   0.1
#>  8      300   0.1
#>  9      360   0.1
#> 10      420   0.1

# Weights explicit. Zero-weight outcomes ("-120") are gone.
df$w <- c(1, 1, 0:9)
dst_empirical(time * 60, w, data = df, na_action_y = "drop")
#> Finite distribution (discrete) 
#> --Parameters--
#> # A tibble: 9 × 2
#>   outcomes  probs
#>      <dbl>  <dbl>
#> 1      -60 0.0222
#> 2        0 0.0444
#> 3       60 0.0667
#> 4      120 0.0889
#> 5      180 0.111 
#> 6      240 0.133 
#> 7      300 0.156 
#> 8      360 0.178 
#> 9      420 0.2   

# "Null" takes precedence over "drop".
df$w <- c(NA, NA, 0:9)
df$time[1] <- -3
df$time[12] <- NA
dst_empirical(time, w, data = df, na_action_w = "null", na_action_y = "drop")
#> Null distribution (NA) 
dst_empirical(time, w, data = df, na_action_w = "drop", na_action_y = "null")
#> Null distribution (NA) 
dst_empirical(time, w, data = df, na_action_w = "drop", na_action_y = "drop")
#> Finite distribution (discrete) 
#> --Parameters--
#> # A tibble: 8 × 2
#>   outcomes  probs
#>      <dbl>  <dbl>
#> 1       -1 0.0278
#> 2        0 0.0556
#> 3        1 0.0833
#> 4        2 0.111 
#> 5        3 0.139 
#> 6        4 0.167 
#> 7        5 0.194 
#> 8        6 0.222 
```
