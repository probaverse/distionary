# Log Normal Distribution

Makes a Log Normal distribution, which is the distribution of the
exponential of a Normally distributed random variable.

## Usage

``` r
dst_lnorm(meanlog, sdlog)
```

## Arguments

- meanlog:

  Mean of the log of the random variable; single numeric.

- sdlog:

  Standard deviation of the log of the random variable; single positive
  numeric.

## Value

A Log Normal distribution.

## Examples

``` r
dst_lnorm(0, 1)
#> Log Normal distribution (continuous) 
#> --Parameters--
#> meanlog   sdlog 
#>       0       1 
```
