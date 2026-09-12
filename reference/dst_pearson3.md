# Pearson Type III distribution

Makes a Pearson Type III distribution, which is a Gamma distribution,
but shifted.

## Usage

``` r
dst_pearson3(location, scale, shape)
```

## Arguments

- location:

  Location parameter, specifying the boundary of the distribution;
  single numeric. It is the left endpoint when `shape` is positive and
  the right endpoint when `shape` is negative.

- scale:

  Scale parameter of the Gamma distribution; single positive numeric.

- shape:

  Shape parameter of the Gamma distribution; single numeric. A negative
  value gives the distribution reflected about `location`: the Pearson
  Type III with negative skewness, upper-bounded at `location`.

## Value

A Pearson Type III distribution.

## Examples

``` r
dst_pearson3(1, 1, 1)
#> Pearson Type III distribution (continuous) 
#> --Parameters--
#> location    scale    shape 
#>        1        1        1 
# A negative shape reflects the distribution about `location`:
dst_pearson3(1, 1, -1)
#> Pearson Type III distribution (continuous) 
#> --Parameters--
#> location    scale    shape 
#>        1        1       -1 
```
