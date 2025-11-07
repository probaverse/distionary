# Pearson Type III distribution

Makes a Pearson Type III distribution, which is a Gamma distribution,
but shifted.

## Usage

``` r
dst_pearson3(location, scale, shape)
```

## Arguments

- location:

  Location parameter, specifying how to shift the Gamma distribution;
  single numeric.

- scale:

  Scale parameter of the Gamma distribution; single positive numeric.

- shape:

  Shape parameter of the Gamma distribution; single positive numeric.

## Value

A Pearson Type III distribution.

## Examples

``` r
dst_pearson3(1, 1, 1)
#> Pearson Type III distribution (continuous) 
#> --Parameters--
#> location    scale    shape 
#>        1        1        1 
```
