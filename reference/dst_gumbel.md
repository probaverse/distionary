# Gumbel Distribution

Makes a Gumbel distribution, which is a special case of the Generalised
Extreme Value (GEV) distribution when the shape parameter is 0.

## Usage

``` r
dst_gumbel(location, scale)
```

## Arguments

- location:

  Location parameter; single numeric.

- scale:

  Scale parameter; single positive numeric.

## Value

A Gumbel distribution.

## Examples

``` r
dst_gumbel(0, 1)
#> Gumbel distribution (continuous) 
#> --Parameters--
#> location    scale 
#>        0        1 
```
