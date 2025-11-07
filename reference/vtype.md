# Variable Type of a Distribution

Retrieve the variable type of a distribution, such as "continuous" or
"discrete".

## Usage

``` r
vtype(distribution)
```

## Arguments

- distribution:

  Distribution object.

## Value

Single character with the variable type.

## Examples

``` r
vtype(dst_beta(1, 2))
#> [1] "continuous"
vtype(dst_bern(0.4))
#> [1] "discrete"
vtype(distribution())
#> Warning: Full suite of distribution properties may not be accessible without specifying 'cdf', and either 'density' or 'pmf'.
#> [1] "unknown"
```
