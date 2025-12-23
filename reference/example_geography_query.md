# Create an example geography-query data frame

Create an example geography-query data frame

## Usage

``` r
example_geography_query(level = "nat_yorks")
```

## Arguments

- level:

  Query level within available options, can be one of \\nat_yorks\\ or
  \\nat_yorks_yorkslas\\

## Value

Data frame containing an example geography query

## Examples

``` r
example_geography_query()
#>   geographic_level location_level location_id_type location_id
#> 1              NAT            NAT             code   E92000001
#> 2              REG            REG             code   E12000003
```
