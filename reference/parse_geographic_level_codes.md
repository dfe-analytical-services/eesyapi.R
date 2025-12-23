# Parse API geographic_levels

The API generally returns abbreviated and acronym versions of
geographic_levels in it's base output. This function converts those to
more hum-readable versions.

## Usage

``` r
parse_geographic_level_codes(geographic_levels, verbose = FALSE)
```

## Arguments

- geographic_levels:

  Vector of API returned geographic_levels

- verbose:

  Run in verbose mode with debugging messages

## Value

Data frame of expanded geographic levels

## Examples

``` r
c("NAT", "NAT", "REG", "LA", "LAD") |>
  eesyapi:::parse_geographic_level_codes()
#>           geographic_level
#> 1                 National
#> 2                 National
#> 3                 Regional
#> 4          Local authority
#> 5 Local authority district
```
