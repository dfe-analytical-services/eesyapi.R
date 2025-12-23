# Create .in query string for URL queries

Outputs a URL query string containing timePeriods.in=...,
geographicLevels.in=..., etc for use with querying a data set via GET.

## Usage

``` r
parse_tourl_filter_in(items, filter_type)
```

## Arguments

- items:

  items to be included in the "in" statement

- filter_type:

  type of filter being queried: "time_periods", "geographic_levels",
  "locations" or "filter_items"

## Value

Query string for use in URL based API queries

## Examples

``` r
eesyapi:::parse_tourl_filter_in(c("2024|W11", "2024|W12"), filter_type = "time_periods")
#> [1] "timePeriods.in=2024|W11%2C2024|W12&"
```
