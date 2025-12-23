# Parse a filter-equal type query to json

Create a json query sub-string based on filter-equal constraints

## Usage

``` r
parse_tojson_filter_eq(items, filter_type = "filter_items")
```

## Arguments

- items:

  items to be included in the "in" statement

- filter_type:

  type of filter being queried: "time_periods", "geographic_levels",
  "locations" or "filter_items"

## Value

String containing json form query based on filter-equal-to constraints

## Examples

``` r
eesyapi:::parse_tojson_filter_eq("NAT", filter_type = "geographic_levels") |> cat()
#>         {
#>           "geographicLevels": {
#>             "eq": "NAT"
#>           }
#>         }
```
