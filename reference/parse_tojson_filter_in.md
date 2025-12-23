# Parse a filter-in type query to json

Create a json query sub-string based on filter-in constraints

## Usage

``` r
parse_tojson_filter_in(items, filter_type = "filter_items")
```

## Arguments

- items:

  items to be included in the "in" statement

- filter_type:

  type of filter being queried: "time_periods", "geographic_levels",
  "locations" or "filter_items"

## Value

String containing json form query based on filter-in constraints

## Examples

``` r
eesyapi:::parse_tojson_filter_in(c("NAT", "REG"), filter_type = "geographic_levels")
#> [1] "    {\n      \"geographicLevels\": {\n        \"in\": [\n          \"NAT\",\n          \"REG\"\n        ]\n      }\n    }"
```
