# Parse a combination-filter query to json

Create a json query sub-string based on a combination \\in\\ and \\and\\
constraints

## Usage

``` r
parse_tojson_filter(items, filter_type = "filter_items")
```

## Arguments

- items:

  items to be included in the "in" statement

- filter_type:

  type of filter being queried: "time_periods", "geographic_levels",
  "locations" or "filter_items"

## Value

String containing json form query with \\and\\-combination of different
filter selections

## Examples

``` r
eesyapi:::parse_tojson_filter(
  list(
    day_number = c("uLQo4", "qf0jG", "aMjLP"),
    reason = c("bBrtT", "ThjPJ", "hsHyW", "m2m9K"),
    education_phase = c("5UNdi", "crH31")
  )
) |>
  cat()
#> {
#> "and": [
#>     {
#>       "filters": {
#>         "in": [
#>           "uLQo4",
#>           "qf0jG",
#>           "aMjLP"
#>         ]
#>       }
#>     },
#>     {
#>       "filters": {
#>         "in": [
#>           "bBrtT",
#>           "ThjPJ",
#>           "hsHyW",
#>           "m2m9K"
#>         ]
#>       }
#>     },
#>     {
#>       "filters": {
#>         "in": [
#>           "5UNdi",
#>           "crH31"
#>         ]
#>       }
#>     }
#> ]
#> }
```
