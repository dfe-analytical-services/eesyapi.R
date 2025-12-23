# Generate URL code for API data query

Generates the query code to append to the base API query URL for use by
query_dataset_get().

## Usage

``` r
api_url_query(
  indicators,
  time_periods = NULL,
  geographic_levels = NULL,
  locations = NULL,
  filter_items = NULL
)
```

## Arguments

- indicators:

  Indicators required as a string or vector of strings (required)

- time_periods:

  Time periods required as a string ("period\|code") or vector of
  strings

- geographic_levels:

  Geographic levels required as a string or vector of strings

- locations:

  Location code required as a string or vector of strings

- filter_items:

  Filter items required as a string or vector of strings

## Value

String containing data query string to append to GET data query URL

## Examples

``` r
eesyapi:::api_url_query(example_id("indicator"))
#> [1] "?indicators=uxo41"
```
