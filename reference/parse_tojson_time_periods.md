# Parse time_periods to json

Create a json query sub-string based on time periods constraints

## Usage

``` r
parse_tojson_time_periods(time_periods)
```

## Arguments

- time_periods:

  Time periods required as a string ("period\|code") or vector of
  strings

## Value

String containing json form query for time periods

## Examples

``` r
eesyapi:::parse_tojson_time_periods(c("2023|W25", "2024|W12"))
#> [1] "    {\n      \"timePeriods\": {\n        \"in\": [\n          {\n            \"period\": \"2023\",\n            \"code\": \"W25\"\n          },\n          {\n            \"period\": \"2024\",\n            \"code\": \"W12\"\n          }\n        ]\n      }\n    }"
```
