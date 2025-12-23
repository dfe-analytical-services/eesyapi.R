# Parse an indicator-in type query to json

Create a json query sub-string based on indicator-in constraints

## Usage

``` r
parse_tojson_indicators(indicators)
```

## Arguments

- indicators:

  String or vector of strings containing indicator ids

## Value

A json query string to select a set of indicators

## Examples

``` r
eesyapi:::parse_tojson_indicators(example_id("indicator")) |>
  cat()
#> 
#> "indicators": [
#>   "uxo41"
#> ]
```
