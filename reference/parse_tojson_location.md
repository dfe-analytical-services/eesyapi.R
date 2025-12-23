# Create json location search string from geographies

Create json location search string from geographies

## Usage

``` r
parse_tojson_location(geographies, include_comma = FALSE)
```

## Arguments

- geographies:

  Vector or data frame of search geographies

- include_comma:

  Include a comma before return strings (logical)

## Value

Vector of strings containing json location search string

## Examples

``` r
eesyapi:::parse_tojson_location(example_geography_query()) |> cat()
#> 
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "NAT",
#>             "code": "E92000001"
#>           }
#>         ]
#>       }
#>     } 
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "REG",
#>             "code": "E12000003"
#>           }
#>         ]
#>       }
#>     }
```
