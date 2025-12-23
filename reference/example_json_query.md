# Create an example json query string

Create an example json query string for use in examples and tests

## Usage

``` r
example_json_query(ees_environment = "prod")
```

## Arguments

- ees_environment:

  EES environment to connect to: "test", or "prod"

## Value

String containing an example json query

## Examples

``` r
example_json_query() |> cat()
#> {
#> "criteria": {
#>   "and": [
#>     {
#>       "timePeriods": {
#>         "in": [
#>           {
#>             "period": "2025",
#>             "code": "W3"
#>           }
#>         ]
#>       }
#>     },
#>     {
#>       "or": [
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "REG"
#>           }
#>         },
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "REG",
#>             "code": "E12000001"
#>           }
#>         ]
#>       }
#>     }
#>   ]
#>   },
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "REG"
#>           }
#>         },
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "REG",
#>             "code": "E12000002"
#>           }
#>         ]
#>       }
#>     }
#>   ]
#>   }
#>     ]
#>   },
#> {
#> "and": [
#>     {
#>       "filters": {
#>         "in": [
#>           "P9Aeb",
#>           "VPw5X"
#>         ]
#>       }
#>     },
#>     {
#>       "filters": {
#>         "in": [
#>           "rbyNj",
#>           "GBMgr"
#>         ]
#>       }
#>     },
#>     {
#>       "filters": {
#>         "in": [
#>           "5ezdi"
#>         ]
#>       }
#>     }
#> ]
#> }
#>   ]
#> },
#> "indicators": [
#>   "X9fKb"
#> ],
#> "debug": false,
#> "page": 1,
#> "pageSize": 1000
#> }
```
