# Parse geographies to json

Create a json query sub-string based on location constraints

## Usage

``` r
parse_tojson_geographies(geographies)
```

## Arguments

- geographies:

  String, vector or data frame containing the geographic levels and
  locations to be queried.

## Value

String containing json form query for geographies

## Examples

``` r
eesyapi:::parse_tojson_geographies(c("NAT", "REG")) |>
  cat()
#>     {
#>       "or": [
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "NAT"
#>           }
#>         }
#>   ]
#>   },
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "REG"
#>           }
#>         }
#>   ]
#>   }
#>     ]
#>   }
eesyapi:::parse_tojson_geographies(c("NAT|id|dP0Zw", "REG|id|rg3Nj")) |>
  cat()
#>     {
#>       "or": [
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "NAT"
#>           }
#>         },
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "NAT",
#>             "id": "dP0Zw"
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
#>             "id": "rg3Nj"
#>           }
#>         ]
#>       }
#>     }
#>   ]
#>   }
#>     ]
#>   }
eesyapi:::parse_tojson_geographies(c("NAT|id|dP0Zw", "REG")) |>
  cat()
#>     {
#>       "or": [
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "NAT"
#>           }
#>         },
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "NAT",
#>             "id": "dP0Zw"
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
#>         }
#>   ]
#>   }
#>     ]
#>   }
eesyapi:::parse_tojson_geographies(c("NAT|id|dP0Zw", "REG")) |>
  cat()
#>     {
#>       "or": [
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "NAT"
#>           }
#>         },
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "NAT",
#>             "id": "dP0Zw"
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
#>         }
#>   ]
#>   }
#>     ]
#>   }
```
