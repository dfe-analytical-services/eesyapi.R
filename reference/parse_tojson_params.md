# Create a json query

Creates a json query for use when POST-ing a query to the API. This
takes time period, geography, filter item and indicator criteria and
produces a working json query as a single string. The result can be used
directly by post_dataset() or the output of
`parse_tojson_params(...) |> cat()` can be copied and pasted as the
"body" content in other API connection software (such as Postman) to
POST a query to the EES API.

## Usage

``` r
parse_tojson_params(
  indicators,
  time_periods = NULL,
  geographies = NULL,
  filter_items = NULL,
  page = 1,
  page_size = 1000,
  debug = FALSE,
  verbose = FALSE
)
```

## Arguments

- indicators:

  Indicators required as a string or vector of strings (required)

- time_periods:

  Time periods required as a string ("period\|code") or vector of
  strings

- geographies:

  String, vector or data frame containing the geographic levels and
  locations to be queried.

- filter_items:

  Filter items required as a string or vector of strings

- page:

  Page number of query results to return

- page_size:

  Number of results to return in a single query

- debug:

  Run POST query in debug mode. Logical, default = FALSE

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

String containing json query body for use with http POST request

## Examples

``` r
eesyapi:::parse_tojson_params(example_id("indicator")) |>
  cat()
#> {
#> 
#> "indicators": [
#>   "uxo41"
#> ],
#> "debug": false,
#> "page": 1,
#> "pageSize": 1000
#> }

eesyapi:::parse_tojson_params(
  example_id("indicator"),
  time_periods = "2024|W23",
  geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
  filter_items = c("pmRSo", "7SdXo")
) |>
  cat()
#> {
#> "criteria": {
#>   "and": [
#>     {
#>       "timePeriods": {
#>         "in": [
#>           {
#>             "period": "2024",
#>             "code": "W23"
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
#>   },
#>     {
#>       "filters": {
#>         "in": [
#>           "pmRSo",
#>           "7SdXo"
#>         ]
#>       }
#>     }
#>   ]
#> },
#> "indicators": [
#>   "uxo41"
#> ],
#> "debug": false,
#> "page": 1,
#> "pageSize": 1000
#> }

# Create a geographies data frame to find both of:
#   - England national level data
#   - all LAs in a specified region ("E12000004")
dfgeographies <- data.frame(
  geographic_level = c("NAT", "LA"),
  location_level = c("NAT", "REG"),
  location_id_type = c("code", "code"),
  location_id = c("E92000001", "E12000004")
)

eesyapi:::parse_tojson_params(
  example_id("indicator"),
  time_periods = "2024|W23",
  geographies = dfgeographies,
  filter_items = c("pmRSo")
) |>
  cat()
#> {
#> "criteria": {
#>   "and": [
#>     {
#>       "timePeriods": {
#>         "in": [
#>           {
#>             "period": "2024",
#>             "code": "W23"
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
#>             "eq": "NAT"
#>           }
#>         },
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
#>   ]
#>   },
#>         {
#>           "and": [
#>         {
#>           "geographicLevels": {
#>             "eq": "LA"
#>           }
#>         },
#>     {
#>       "locations": {
#>         "in": [
#>           {
#>             "level": "REG",
#>             "code": "E12000004"
#>           }
#>         ]
#>       }
#>     }
#>   ]
#>   }
#>     ]
#>   },
#>     {
#>       "filters": {
#>         "in": [
#>           "pmRSo"
#>         ]
#>       }
#>     }
#>   ]
#> },
#> "indicators": [
#>   "uxo41"
#> ],
#> "debug": false,
#> "page": 1,
#> "pageSize": 1000
#> }

# Create a filter list to find the combination of:
#   - day_number is in c("uLQo4", "qf0jG", "aMjLP") *and*
#   - reason is in c("bBrtT", "ThjPJ", "hsHyW", "m2m9K") *and*
#   - education_phase is in c("5UNdi", "crH31")
filter_list <- list(
  day_number = c("uLQo4", "qf0jG", "aMjLP"),
  reason = c("bBrtT", "ThjPJ", "hsHyW", "m2m9K"),
  education_phase = c("5UNdi", "crH31")
)

eesyapi:::parse_tojson_params(
  example_id("indicator"),
  time_periods = "2024|W23",
  geographies = "NAT|code|E92000001",
  filter_items = filter_list
) |>
  cat()
#> {
#> "criteria": {
#>   "and": [
#>     {
#>       "timePeriods": {
#>         "in": [
#>           {
#>             "period": "2024",
#>             "code": "W23"
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
#>             "eq": "NAT"
#>           }
#>         },
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
#>   ]
#>   }
#>     ]
#>   },
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
#>   ]
#> },
#> "indicators": [
#>   "uxo41"
#> ],
#> "debug": false,
#> "page": 1,
#> "pageSize": 1000
#> }
```
