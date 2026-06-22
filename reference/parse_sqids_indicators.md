# Parse IDs in a set of indicators

The API uses unique IDs (sqids) to identify each indicator column. This
function parses those into the data creators' column names based on the
meta data stored on the API for the data set.

## Usage

``` r
parse_sqids_indicators(indicators, meta, verbose = FALSE)
```

## Arguments

- indicators:

  A set of indicator columns as taken from a data set downloaded from
  the API

- meta:

  Meta data for the data set as provided by
  [`get_meta()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_meta.md)

- verbose:

  Run in verbose mode with debugging messages

## Value

Data frame

## Examples

``` r
example_data_raw(group = "attendance") |>
  magrittr::use_series("values") |>
  eesyapi:::parse_sqids_indicators(get_meta(example_id(group = "attendance")))
#>    session_count
#> 1            799
#> 2            783
#> 3           1582
#> 4             15
#> 5             74
#> 6             15
#> 7              0
#> 8             11
#> 9              0
#> 10             0
#> 11             6
#> 12            58
#> 13             0
#> 14             0
#> 15            50
#> 16            10
#> 17             0
#> 18             0
#> 19             9
#> 20             0
#> 21             0
#> 22            14
#> 23             6
#> 24            47
#> 25             3
#> 26             0
#> 27             0
#> 28             0
#> 29             0
#> 30             0
#> 31             0
#> 32             0
```
