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
#> 1       12787248
#> 2          78936
#> 3       12866184
#> 4         897838
#> 5         609096
#> 6         288742
#> 7         350469
#> 8       13764022
#> 9        6127328
#> 10       6348550
#> 11      12475878
#> 12        311370
#> 13        455999
#> 14         48326
#> 15           297
#> 16           140
#> 17           656
#> 18             0
#> 19         19058
#> 20         46578
#> 21           394
#> 22          2106
#> 23         35533
#> 24         24085
#> 25             0
#> 26          8561
#> 27         36259
#> 28          4135
#> 29          5896
#> 30         27170
#> 31         39099
#> 32        209572
```
