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
#> 1       12085186
#> 2          70069
#> 3       12155255
#> 4        1115334
#> 5         813326
#> 6         302008
#> 7         335672
#> 8       13270589
#> 9        5781997
#> 10       6004234
#> 11      11786231
#> 12        298955
#> 13        669932
#> 14         43893
#> 15           268
#> 16           240
#> 17           768
#> 18             0
#> 19         18735
#> 20         45754
#> 21           773
#> 22          1370
#> 23         31592
#> 24         21701
#> 25             1
#> 26          8376
#> 27         32150
#> 28          2736
#> 29          5105
#> 30         36466
#> 31         36717
#> 32        213504
```
