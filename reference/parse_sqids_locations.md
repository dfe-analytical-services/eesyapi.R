# Parse location sqids

The API uses unique IDs (sqids) to identify each location in a data set.
This function parses those into the corresponding location codes and
names based on the meta data stored on the API for the data set.

## Usage

``` r
parse_sqids_locations(locations, meta, verbose = FALSE)
```

## Arguments

- locations:

  A set of location columns as taken from a data set downloaded from the
  API

- meta:

  Meta data for the data set as provided by
  [`get_meta()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_meta.md)

- verbose:

  Run in verbose mode with debugging messages

## Value

Data frame of parsed geography information

## Examples

``` r
example_data_raw() |>
  magrittr::use_series("locations") |>
  eesyapi:::parse_sqids_locations(get_meta(example_id(group = "attendance")))
#>    nat_name  nat_code
#> 1   England E92000001
#> 2   England E92000001
#> 3   England E92000001
#> 4   England E92000001
#> 5   England E92000001
#> 6   England E92000001
#> 7   England E92000001
#> 8   England E92000001
#> 9   England E92000001
#> 10  England E92000001
#> 11  England E92000001
#> 12  England E92000001
#> 13  England E92000001
#> 14  England E92000001
#> 15  England E92000001
#> 16  England E92000001
#> 17  England E92000001
#> 18  England E92000001
#> 19  England E92000001
#> 20  England E92000001
#> 21  England E92000001
#> 22  England E92000001
#> 23  England E92000001
#> 24  England E92000001
#> 25  England E92000001
#> 26  England E92000001
#> 27  England E92000001
#> 28  England E92000001
#> 29  England E92000001
#> 30  England E92000001
#> 31  England E92000001
#> 32  England E92000001
```
