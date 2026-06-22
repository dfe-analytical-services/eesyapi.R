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
#>      la_name   la_code la_oldCode nat_name  nat_code   reg_name  reg_code
#> 1  Stockport E08000007        356  England E92000001 North West E12000002
#> 2  Stockport E08000007        356  England E92000001 North West E12000002
#> 3  Stockport E08000007        356  England E92000001 North West E12000002
#> 4  Stockport E08000007        356  England E92000001 North West E12000002
#> 5  Stockport E08000007        356  England E92000001 North West E12000002
#> 6  Stockport E08000007        356  England E92000001 North West E12000002
#> 7  Stockport E08000007        356  England E92000001 North West E12000002
#> 8  Stockport E08000007        356  England E92000001 North West E12000002
#> 9  Stockport E08000007        356  England E92000001 North West E12000002
#> 10 Stockport E08000007        356  England E92000001 North West E12000002
#> 11 Stockport E08000007        356  England E92000001 North West E12000002
#> 12 Stockport E08000007        356  England E92000001 North West E12000002
#> 13 Stockport E08000007        356  England E92000001 North West E12000002
#> 14 Stockport E08000007        356  England E92000001 North West E12000002
#> 15 Stockport E08000007        356  England E92000001 North West E12000002
#> 16 Stockport E08000007        356  England E92000001 North West E12000002
#> 17 Stockport E08000007        356  England E92000001 North West E12000002
#> 18 Stockport E08000007        356  England E92000001 North West E12000002
#> 19 Stockport E08000007        356  England E92000001 North West E12000002
#> 20 Stockport E08000007        356  England E92000001 North West E12000002
#> 21 Stockport E08000007        356  England E92000001 North West E12000002
#> 22 Stockport E08000007        356  England E92000001 North West E12000002
#> 23 Stockport E08000007        356  England E92000001 North West E12000002
#> 24 Stockport E08000007        356  England E92000001 North West E12000002
#> 25 Stockport E08000007        356  England E92000001 North West E12000002
#> 26 Stockport E08000007        356  England E92000001 North West E12000002
#> 27 Stockport E08000007        356  England E92000001 North West E12000002
#> 28 Stockport E08000007        356  England E92000001 North West E12000002
#> 29 Stockport E08000007        356  England E92000001 North West E12000002
#> 30 Stockport E08000007        356  England E92000001 North West E12000002
#> 31 Stockport E08000007        356  England E92000001 North West E12000002
#> 32 Stockport E08000007        356  England E92000001 North West E12000002
```
