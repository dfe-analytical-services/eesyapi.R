# Parse API meta to give the filter columns

Parse API meta to give the filter columns

## Usage

``` r
parse_meta_filter_columns(api_meta_filters, verbose = FALSE)
```

## Arguments

- api_meta_filters:

  Filter information provided by the API output

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

data frame containing column names and labels

## Examples

``` r
eesyapi:::get_meta_response(example_id())$filters |>
  eesyapi:::parse_meta_filter_columns()
#>   col_id        col_name           label
#> 1  BT7J3 education_phase Education phase
```
