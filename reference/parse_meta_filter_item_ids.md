# Parse API meta to give the filter item codes

Parse API meta to give the filter item codes

## Usage

``` r
parse_meta_filter_item_ids(api_meta_filters, verbose = FALSE)
```

## Arguments

- api_meta_filters:

  Filter information provided by the API output

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

Data frame containing filter item codes matched to filter item labels
and col_name

## Examples

``` r
eesyapi:::get_meta_response(example_id())$filters |>
  eesyapi:::parse_meta_filter_item_ids()
#>   col_id        col_name           label item_id  item_label isAggregate
#> 1  BT7J3 education_phase Education phase   UyHRF All schools          NA
#> 2  BT7J3 education_phase Education phase   oUXmX     Primary          NA
#> 3  BT7J3 education_phase Education phase   rwhNj   Secondary          NA
#> 4  BT7J3 education_phase Education phase   GIxgr     Special          NA
```
