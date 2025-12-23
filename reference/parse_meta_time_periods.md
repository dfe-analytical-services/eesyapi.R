# Parse API meta to give the time periods

Parse API meta to give the time periods

## Usage

``` r
parse_meta_time_periods(api_meta_time_periods, verbose = FALSE)
```

## Arguments

- api_meta_time_periods:

  Time periods information provided by the API output

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

Data frame containing location item codes matched

## Examples

``` r
eesyapi:::get_meta_response(example_id())$timePeriods |>
  eesyapi:::parse_meta_time_periods()
#>    code period        label
#> 1    W8   2025  2025 Week 8
#> 2   W10   2025 2025 Week 10
#> 3   W12   2025 2025 Week 12
#> 4   W14   2025 2025 Week 14
#> 5   W15   2025 2025 Week 15
#> 6   W18   2025 2025 Week 18
#> 7   W20   2025 2025 Week 20
#> 8   W21   2025 2025 Week 21
#> 9   W24   2025 2025 Week 24
#> 10  W26   2025 2025 Week 26
#> 11  W28   2025 2025 Week 28
#> 12  W30   2025 2025 Week 30
```
