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
#> 1    W2   2026  2026 Week 2
#> 2    W4   2026  2026 Week 4
#> 3    W8   2025  2025 Week 8
#> 4   W10   2025 2025 Week 10
#> 5   W12   2025 2025 Week 12
#> 6   W14   2025 2025 Week 14
#> 7   W15   2025 2025 Week 15
#> 8   W18   2025 2025 Week 18
#> 9   W20   2025 2025 Week 20
#> 10  W21   2025 2025 Week 21
#> 11  W24   2025 2025 Week 24
#> 12  W26   2025 2025 Week 26
#> 13  W28   2025 2025 Week 28
#> 14  W30   2025 2025 Week 30
```
