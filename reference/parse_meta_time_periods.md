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
#> 3    W6   2026  2026 Week 6
#> 4    W8   2025  2025 Week 8
#> 5    W8   2026  2026 Week 8
#> 6   W10   2025 2025 Week 10
#> 7   W10   2026 2026 Week 10
#> 8   W12   2025 2025 Week 12
#> 9   W12   2026 2026 Week 12
#> 10  W14   2025 2025 Week 14
#> 11  W14   2026 2026 Week 14
#> 12  W15   2025 2025 Week 15
#> 13  W16   2026 2026 Week 16
#> 14  W18   2025 2025 Week 18
#> 15  W18   2026 2026 Week 18
#> 16  W20   2025 2025 Week 20
#> 17  W20   2026 2026 Week 20
#> 18  W21   2025 2025 Week 21
#> 19  W21   2026 2026 Week 21
#> 20  W24   2025 2025 Week 24
#> 21  W26   2025 2025 Week 26
#> 22  W28   2025 2025 Week 28
#> 23  W30   2025 2025 Week 30
```
