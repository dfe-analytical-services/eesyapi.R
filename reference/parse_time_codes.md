# Parse API time codes

The API returns some abbreviated versions of time periods in it's base
output. This function converts those to more human-readable versions.

## Usage

``` r
parse_time_codes(time_periods, verbose = FALSE)
```

## Arguments

- time_periods:

  data frame of API returned time periods and indicators

- verbose:

  Run in verbose mode with debugging messages

## Value

Data frame of expanded time codes

## Examples

``` r
data.frame(
  code = c("W1", "W12", "Academic year"),
  period = c("2024", "2025", "202425")
) |>
  eesyapi:::parse_time_codes()
#>   time_period time_identifier
#> 1        2024          Week 1
#> 2        2025         Week 12
#> 3      202425   Academic year
```
