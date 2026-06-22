# Get data set versions

Get data set versions

## Usage

``` r
get_dataset_versions(
  dataset_id,
  detail = "light",
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  page_size = 20,
  page = NULL,
  verbose = FALSE
)
```

## Arguments

- dataset_id:

  ID of data set to be connected to. This is required if the endpoint is
  one of "get-dataset-versions", "get-summary", "get-meta", "get-csv",
  "get-data" or "post-data"

- detail:

  Level of detail to return. Given as a character string, it should be
  one of: "light" (default) or "full". Using "light" gives the
  following:

  - version - version number

  - type - version type (major or minor)

  - total_rows - total number of rows in the listed version

  - release_date - release date of the data set version

  - release_name - name of the release in which the data set version was
    published

  - time_period_start - first available time period in the data

  - time_period_start - latest available time period in the data

  Using "full" adds the following in addition to the above:

  - version_id - unique ID code for the data set version

  - status - publication status of the data set version

  - geographic_levels - geographic levels provided

  - filters - names of filters in the data set version

  - indicators - names of indicators in the data set version

  - notes - any additional information

- preview_token:

  Preview token required for access to private data sets

- ees_environment:

  EES ees_environment to connect to: "dev", "test", "preprod" or "prod"

- api_version:

  EES API version

- page_size:

  Number of results to return in a single query

- page:

  Page number of query results to return

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

A data frame with X columns:

Data frame listing all available versions of the given data set

## Examples

``` r
get_dataset_versions(dataset_id = example_id(group = "attendance"))
#>    version  type total_rows release_date release_name time_period_start
#> 1     3.16 Minor    5528878   2026-06-11 Week 21 2026      2025 Week 37
#> 2     3.15 Minor    5350306   2026-05-28 Week 20 2026      2025 Week 37
#> 3     3.14 Minor    5022924   2026-05-14 Week 18 2026      2025 Week 37
#> 4     3.13 Minor    4734734   2026-04-30 Week 16 2026      2025 Week 37
#> 5     3.12 Minor    4595492   2026-04-16 Week 13 2026      2025 Week 37
#> 6     3.11 Minor    4306612   2026-04-02 Week 12 2026      2025 Week 37
#> 7     3.10 Minor    3949468   2026-03-19 Week 10 2026      2025 Week 37
#> 8      3.9 Minor    3602444   2026-03-05  Week 7 2026      2025 Week 37
#> 9      3.8 Minor    3413982   2026-02-19  Week 6 2026      2025 Week 37
#> 10     3.7 Minor    3056930   2026-02-05  Week 4 2026      2025 Week 37
#> 11     3.6 Minor    2699786   2026-01-22  Week 2 2026      2025 Week 37
#> 12     3.5 Minor    2521444   2026-01-08 Week 51 2025      2025 Week 37
#> 13     3.4 Minor    2164346   2025-12-18 Week 49 2025      2025 Week 37
#> 14     3.3 Minor    1807202   2025-12-04 Week 47 2025      2025 Week 37
#> 15     3.2 Minor    1450058   2025-11-20 Week 45 2025      2025 Week 37
#> 16     3.1 Minor    1267898   2025-11-06 Week 43 2025      2025 Week 37
#> 17     3.0 Major     922622   2025-10-23 Week 41 2025      2025 Week 37
#> 18   2.1.1 Patch     577771   2025-10-10 Week 39 2025      2025 Week 37
#> 19     2.1 Minor     638589   2025-10-09 Week 39 2025      2025 Week 37
#> 20     2.0 Major     208334   2025-09-25 Week 37 2025      2025 Week 37
#> 21    1.11 Minor    5162258   2025-08-07 Week 29 2025      2024 Week 37
#> 22    1.10 Minor    5020394   2025-07-24 Week 28 2025      2024 Week 37
#> 23     1.9 Minor    4747752   2025-07-10 Week 26 2025      2024 Week 37
#> 24     1.8 Minor    4475018   2025-06-26 Week 24 2025      2024 Week 37
#> 25     1.7 Minor    4202376   2025-06-16 Week 21 2025      2024 Week 37
#> 26     1.6 Minor    4066032   2025-05-29 Week 20 2025      2024 Week 37
#> 27     1.5 Minor    3815654   2025-05-15 Week 18 2025      2024 Week 37
#> 28     1.4 Minor    3583998   2025-05-01 Week 15 2025      2024 Week 37
#> 29     1.3 Minor    3556490   2025-04-17 Week 14 2025      2024 Week 37
#> 30     1.2 Minor    3285734   2025-04-03 Week 12 2025      2024 Week 37
#> 31     1.1 Minor    3013000   2025-03-20 Week 10 2025      2024 Week 37
#> 32     1.0 Major    2750478   2025-03-06  Week 7 2025      2024 Week 37
#>    time_period_end
#> 1     2026 Week 21
#> 2     2026 Week 20
#> 3     2026 Week 18
#> 4     2026 Week 16
#> 5     2026 Week 14
#> 6     2026 Week 12
#> 7     2026 Week 10
#> 8      2026 Week 8
#> 9      2026 Week 6
#> 10     2026 Week 4
#> 11     2026 Week 2
#> 12    2025 Week 51
#> 13    2025 Week 49
#> 14    2025 Week 47
#> 15    2025 Week 45
#> 16    2025 Week 43
#> 17    2025 Week 41
#> 18    2025 Week 39
#> 19    2025 Week 39
#> 20    2025 Week 37
#> 21    2025 Week 30
#> 22    2025 Week 28
#> 23    2025 Week 26
#> 24    2025 Week 24
#> 25    2025 Week 21
#> 26    2025 Week 20
#> 27    2025 Week 18
#> 28    2025 Week 15
#> 29    2025 Week 14
#> 30    2025 Week 12
#> 31    2025 Week 10
#> 32     2025 Week 8
```
