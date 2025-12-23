# Get data set versions

Get data set versions

## Usage

``` r
get_dataset_versions(
  dataset_id,
  detail = "light",
  ees_environment = NULL,
  api_version = NULL,
  page_size = 40,
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
#> 1      3.4 Minor    2164346   2025-12-18 Week 49 2025      2025 Week 37
#> 2      3.3 Minor    1807202   2025-12-04 Week 47 2025      2025 Week 37
#> 3      3.2 Minor    1450058   2025-11-20 Week 45 2025      2025 Week 37
#> 4      3.1 Minor    1267898   2025-11-06 Week 43 2025      2025 Week 37
#> 5      3.0 Major     922622   2025-10-23 Week 41 2025      2025 Week 37
#> 6    2.1.1 Patch     577771   2025-10-10 Week 39 2025      2025 Week 37
#> 7      2.1 Minor     638589   2025-10-09 Week 39 2025      2025 Week 37
#> 8      2.0 Major     208334   2025-09-25 Week 37 2025      2025 Week 37
#> 9     1.11 Minor    5162258   2025-08-07 Week 29 2025      2024 Week 37
#> 10    1.10 Minor    5020394   2025-07-24 Week 28 2025      2024 Week 37
#> 11     3.4 Minor    2164346   2025-12-18 Week 49 2025      2025 Week 37
#> 12     3.3 Minor    1807202   2025-12-04 Week 47 2025      2025 Week 37
#> 13     3.2 Minor    1450058   2025-11-20 Week 45 2025      2025 Week 37
#> 14     3.1 Minor    1267898   2025-11-06 Week 43 2025      2025 Week 37
#> 15     3.0 Major     922622   2025-10-23 Week 41 2025      2025 Week 37
#> 16   2.1.1 Patch     577771   2025-10-10 Week 39 2025      2025 Week 37
#> 17     2.1 Minor     638589   2025-10-09 Week 39 2025      2025 Week 37
#> 18     2.0 Major     208334   2025-09-25 Week 37 2025      2025 Week 37
#> 19    1.11 Minor    5162258   2025-08-07 Week 29 2025      2024 Week 37
#> 20    1.10 Minor    5020394   2025-07-24 Week 28 2025      2024 Week 37
#>    time_period_end
#> 1     2025 Week 49
#> 2     2025 Week 47
#> 3     2025 Week 45
#> 4     2025 Week 43
#> 5     2025 Week 41
#> 6     2025 Week 39
#> 7     2025 Week 39
#> 8     2025 Week 37
#> 9     2025 Week 30
#> 10    2025 Week 28
#> 11    2025 Week 49
#> 12    2025 Week 47
#> 13    2025 Week 45
#> 14    2025 Week 43
#> 15    2025 Week 41
#> 16    2025 Week 39
#> 17    2025 Week 39
#> 18    2025 Week 37
#> 19    2025 Week 30
#> 20    2025 Week 28
```
