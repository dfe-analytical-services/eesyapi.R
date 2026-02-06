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
#> 1      3.7 Minor    3056930   2026-02-05  Week 4 2026      2025 Week 37
#> 2      3.6 Minor    2699786   2026-01-22  Week 2 2026      2025 Week 37
#> 3      3.5 Minor    2521444   2026-01-08 Week 51 2025      2025 Week 37
#> 4      3.4 Minor    2164346   2025-12-18 Week 49 2025      2025 Week 37
#> 5      3.3 Minor    1807202   2025-12-04 Week 47 2025      2025 Week 37
#> 6      3.2 Minor    1450058   2025-11-20 Week 45 2025      2025 Week 37
#> 7      3.1 Minor    1267898   2025-11-06 Week 43 2025      2025 Week 37
#> 8      3.0 Major     922622   2025-10-23 Week 41 2025      2025 Week 37
#> 9    2.1.1 Patch     577771   2025-10-10 Week 39 2025      2025 Week 37
#> 10     2.1 Minor     638589   2025-10-09 Week 39 2025      2025 Week 37
#> 11     3.7 Minor    3056930   2026-02-05  Week 4 2026      2025 Week 37
#> 12     3.6 Minor    2699786   2026-01-22  Week 2 2026      2025 Week 37
#> 13     3.5 Minor    2521444   2026-01-08 Week 51 2025      2025 Week 37
#> 14     3.4 Minor    2164346   2025-12-18 Week 49 2025      2025 Week 37
#> 15     3.3 Minor    1807202   2025-12-04 Week 47 2025      2025 Week 37
#> 16     3.2 Minor    1450058   2025-11-20 Week 45 2025      2025 Week 37
#> 17     3.1 Minor    1267898   2025-11-06 Week 43 2025      2025 Week 37
#> 18     3.0 Major     922622   2025-10-23 Week 41 2025      2025 Week 37
#> 19   2.1.1 Patch     577771   2025-10-10 Week 39 2025      2025 Week 37
#> 20     2.1 Minor     638589   2025-10-09 Week 39 2025      2025 Week 37
#> 21     3.7 Minor    3056930   2026-02-05  Week 4 2026      2025 Week 37
#> 22     3.6 Minor    2699786   2026-01-22  Week 2 2026      2025 Week 37
#> 23     3.5 Minor    2521444   2026-01-08 Week 51 2025      2025 Week 37
#> 24     3.4 Minor    2164346   2025-12-18 Week 49 2025      2025 Week 37
#> 25     3.3 Minor    1807202   2025-12-04 Week 47 2025      2025 Week 37
#> 26     3.2 Minor    1450058   2025-11-20 Week 45 2025      2025 Week 37
#> 27     3.1 Minor    1267898   2025-11-06 Week 43 2025      2025 Week 37
#> 28     3.0 Major     922622   2025-10-23 Week 41 2025      2025 Week 37
#> 29   2.1.1 Patch     577771   2025-10-10 Week 39 2025      2025 Week 37
#> 30     2.1 Minor     638589   2025-10-09 Week 39 2025      2025 Week 37
#>    time_period_end
#> 1      2026 Week 4
#> 2      2026 Week 2
#> 3     2025 Week 51
#> 4     2025 Week 49
#> 5     2025 Week 47
#> 6     2025 Week 45
#> 7     2025 Week 43
#> 8     2025 Week 41
#> 9     2025 Week 39
#> 10    2025 Week 39
#> 11     2026 Week 4
#> 12     2026 Week 2
#> 13    2025 Week 51
#> 14    2025 Week 49
#> 15    2025 Week 47
#> 16    2025 Week 45
#> 17    2025 Week 43
#> 18    2025 Week 41
#> 19    2025 Week 39
#> 20    2025 Week 39
#> 21     2026 Week 4
#> 22     2026 Week 2
#> 23    2025 Week 51
#> 24    2025 Week 49
#> 25    2025 Week 47
#> 26    2025 Week 45
#> 27    2025 Week 43
#> 28    2025 Week 41
#> 29    2025 Week 39
#> 30    2025 Week 39
```
