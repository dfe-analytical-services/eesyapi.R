# Get publication specific data set catalogue

Get publication specific data set catalogue

## Usage

``` r
get_data_catalogue(
  publication_id,
  ees_environment = NULL,
  api_version = NULL,
  page_size = NULL,
  page = NULL,
  verbose = FALSE
)
```

## Arguments

- publication_id:

  ID of the publication to be connected to. This is required if the
  endpoint is "get-data-catalogue"

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

Data frame listing the data sets contained within a single publication

## Examples

``` r
get_data_catalogue(example_id("publication"))
#>                                     id                              title
#> 1 55629501-1945-0174-956c-594f21c90404 Numbers of schools submitting data
#> 2 55629501-e98b-0c75-adba-f95a0cfbb5e9      Persistent absence in schools
#> 3 63629501-d3ca-c471-9780-ec4cb6fdf172 Reasons for absence and attendance
#>                                                           summary    status
#> 1 The number of schools returning data as used in the publication Published
#> 2                                     Persistent absence measures Published
#> 3                        Reasons for pupil attendance and absence Published
#>   latestVersion.version          latestVersion.published
#> 1                   2.9 2026-02-05T09:30:25.727543+00:00
#> 2                  1.13 2026-02-05T09:30:25.304485+00:00
#> 3                   3.7 2026-02-05T09:30:24.823526+00:00
#>   latestVersion.totalResults                     latestVersion.id
#> 1                      66455 3d9783e7-8937-4005-a202-e46872b50523
#> 2                       7222 36ca5c5a-00e3-4183-98cf-c4d06cd3081f
#> 3                    3056930 f0e53797-56c0-43c7-8263-0bccae57a7ef
#>   latestVersion.timePeriods.start latestVersion.timePeriods.end
#> 1                    2025 Week 37                   2026 Week 4
#> 2                     2025 Week 8                   2026 Week 4
#> 3                    2025 Week 37                   2026 Week 4
#>        latestVersion.geographicLevels
#> 1 Local authority, National, Regional
#> 2 Local authority, National, Regional
#> 3 Local authority, National, Regional
#>                                                                latestVersion.filters
#> 1                                                        Education phase, Time frame
#> 2                                                                    Education phase
#> 3 Attendance reason, Attendance status, Attendance type, Education phase, Time frame
#>                                                latestVersion.indicators
#> 1 Number of schools submitting, Reference date, Total number of schools
#> 2                                               Persistent absence rate
#> 3               Number of sessions, Percent of sessions, Reference date
```
