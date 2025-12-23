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
#> 2 63629501-d3ca-c471-9780-ec4cb6fdf172 Reasons for absence and attendance
#> 3 55629501-e98b-0c75-adba-f95a0cfbb5e9      Persistent absence in schools
#>                                                           summary    status
#> 1 The number of schools returning data as used in the publication Published
#> 2                        Reasons for pupil attendance and absence Published
#> 3                                     Persistent absence measures Published
#>   latestVersion.version          latestVersion.published
#> 1                   2.6 2025-12-18T09:30:23.578369+00:00
#> 2                   3.4  2025-12-18T09:30:18.28033+00:00
#> 3                  1.11 2025-08-07T08:30:21.190815+00:00
#>   latestVersion.totalResults                     latestVersion.id
#> 1                      47051 25faeaad-5c3e-454b-9f8e-5eff7b17572f
#> 2                    2164346 7dfd66bb-be6e-479c-b792-83025823c68f
#> 3                       5928 298d8a61-d321-4cfa-aa4b-ca0376def3c5
#>   latestVersion.timePeriods.start latestVersion.timePeriods.end
#> 1                    2025 Week 37                  2025 Week 49
#> 2                    2025 Week 37                  2025 Week 49
#> 3                     2025 Week 8                  2025 Week 30
#>        latestVersion.geographicLevels
#> 1 Local authority, National, Regional
#> 2 Local authority, National, Regional
#> 3 Local authority, National, Regional
#>                                                                latestVersion.filters
#> 1                                                        Education phase, Time frame
#> 2 Attendance reason, Attendance status, Attendance type, Education phase, Time frame
#> 3                                                                    Education phase
#>                                                latestVersion.indicators
#> 1 Number of schools submitting, Reference date, Total number of schools
#> 2               Number of sessions, Percent of sessions, Reference date
#> 3                                            Persistent absence percent
```
