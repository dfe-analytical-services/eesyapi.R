# Query a data set using GET and a query URL

This function provides a method for generating and sending a URL based
data query to the EES API. As a minimum, it requires the dataset_id and
indicators flags to be provided.

Note that the GET command is very limited on the level of logic it can
process. For example there is no way of using GET to make a query that
combines different filters with AND logic. So if you give GET a set of
filter items to search on, it can only return all rows containing any of
those items.

## Usage

``` r
get_dataset(
  dataset_id,
  indicators,
  time_periods = NULL,
  geographic_levels = NULL,
  locations = NULL,
  filter_items = NULL,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  page = NULL,
  page_size = 10000,
  parse = TRUE,
  verbose = FALSE
)
```

## Arguments

- dataset_id:

  ID of data set to be connected to. This is required if the endpoint is
  one of "get-dataset-versions", "get-summary", "get-meta", "get-csv",
  "get-data" or "post-data"

- indicators:

  Indicators required as a string or vector of strings (required)

- time_periods:

  Time periods required as a string ("period\|code") or vector of
  strings

- geographic_levels:

  Geographic levels required as a string or vector of strings

- locations:

  Location code required as a string or vector of strings

- filter_items:

  Filter items required as a string or vector of strings

- dataset_version:

  Version of data set to be connected to, in "major.minor.patch" format,
  with optional wildcards, e.g. "*", "2.*", "2.1.\*", "2.1.0". Can also
  be provided as a numeric value

- preview_token:

  Preview token required for access to private data sets

- ees_environment:

  EES ees_environment to connect to: "dev", "test", "preprod" or "prod"

- api_version:

  EES API version

- page:

  Page number of query results to return

- page_size:

  Number of results to return in a single query

- parse:

  Logical flag to activate parsing of the results. Default: TRUE

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

Data frame containing query results of an API data set

## Examples

``` r
eesyapi:::get_dataset(
  example_id(),
  geographic_levels = c("NAT"),
  filter_items = example_id("filter_item"),
  indicators = example_id("indicator")
)
#>    time_period time_identifier geographic_level nat_name  nat_code
#> 1         2026         Week 21         National  England E92000001
#> 2         2026         Week 20         National  England E92000001
#> 3         2026         Week 18         National  England E92000001
#> 4         2026         Week 16         National  England E92000001
#> 5         2026         Week 14         National  England E92000001
#> 6         2026         Week 12         National  England E92000001
#> 7         2026         Week 10         National  England E92000001
#> 8         2026          Week 8         National  England E92000001
#> 9         2026          Week 6         National  England E92000001
#> 10        2026          Week 4         National  England E92000001
#> 11        2026          Week 2         National  England E92000001
#> 12        2025         Week 30         National  England E92000001
#> 13        2025         Week 28         National  England E92000001
#> 14        2025         Week 26         National  England E92000001
#> 15        2025         Week 24         National  England E92000001
#> 16        2025         Week 21         National  England E92000001
#> 17        2025         Week 20         National  England E92000001
#> 18        2025         Week 18         National  England E92000001
#> 19        2025         Week 15         National  England E92000001
#> 20        2025         Week 14         National  England E92000001
#> 21        2025         Week 12         National  England E92000001
#> 22        2025         Week 10         National  England E92000001
#> 23        2025          Week 8         National  England E92000001
#>    education_phase persistent_absence_percent
#> 1          Primary                      13.18
#> 2          Primary                      13.13
#> 3          Primary                      13.45
#> 4          Primary                      13.78
#> 5          Primary                      13.86
#> 6          Primary                      14.04
#> 7          Primary                      14.08
#> 8          Primary                      14.63
#> 9          Primary                      14.54
#> 10         Primary                      14.73
#> 11         Primary                      15.13
#> 12         Primary                       13.5
#> 13         Primary                      13.41
#> 14         Primary                      13.46
#> 15         Primary                      13.57
#> 16         Primary                      13.42
#> 17         Primary                      13.41
#> 18         Primary                      13.76
#> 19         Primary                       14.1
#> 20         Primary                      14.15
#> 21         Primary                      14.06
#> 22         Primary                      14.41
#> 23         Primary                      14.95
```
