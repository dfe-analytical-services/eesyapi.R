# Parse contents of API data set json output

This takes the json results output from the API and converts it into a
usable data frame. It's primary use is as a helper to get_dataset and
post_dataset, but can be used in isolation from those by running an API
query and passing the following to parse_api_dataset(): response \|\>
httr::content("text") \|\> jsonlite::fromJSON() \|\> parse_api_dataset()

## Usage

``` r
parse_api_dataset(
  api_data_result,
  dataset_id,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  verbose = FALSE
)
```

## Arguments

- api_data_result:

  A json data result list as returned from the API

- dataset_id:

  ID of data set to be connected to. This is required if the endpoint is
  one of "get-dataset-versions", "get-summary", "get-meta", "get-csv",
  "get-data" or "post-data"

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

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

Data frame containing API data results

## Examples

``` r
example_data_raw(group = "attendance") |>
  eesyapi:::parse_api_dataset(example_id(group = "attendance"))
#>    time_period time_identifier geographic_level nat_name  nat_code
#> 1         2026          Week 4         National  England E92000001
#> 2         2026          Week 4         National  England E92000001
#> 3         2026          Week 4         National  England E92000001
#> 4         2026          Week 4         National  England E92000001
#> 5         2026          Week 4         National  England E92000001
#> 6         2026          Week 4         National  England E92000001
#> 7         2026          Week 4         National  England E92000001
#> 8         2026          Week 4         National  England E92000001
#> 9         2026          Week 4         National  England E92000001
#> 10        2026          Week 4         National  England E92000001
#> 11        2026          Week 4         National  England E92000001
#> 12        2026          Week 4         National  England E92000001
#> 13        2026          Week 4         National  England E92000001
#> 14        2026          Week 4         National  England E92000001
#> 15        2026          Week 4         National  England E92000001
#> 16        2026          Week 4         National  England E92000001
#> 17        2026          Week 4         National  England E92000001
#> 18        2026          Week 4         National  England E92000001
#> 19        2026          Week 4         National  England E92000001
#> 20        2026          Week 4         National  England E92000001
#> 21        2026          Week 4         National  England E92000001
#> 22        2026          Week 4         National  England E92000001
#> 23        2026          Week 4         National  England E92000001
#> 24        2026          Week 4         National  England E92000001
#> 25        2026          Week 4         National  England E92000001
#> 26        2026          Week 4         National  England E92000001
#> 27        2026          Week 4         National  England E92000001
#> 28        2026          Week 4         National  England E92000001
#> 29        2026          Week 4         National  England E92000001
#> 30        2026          Week 4         National  England E92000001
#> 31        2026          Week 4         National  England E92000001
#> 32        2026          Week 4         National  England E92000001
#>                           attendance_reason attendance_status
#> 1                               All present        Attendance
#> 2         All approved educational activity        Attendance
#> 3                        Overall attendance        Attendance
#> 4                           Overall absence           Absence
#> 5                            All authorised           Absence
#> 6                          All unauthorised           Absence
#> 7                         All late sessions     Late sessions
#> 8                     All possible sessions Possible sessions
#> 9                                Present am        Attendance
#> 10                               Present pm        Attendance
#> 11                                  Present        Attendance
#> 12 Present late before registers closed (l)        Attendance
#> 13                              Illness (i)           Absence
#> 14                       Medical dental (m)           Absence
#> 15                 Religious observance (r)           Absence
#> 16                          Study leave (s)           Absence
#> 17                         Mobile child (t)           Absence
#> 18                   Authorised holiday (h)           Absence
#> 19                             Excluded (e)           Absence
#> 20                     Other authorised (c)           Absence
#> 21               Regulated performance (c1)           Absence
#> 22                           Interview (j1)           Absence
#> 23         Temporary reduced timetable (c2)           Absence
#> 24                   Education off site (b)        Attendance
#> 25                            Interview (j)        Attendance
#> 26           Approved sporting activity (p)        Attendance
#> 27               Educational visit trip (v)        Attendance
#> 28                      Work experience (w)        Attendance
#> 29             Education arranged by LA (k)        Attendance
#> 30                 Unauthorised holiday (g)           Absence
#> 31          Late after registers closed (u)           Absence
#> 32                   Other unauthorised (o)           Absence
#>                  attendance_type education_phase time_frame session_count
#> 1                        Present     All schools     Monday      12787248
#> 2  Approved educational activity     All schools     Monday         78936
#> 3             Overall attendance     All schools     Monday      12866184
#> 4                Overall absence     All schools     Monday        897838
#> 5                     Authorised     All schools     Monday        609096
#> 6                   Unauthorised     All schools     Monday        288742
#> 7              All late sessions     All schools     Monday        350469
#> 8          All possible sessions     All schools     Monday      13764022
#> 9                        Present     All schools     Monday       6127328
#> 10                       Present     All schools     Monday       6348550
#> 11                       Present     All schools     Monday      12475878
#> 12                       Present     All schools     Monday        311370
#> 13                    Authorised     All schools     Monday        455999
#> 14                    Authorised     All schools     Monday         48326
#> 15                    Authorised     All schools     Monday           297
#> 16                    Authorised     All schools     Monday           140
#> 17                    Authorised     All schools     Monday           656
#> 18                    Authorised     All schools     Monday             0
#> 19                    Authorised     All schools     Monday         19058
#> 20                    Authorised     All schools     Monday         46578
#> 21                    Authorised     All schools     Monday           394
#> 22                    Authorised     All schools     Monday          2106
#> 23                    Authorised     All schools     Monday         35533
#> 24 Approved educational activity     All schools     Monday         24085
#> 25 Approved educational activity     All schools     Monday             0
#> 26 Approved educational activity     All schools     Monday          8561
#> 27 Approved educational activity     All schools     Monday         36259
#> 28 Approved educational activity     All schools     Monday          4135
#> 29 Approved educational activity     All schools     Monday          5896
#> 30                  Unauthorised     All schools     Monday         27170
#> 31                  Unauthorised     All schools     Monday         39099
#> 32                  Unauthorised     All schools     Monday        209572
```
