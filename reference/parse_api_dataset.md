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
#> 1         2025         Week 49         National  England E92000001
#> 2         2025         Week 49         National  England E92000001
#> 3         2025         Week 49         National  England E92000001
#> 4         2025         Week 49         National  England E92000001
#> 5         2025         Week 49         National  England E92000001
#> 6         2025         Week 49         National  England E92000001
#> 7         2025         Week 49         National  England E92000001
#> 8         2025         Week 49         National  England E92000001
#> 9         2025         Week 49         National  England E92000001
#> 10        2025         Week 49         National  England E92000001
#> 11        2025         Week 49         National  England E92000001
#> 12        2025         Week 49         National  England E92000001
#> 13        2025         Week 49         National  England E92000001
#> 14        2025         Week 49         National  England E92000001
#> 15        2025         Week 49         National  England E92000001
#> 16        2025         Week 49         National  England E92000001
#> 17        2025         Week 49         National  England E92000001
#> 18        2025         Week 49         National  England E92000001
#> 19        2025         Week 49         National  England E92000001
#> 20        2025         Week 49         National  England E92000001
#> 21        2025         Week 49         National  England E92000001
#> 22        2025         Week 49         National  England E92000001
#> 23        2025         Week 49         National  England E92000001
#> 24        2025         Week 49         National  England E92000001
#> 25        2025         Week 49         National  England E92000001
#> 26        2025         Week 49         National  England E92000001
#> 27        2025         Week 49         National  England E92000001
#> 28        2025         Week 49         National  England E92000001
#> 29        2025         Week 49         National  England E92000001
#> 30        2025         Week 49         National  England E92000001
#> 31        2025         Week 49         National  England E92000001
#> 32        2025         Week 49         National  England E92000001
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
#> 1                        Present     All schools     Monday      12085186
#> 2  Approved educational activity     All schools     Monday         70069
#> 3             Overall attendance     All schools     Monday      12155255
#> 4                Overall absence     All schools     Monday       1115334
#> 5                     Authorised     All schools     Monday        813326
#> 6                   Unauthorised     All schools     Monday        302008
#> 7              All late sessions     All schools     Monday        335672
#> 8          All possible sessions     All schools     Monday      13270589
#> 9                        Present     All schools     Monday       5781997
#> 10                       Present     All schools     Monday       6004234
#> 11                       Present     All schools     Monday      11786231
#> 12                       Present     All schools     Monday        298955
#> 13                    Authorised     All schools     Monday        669932
#> 14                    Authorised     All schools     Monday         43893
#> 15                    Authorised     All schools     Monday           268
#> 16                    Authorised     All schools     Monday           240
#> 17                    Authorised     All schools     Monday           768
#> 18                    Authorised     All schools     Monday             0
#> 19                    Authorised     All schools     Monday         18735
#> 20                    Authorised     All schools     Monday         45754
#> 21                    Authorised     All schools     Monday           773
#> 22                    Authorised     All schools     Monday          1370
#> 23                    Authorised     All schools     Monday         31592
#> 24 Approved educational activity     All schools     Monday         21701
#> 25 Approved educational activity     All schools     Monday             1
#> 26 Approved educational activity     All schools     Monday          8376
#> 27 Approved educational activity     All schools     Monday         32150
#> 28 Approved educational activity     All schools     Monday          2736
#> 29 Approved educational activity     All schools     Monday          5105
#> 30                  Unauthorised     All schools     Monday         36466
#> 31                  Unauthorised     All schools     Monday         36717
#> 32                  Unauthorised     All schools     Monday        213504
```
