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
#>    time_period time_identifier geographic_level   la_name   la_code la_oldCode
#> 1         2026         Week 21  Local authority Stockport E08000007        356
#> 2         2026         Week 21  Local authority Stockport E08000007        356
#> 3         2026         Week 21  Local authority Stockport E08000007        356
#> 4         2026         Week 21  Local authority Stockport E08000007        356
#> 5         2026         Week 21  Local authority Stockport E08000007        356
#> 6         2026         Week 21  Local authority Stockport E08000007        356
#> 7         2026         Week 21  Local authority Stockport E08000007        356
#> 8         2026         Week 21  Local authority Stockport E08000007        356
#> 9         2026         Week 21  Local authority Stockport E08000007        356
#> 10        2026         Week 21  Local authority Stockport E08000007        356
#> 11        2026         Week 21  Local authority Stockport E08000007        356
#> 12        2026         Week 21  Local authority Stockport E08000007        356
#> 13        2026         Week 21  Local authority Stockport E08000007        356
#> 14        2026         Week 21  Local authority Stockport E08000007        356
#> 15        2026         Week 21  Local authority Stockport E08000007        356
#> 16        2026         Week 21  Local authority Stockport E08000007        356
#> 17        2026         Week 21  Local authority Stockport E08000007        356
#> 18        2026         Week 21  Local authority Stockport E08000007        356
#> 19        2026         Week 21  Local authority Stockport E08000007        356
#> 20        2026         Week 21  Local authority Stockport E08000007        356
#> 21        2026         Week 21  Local authority Stockport E08000007        356
#> 22        2026         Week 21  Local authority Stockport E08000007        356
#> 23        2026         Week 21  Local authority Stockport E08000007        356
#> 24        2026         Week 21  Local authority Stockport E08000007        356
#> 25        2026         Week 21  Local authority Stockport E08000007        356
#> 26        2026         Week 21  Local authority Stockport E08000007        356
#> 27        2026         Week 21  Local authority Stockport E08000007        356
#> 28        2026         Week 21  Local authority Stockport E08000007        356
#> 29        2026         Week 21  Local authority Stockport E08000007        356
#> 30        2026         Week 21  Local authority Stockport E08000007        356
#> 31        2026         Week 21  Local authority Stockport E08000007        356
#> 32        2026         Week 21  Local authority Stockport E08000007        356
#>    nat_name  nat_code   reg_name  reg_code
#> 1   England E92000001 North West E12000002
#> 2   England E92000001 North West E12000002
#> 3   England E92000001 North West E12000002
#> 4   England E92000001 North West E12000002
#> 5   England E92000001 North West E12000002
#> 6   England E92000001 North West E12000002
#> 7   England E92000001 North West E12000002
#> 8   England E92000001 North West E12000002
#> 9   England E92000001 North West E12000002
#> 10  England E92000001 North West E12000002
#> 11  England E92000001 North West E12000002
#> 12  England E92000001 North West E12000002
#> 13  England E92000001 North West E12000002
#> 14  England E92000001 North West E12000002
#> 15  England E92000001 North West E12000002
#> 16  England E92000001 North West E12000002
#> 17  England E92000001 North West E12000002
#> 18  England E92000001 North West E12000002
#> 19  England E92000001 North West E12000002
#> 20  England E92000001 North West E12000002
#> 21  England E92000001 North West E12000002
#> 22  England E92000001 North West E12000002
#> 23  England E92000001 North West E12000002
#> 24  England E92000001 North West E12000002
#> 25  England E92000001 North West E12000002
#> 26  England E92000001 North West E12000002
#> 27  England E92000001 North West E12000002
#> 28  England E92000001 North West E12000002
#> 29  England E92000001 North West E12000002
#> 30  England E92000001 North West E12000002
#> 31  England E92000001 North West E12000002
#> 32  England E92000001 North West E12000002
#>                           attendance_reason           attendance_status
#> 1                                Present am                  Attendance
#> 2                                Present pm                  Attendance
#> 3                                   Present                  Attendance
#> 4  Present late before registers closed (l)                  Attendance
#> 5                               Illness (i)                     Absence
#> 6                        Medical dental (m)                     Absence
#> 7                  Religious observance (r)                     Absence
#> 8                           Study leave (s)                     Absence
#> 9                          Mobile child (t)                     Absence
#> 10                   Authorised holiday (h)                     Absence
#> 11                             Excluded (e)                     Absence
#> 12                     Other authorised (c)                     Absence
#> 13               Regulated performance (c1)                     Absence
#> 14                           Interview (j1)                     Absence
#> 15         Temporary reduced timetable (c2)                     Absence
#> 16                   Education off site (b)                  Attendance
#> 17                            Interview (j)                  Attendance
#> 18           Approved sporting activity (p)                  Attendance
#> 19               Educational visit trip (v)                  Attendance
#> 20                      Work experience (w)                  Attendance
#> 21             Education arranged by LA (k)                  Attendance
#> 22                 Unauthorised holiday (g)                     Absence
#> 23          Late after registers closed (u)                     Absence
#> 24                   Other unauthorised (o)                     Absence
#> 25                        No reason yet (n)                     Absence
#> 26                    Dual registration (d) Management and legacy codes
#> 27                      LA arrangements (q) Management and legacy codes
#> 28       Not attending enforced closure (y) Management and legacy codes
#> 29              Transport not provided (y1) Management and legacy codes
#> 30                   Travel disruption (y2) Management and legacy codes
#> 31                 Premises out of use (y3) Management and legacy codes
#> 32            Closed session cancelled (y4) Management and legacy codes
#>                  attendance_type education_phase time_frame session_count
#> 1                        Present         Special   Thursday           799
#> 2                        Present         Special   Thursday           783
#> 3                        Present         Special   Thursday          1582
#> 4                        Present         Special   Thursday            15
#> 5                     Authorised         Special   Thursday            74
#> 6                     Authorised         Special   Thursday            15
#> 7                     Authorised         Special   Thursday             0
#> 8                     Authorised         Special   Thursday            11
#> 9                     Authorised         Special   Thursday             0
#> 10                    Authorised         Special   Thursday             0
#> 11                    Authorised         Special   Thursday             6
#> 12                    Authorised         Special   Thursday            58
#> 13                    Authorised         Special   Thursday             0
#> 14                    Authorised         Special   Thursday             0
#> 15                    Authorised         Special   Thursday            50
#> 16 Approved educational activity         Special   Thursday            10
#> 17 Approved educational activity         Special   Thursday             0
#> 18 Approved educational activity         Special   Thursday             0
#> 19 Approved educational activity         Special   Thursday             9
#> 20 Approved educational activity         Special   Thursday             0
#> 21 Approved educational activity         Special   Thursday             0
#> 22                  Unauthorised         Special   Thursday            14
#> 23                  Unauthorised         Special   Thursday             6
#> 24                  Unauthorised         Special   Thursday            47
#> 25                  Unauthorised         Special   Thursday             3
#> 26   Management and legacy codes         Special   Thursday             0
#> 27   Management and legacy codes         Special   Thursday             0
#> 28   Management and legacy codes         Special   Thursday             0
#> 29   Management and legacy codes         Special   Thursday             0
#> 30   Management and legacy codes         Special   Thursday             0
#> 31   Management and legacy codes         Special   Thursday             0
#> 32   Management and legacy codes         Special   Thursday             0
```
