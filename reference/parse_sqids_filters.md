# Parse IDs in a set of filters

The API uses unique IDs (sqids) to identify each filter column and its
contents (filter items). This function parses those into the data
creators' id and item labels based on the meta data stored on the API
for the data set.

## Usage

``` r
parse_sqids_filters(filters, meta, verbose = FALSE)
```

## Arguments

- filters:

  A set of filter item columns as taken from a data set downloaded from
  the API

- meta:

  Meta data for the data set as provided by
  [`get_meta()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_meta.md)

- verbose:

  Run in verbose mode with debugging messages

## Value

Data frame

## Examples

``` r
example_data_raw() |>
  magrittr::use_series("filters") |>
  eesyapi:::parse_sqids_filters(get_meta(example_id(group = "attendance")))
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
#>                  attendance_type education_phase time_frame
#> 1                        Present         Special   Thursday
#> 2                        Present         Special   Thursday
#> 3                        Present         Special   Thursday
#> 4                        Present         Special   Thursday
#> 5                     Authorised         Special   Thursday
#> 6                     Authorised         Special   Thursday
#> 7                     Authorised         Special   Thursday
#> 8                     Authorised         Special   Thursday
#> 9                     Authorised         Special   Thursday
#> 10                    Authorised         Special   Thursday
#> 11                    Authorised         Special   Thursday
#> 12                    Authorised         Special   Thursday
#> 13                    Authorised         Special   Thursday
#> 14                    Authorised         Special   Thursday
#> 15                    Authorised         Special   Thursday
#> 16 Approved educational activity         Special   Thursday
#> 17 Approved educational activity         Special   Thursday
#> 18 Approved educational activity         Special   Thursday
#> 19 Approved educational activity         Special   Thursday
#> 20 Approved educational activity         Special   Thursday
#> 21 Approved educational activity         Special   Thursday
#> 22                  Unauthorised         Special   Thursday
#> 23                  Unauthorised         Special   Thursday
#> 24                  Unauthorised         Special   Thursday
#> 25                  Unauthorised         Special   Thursday
#> 26   Management and legacy codes         Special   Thursday
#> 27   Management and legacy codes         Special   Thursday
#> 28   Management and legacy codes         Special   Thursday
#> 29   Management and legacy codes         Special   Thursday
#> 30   Management and legacy codes         Special   Thursday
#> 31   Management and legacy codes         Special   Thursday
#> 32   Management and legacy codes         Special   Thursday
```
