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
#>                  attendance_type education_phase time_frame
#> 1                        Present     All schools     Monday
#> 2  Approved educational activity     All schools     Monday
#> 3             Overall attendance     All schools     Monday
#> 4                Overall absence     All schools     Monday
#> 5                     Authorised     All schools     Monday
#> 6                   Unauthorised     All schools     Monday
#> 7              All late sessions     All schools     Monday
#> 8          All possible sessions     All schools     Monday
#> 9                        Present     All schools     Monday
#> 10                       Present     All schools     Monday
#> 11                       Present     All schools     Monday
#> 12                       Present     All schools     Monday
#> 13                    Authorised     All schools     Monday
#> 14                    Authorised     All schools     Monday
#> 15                    Authorised     All schools     Monday
#> 16                    Authorised     All schools     Monday
#> 17                    Authorised     All schools     Monday
#> 18                    Authorised     All schools     Monday
#> 19                    Authorised     All schools     Monday
#> 20                    Authorised     All schools     Monday
#> 21                    Authorised     All schools     Monday
#> 22                    Authorised     All schools     Monday
#> 23                    Authorised     All schools     Monday
#> 24 Approved educational activity     All schools     Monday
#> 25 Approved educational activity     All schools     Monday
#> 26 Approved educational activity     All schools     Monday
#> 27 Approved educational activity     All schools     Monday
#> 28 Approved educational activity     All schools     Monday
#> 29 Approved educational activity     All schools     Monday
#> 30                  Unauthorised     All schools     Monday
#> 31                  Unauthorised     All schools     Monday
#> 32                  Unauthorised     All schools     Monday
```
