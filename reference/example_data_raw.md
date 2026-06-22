# Example raw data

Download some example raw data. Mainly intended for use in developing /
testing the sqid parsing or as an example of getting raw data if any end
users would prefer to do the sqid parsing themselves.

## Usage

``` r
example_data_raw(group = "attendance", size = 32)
```

## Arguments

- group:

  Choose the publication group of examples to use. Options are:

  - "attendance": Large example data set, careful what you ask for

  - "absence": Smaller example data set

- size:

  Number of rows to return (max = 1000)

## Value

Nested list form of example data from the API

## Examples

``` r
example_data_raw()
#>    timePeriod.code timePeriod.period geographicLevel locations.LA locations.NAT
#> 1              W21              2026              LA        0ehT5         dP0Zw
#> 2              W21              2026              LA        0ehT5         dP0Zw
#> 3              W21              2026              LA        0ehT5         dP0Zw
#> 4              W21              2026              LA        0ehT5         dP0Zw
#> 5              W21              2026              LA        0ehT5         dP0Zw
#> 6              W21              2026              LA        0ehT5         dP0Zw
#> 7              W21              2026              LA        0ehT5         dP0Zw
#> 8              W21              2026              LA        0ehT5         dP0Zw
#> 9              W21              2026              LA        0ehT5         dP0Zw
#> 10             W21              2026              LA        0ehT5         dP0Zw
#> 11             W21              2026              LA        0ehT5         dP0Zw
#> 12             W21              2026              LA        0ehT5         dP0Zw
#> 13             W21              2026              LA        0ehT5         dP0Zw
#> 14             W21              2026              LA        0ehT5         dP0Zw
#> 15             W21              2026              LA        0ehT5         dP0Zw
#> 16             W21              2026              LA        0ehT5         dP0Zw
#> 17             W21              2026              LA        0ehT5         dP0Zw
#> 18             W21              2026              LA        0ehT5         dP0Zw
#> 19             W21              2026              LA        0ehT5         dP0Zw
#> 20             W21              2026              LA        0ehT5         dP0Zw
#> 21             W21              2026              LA        0ehT5         dP0Zw
#> 22             W21              2026              LA        0ehT5         dP0Zw
#> 23             W21              2026              LA        0ehT5         dP0Zw
#> 24             W21              2026              LA        0ehT5         dP0Zw
#> 25             W21              2026              LA        0ehT5         dP0Zw
#> 26             W21              2026              LA        0ehT5         dP0Zw
#> 27             W21              2026              LA        0ehT5         dP0Zw
#> 28             W21              2026              LA        0ehT5         dP0Zw
#> 29             W21              2026              LA        0ehT5         dP0Zw
#> 30             W21              2026              LA        0ehT5         dP0Zw
#> 31             W21              2026              LA        0ehT5         dP0Zw
#> 32             W21              2026              LA        0ehT5         dP0Zw
#>    locations.REG filters.uxo41 filters.z4FQE filters.X9fKb filters.krhsL
#> 1          VTQ5X         UazRF         qOnjG         STCVx         TmQPJ
#> 2          VTQ5X         UazRF         BHu7J         STCVx         TmQPJ
#> 3          VTQ5X         UazRF         YvoHK         STCVx         TmQPJ
#> 4          VTQ5X         UazRF         acQLP         STCVx         TmQPJ
#> 5          VTQ5X         1Dui3         28Wlz         STCVx         e4wuS
#> 6          VTQ5X         1Dui3         duo0Z         STCVx         e4wuS
#> 7          VTQ5X         1Dui3         uU1o4         STCVx         e4wuS
#> 8          VTQ5X         1Dui3         XFRfK         STCVx         e4wuS
#> 9          VTQ5X         1Dui3         OXYCL         STCVx         e4wuS
#> 10         VTQ5X         1Dui3         5PHdi         STCVx         e4wuS
#> 11         VTQ5X         1Dui3         9ko4v         STCVx         e4wuS
#> 12         VTQ5X         1Dui3         L31Wj         STCVx         e4wuS
#> 13         VTQ5X         1Dui3         kWQhs         STCVx         e4wuS
#> 14         VTQ5X         1Dui3         NtQDC         STCVx         e4wuS
#> 15         VTQ5X         1Dui3         TmKPJ         STCVx         e4wuS
#> 16         VTQ5X         P9Aeb         QErwb         STCVx         TmQPJ
#> 17         VTQ5X         P9Aeb         PmPeb         STCVx         TmQPJ
#> 18         VTQ5X         P9Aeb         e9vuS         STCVx         TmQPJ
#> 19         VTQ5X         P9Aeb         W3k2b         STCVx         TmQPJ
#> 20         VTQ5X         P9Aeb         jC9AM         STCVx         TmQPJ
#> 21         VTQ5X         P9Aeb         EGQqF         STCVx         TmQPJ
#> 22         VTQ5X         oRfmX         tGPEm         STCVx         e4wuS
#> 23         VTQ5X         oRfmX         oRUmX         STCVx         e4wuS
#> 24         VTQ5X         oRfmX         6wgrf         STCVx         e4wuS
#> 25         VTQ5X         oRfmX         7zvXo         STCVx         e4wuS
#> 26         VTQ5X         duj0Z         0unT5         STCVx         mkA9K
#> 27         VTQ5X         duj0Z         wjEbx         STCVx         mkA9K
#> 28         VTQ5X         duj0Z         p0uSo         STCVx         mkA9K
#> 29         VTQ5X         duj0Z         cxI31         STCVx         mkA9K
#> 30         VTQ5X         duj0Z         5eMdi         STCVx         mkA9K
#> 31         VTQ5X         duj0Z         Cf5Id         STCVx         mkA9K
#> 32         VTQ5X         duj0Z         4h0UZ         STCVx         mkA9K
#>    filters.TsPJP X9fKb
#> 1          bAxtT   799
#> 2          bAxtT   783
#> 3          bAxtT  1582
#> 4          bAxtT    15
#> 5          bAxtT    74
#> 6          bAxtT    15
#> 7          bAxtT     0
#> 8          bAxtT    11
#> 9          bAxtT     0
#> 10         bAxtT     0
#> 11         bAxtT     6
#> 12         bAxtT    58
#> 13         bAxtT     0
#> 14         bAxtT     0
#> 15         bAxtT    50
#> 16         bAxtT    10
#> 17         bAxtT     0
#> 18         bAxtT     0
#> 19         bAxtT     9
#> 20         bAxtT     0
#> 21         bAxtT     0
#> 22         bAxtT    14
#> 23         bAxtT     6
#> 24         bAxtT    47
#> 25         bAxtT     3
#> 26         bAxtT     0
#> 27         bAxtT     0
#> 28         bAxtT     0
#> 29         bAxtT     0
#> 30         bAxtT     0
#> 31         bAxtT     0
#> 32         bAxtT     0
```
