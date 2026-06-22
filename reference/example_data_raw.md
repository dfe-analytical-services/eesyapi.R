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
#>    locations.REG filters.uxo41 filters.krhsL filters.TsPJP filters.z4FQE
#> 1          VTQ5X         UazRF         TmQPJ         bAxtT         qOnjG
#> 2          VTQ5X         UazRF         TmQPJ         bAxtT         BHu7J
#> 3          VTQ5X         UazRF         TmQPJ         bAxtT         YvoHK
#> 4          VTQ5X         UazRF         TmQPJ         bAxtT         acQLP
#> 5          VTQ5X         1Dui3         e4wuS         bAxtT         28Wlz
#> 6          VTQ5X         1Dui3         e4wuS         bAxtT         duo0Z
#> 7          VTQ5X         1Dui3         e4wuS         bAxtT         uU1o4
#> 8          VTQ5X         1Dui3         e4wuS         bAxtT         XFRfK
#> 9          VTQ5X         1Dui3         e4wuS         bAxtT         OXYCL
#> 10         VTQ5X         1Dui3         e4wuS         bAxtT         5PHdi
#> 11         VTQ5X         1Dui3         e4wuS         bAxtT         9ko4v
#> 12         VTQ5X         1Dui3         e4wuS         bAxtT         L31Wj
#> 13         VTQ5X         1Dui3         e4wuS         bAxtT         kWQhs
#> 14         VTQ5X         1Dui3         e4wuS         bAxtT         NtQDC
#> 15         VTQ5X         1Dui3         e4wuS         bAxtT         TmKPJ
#> 16         VTQ5X         P9Aeb         TmQPJ         bAxtT         QErwb
#> 17         VTQ5X         P9Aeb         TmQPJ         bAxtT         PmPeb
#> 18         VTQ5X         P9Aeb         TmQPJ         bAxtT         e9vuS
#> 19         VTQ5X         P9Aeb         TmQPJ         bAxtT         W3k2b
#> 20         VTQ5X         P9Aeb         TmQPJ         bAxtT         jC9AM
#> 21         VTQ5X         P9Aeb         TmQPJ         bAxtT         EGQqF
#> 22         VTQ5X         oRfmX         e4wuS         bAxtT         tGPEm
#> 23         VTQ5X         oRfmX         e4wuS         bAxtT         oRUmX
#> 24         VTQ5X         oRfmX         e4wuS         bAxtT         6wgrf
#> 25         VTQ5X         oRfmX         e4wuS         bAxtT         7zvXo
#> 26         VTQ5X         duj0Z         mkA9K         bAxtT         0unT5
#> 27         VTQ5X         duj0Z         mkA9K         bAxtT         wjEbx
#> 28         VTQ5X         duj0Z         mkA9K         bAxtT         p0uSo
#> 29         VTQ5X         duj0Z         mkA9K         bAxtT         cxI31
#> 30         VTQ5X         duj0Z         mkA9K         bAxtT         5eMdi
#> 31         VTQ5X         duj0Z         mkA9K         bAxtT         Cf5Id
#> 32         VTQ5X         duj0Z         mkA9K         bAxtT         4h0UZ
#>    filters.X9fKb X9fKb
#> 1          STCVx   799
#> 2          STCVx   783
#> 3          STCVx  1582
#> 4          STCVx    15
#> 5          STCVx    74
#> 6          STCVx    15
#> 7          STCVx     0
#> 8          STCVx    11
#> 9          STCVx     0
#> 10         STCVx     0
#> 11         STCVx     6
#> 12         STCVx    58
#> 13         STCVx     0
#> 14         STCVx     0
#> 15         STCVx    50
#> 16         STCVx    10
#> 17         STCVx     0
#> 18         STCVx     0
#> 19         STCVx     9
#> 20         STCVx     0
#> 21         STCVx     0
#> 22         STCVx    14
#> 23         STCVx     6
#> 24         STCVx    47
#> 25         STCVx     3
#> 26         STCVx     0
#> 27         STCVx     0
#> 28         STCVx     0
#> 29         STCVx     0
#> 30         STCVx     0
#> 31         STCVx     0
#> 32         STCVx     0
```
