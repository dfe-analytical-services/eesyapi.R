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
#>    timePeriod.code timePeriod.period geographicLevel   NAT filters.uxo41
#> 1               W4              2026             NAT dP0Zw         UazRF
#> 2               W4              2026             NAT dP0Zw         P9Aeb
#> 3               W4              2026             NAT dP0Zw         ls5cB
#> 4               W4              2026             NAT dP0Zw         uUIo4
#> 5               W4              2026             NAT dP0Zw         1Dui3
#> 6               W4              2026             NAT dP0Zw         oRfmX
#> 7               W4              2026             NAT dP0Zw         AIaGK
#> 8               W4              2026             NAT dP0Zw         VPw5X
#> 9               W4              2026             NAT dP0Zw         UazRF
#> 10              W4              2026             NAT dP0Zw         UazRF
#> 11              W4              2026             NAT dP0Zw         UazRF
#> 12              W4              2026             NAT dP0Zw         UazRF
#> 13              W4              2026             NAT dP0Zw         1Dui3
#> 14              W4              2026             NAT dP0Zw         1Dui3
#> 15              W4              2026             NAT dP0Zw         1Dui3
#> 16              W4              2026             NAT dP0Zw         1Dui3
#> 17              W4              2026             NAT dP0Zw         1Dui3
#> 18              W4              2026             NAT dP0Zw         1Dui3
#> 19              W4              2026             NAT dP0Zw         1Dui3
#> 20              W4              2026             NAT dP0Zw         1Dui3
#> 21              W4              2026             NAT dP0Zw         1Dui3
#> 22              W4              2026             NAT dP0Zw         1Dui3
#> 23              W4              2026             NAT dP0Zw         1Dui3
#> 24              W4              2026             NAT dP0Zw         P9Aeb
#> 25              W4              2026             NAT dP0Zw         P9Aeb
#> 26              W4              2026             NAT dP0Zw         P9Aeb
#> 27              W4              2026             NAT dP0Zw         P9Aeb
#> 28              W4              2026             NAT dP0Zw         P9Aeb
#> 29              W4              2026             NAT dP0Zw         P9Aeb
#> 30              W4              2026             NAT dP0Zw         oRfmX
#> 31              W4              2026             NAT dP0Zw         oRfmX
#> 32              W4              2026             NAT dP0Zw         oRfmX
#>    filters.krhsL filters.TsPJP filters.z4FQE filters.X9fKb    X9fKb
#> 1          TmQPJ         RL5ka         bjstT         rbyNj 12787248
#> 2          TmQPJ         RL5ka         3xu8u         rbyNj    78936
#> 3          TmQPJ         RL5ka         HnuzL         rbyNj 12866184
#> 4          e4wuS         RL5ka         0oaT5         rbyNj   897838
#> 5          e4wuS         RL5ka         S0OVx         rbyNj   609096
#> 6          e4wuS         RL5ka         Dt8Qe         rbyNj   288742
#> 7          h2IyW         RL5ka         y2daB         rbyNj   350469
#> 8          g7LO9         RL5ka         RaVka         rbyNj 13764022
#> 9          TmQPJ         RL5ka         qOnjG         rbyNj  6127328
#> 10         TmQPJ         RL5ka         BHu7J         rbyNj  6348550
#> 11         TmQPJ         RL5ka         YvoHK         rbyNj 12475878
#> 12         TmQPJ         RL5ka         acQLP         rbyNj   311370
#> 13         e4wuS         RL5ka         28Wlz         rbyNj   455999
#> 14         e4wuS         RL5ka         duo0Z         rbyNj    48326
#> 15         e4wuS         RL5ka         uU1o4         rbyNj      297
#> 16         e4wuS         RL5ka         XFRfK         rbyNj      140
#> 17         e4wuS         RL5ka         OXYCL         rbyNj      656
#> 18         e4wuS         RL5ka         5PHdi         rbyNj        0
#> 19         e4wuS         RL5ka         9ko4v         rbyNj    19058
#> 20         e4wuS         RL5ka         L31Wj         rbyNj    46578
#> 21         e4wuS         RL5ka         kWQhs         rbyNj      394
#> 22         e4wuS         RL5ka         NtQDC         rbyNj     2106
#> 23         e4wuS         RL5ka         TmKPJ         rbyNj    35533
#> 24         TmQPJ         RL5ka         QErwb         rbyNj    24085
#> 25         TmQPJ         RL5ka         PmPeb         rbyNj        0
#> 26         TmQPJ         RL5ka         e9vuS         rbyNj     8561
#> 27         TmQPJ         RL5ka         W3k2b         rbyNj    36259
#> 28         TmQPJ         RL5ka         jC9AM         rbyNj     4135
#> 29         TmQPJ         RL5ka         EGQqF         rbyNj     5896
#> 30         e4wuS         RL5ka         tGPEm         rbyNj    27170
#> 31         e4wuS         RL5ka         oRUmX         rbyNj    39099
#> 32         e4wuS         RL5ka         6wgrf         rbyNj   209572
```
