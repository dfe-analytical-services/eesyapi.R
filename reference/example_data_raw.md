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
#> 1              W49              2025             NAT dP0Zw         UazRF
#> 2              W49              2025             NAT dP0Zw         P9Aeb
#> 3              W49              2025             NAT dP0Zw         ls5cB
#> 4              W49              2025             NAT dP0Zw         uUIo4
#> 5              W49              2025             NAT dP0Zw         1Dui3
#> 6              W49              2025             NAT dP0Zw         oRfmX
#> 7              W49              2025             NAT dP0Zw         AIaGK
#> 8              W49              2025             NAT dP0Zw         VPw5X
#> 9              W49              2025             NAT dP0Zw         UazRF
#> 10             W49              2025             NAT dP0Zw         UazRF
#> 11             W49              2025             NAT dP0Zw         UazRF
#> 12             W49              2025             NAT dP0Zw         UazRF
#> 13             W49              2025             NAT dP0Zw         1Dui3
#> 14             W49              2025             NAT dP0Zw         1Dui3
#> 15             W49              2025             NAT dP0Zw         1Dui3
#> 16             W49              2025             NAT dP0Zw         1Dui3
#> 17             W49              2025             NAT dP0Zw         1Dui3
#> 18             W49              2025             NAT dP0Zw         1Dui3
#> 19             W49              2025             NAT dP0Zw         1Dui3
#> 20             W49              2025             NAT dP0Zw         1Dui3
#> 21             W49              2025             NAT dP0Zw         1Dui3
#> 22             W49              2025             NAT dP0Zw         1Dui3
#> 23             W49              2025             NAT dP0Zw         1Dui3
#> 24             W49              2025             NAT dP0Zw         P9Aeb
#> 25             W49              2025             NAT dP0Zw         P9Aeb
#> 26             W49              2025             NAT dP0Zw         P9Aeb
#> 27             W49              2025             NAT dP0Zw         P9Aeb
#> 28             W49              2025             NAT dP0Zw         P9Aeb
#> 29             W49              2025             NAT dP0Zw         P9Aeb
#> 30             W49              2025             NAT dP0Zw         oRfmX
#> 31             W49              2025             NAT dP0Zw         oRfmX
#> 32             W49              2025             NAT dP0Zw         oRfmX
#>    filters.krhsL filters.TsPJP filters.z4FQE filters.X9fKb    X9fKb
#> 1          TmQPJ         RL5ka         bjstT         rbyNj 12085186
#> 2          TmQPJ         RL5ka         3xu8u         rbyNj    70069
#> 3          TmQPJ         RL5ka         HnuzL         rbyNj 12155255
#> 4          e4wuS         RL5ka         0oaT5         rbyNj  1115334
#> 5          e4wuS         RL5ka         S0OVx         rbyNj   813326
#> 6          e4wuS         RL5ka         Dt8Qe         rbyNj   302008
#> 7          h2IyW         RL5ka         y2daB         rbyNj   335672
#> 8          g7LO9         RL5ka         RaVka         rbyNj 13270589
#> 9          TmQPJ         RL5ka         qOnjG         rbyNj  5781997
#> 10         TmQPJ         RL5ka         BHu7J         rbyNj  6004234
#> 11         TmQPJ         RL5ka         YvoHK         rbyNj 11786231
#> 12         TmQPJ         RL5ka         acQLP         rbyNj   298955
#> 13         e4wuS         RL5ka         28Wlz         rbyNj   669932
#> 14         e4wuS         RL5ka         duo0Z         rbyNj    43893
#> 15         e4wuS         RL5ka         uU1o4         rbyNj      268
#> 16         e4wuS         RL5ka         XFRfK         rbyNj      240
#> 17         e4wuS         RL5ka         OXYCL         rbyNj      768
#> 18         e4wuS         RL5ka         5PHdi         rbyNj        0
#> 19         e4wuS         RL5ka         9ko4v         rbyNj    18735
#> 20         e4wuS         RL5ka         L31Wj         rbyNj    45754
#> 21         e4wuS         RL5ka         kWQhs         rbyNj      773
#> 22         e4wuS         RL5ka         NtQDC         rbyNj     1370
#> 23         e4wuS         RL5ka         TmKPJ         rbyNj    31592
#> 24         TmQPJ         RL5ka         QErwb         rbyNj    21701
#> 25         TmQPJ         RL5ka         PmPeb         rbyNj        1
#> 26         TmQPJ         RL5ka         e9vuS         rbyNj     8376
#> 27         TmQPJ         RL5ka         W3k2b         rbyNj    32150
#> 28         TmQPJ         RL5ka         jC9AM         rbyNj     2736
#> 29         TmQPJ         RL5ka         EGQqF         rbyNj     5105
#> 30         e4wuS         RL5ka         tGPEm         rbyNj    36466
#> 31         e4wuS         RL5ka         oRUmX         rbyNj    36717
#> 32         e4wuS         RL5ka         6wgrf         rbyNj   213504
```
