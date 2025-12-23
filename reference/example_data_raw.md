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
#>    filters.z4FQE filters.X9fKb filters.krhsL filters.TsPJP    X9fKb
#> 1          bjstT         rbyNj         TmQPJ         RL5ka 12085186
#> 2          3xu8u         rbyNj         TmQPJ         RL5ka    70069
#> 3          HnuzL         rbyNj         TmQPJ         RL5ka 12155255
#> 4          0oaT5         rbyNj         e4wuS         RL5ka  1115334
#> 5          S0OVx         rbyNj         e4wuS         RL5ka   813326
#> 6          Dt8Qe         rbyNj         e4wuS         RL5ka   302008
#> 7          y2daB         rbyNj         h2IyW         RL5ka   335672
#> 8          RaVka         rbyNj         g7LO9         RL5ka 13270589
#> 9          qOnjG         rbyNj         TmQPJ         RL5ka  5781997
#> 10         BHu7J         rbyNj         TmQPJ         RL5ka  6004234
#> 11         YvoHK         rbyNj         TmQPJ         RL5ka 11786231
#> 12         acQLP         rbyNj         TmQPJ         RL5ka   298955
#> 13         28Wlz         rbyNj         e4wuS         RL5ka   669932
#> 14         duo0Z         rbyNj         e4wuS         RL5ka    43893
#> 15         uU1o4         rbyNj         e4wuS         RL5ka      268
#> 16         XFRfK         rbyNj         e4wuS         RL5ka      240
#> 17         OXYCL         rbyNj         e4wuS         RL5ka      768
#> 18         5PHdi         rbyNj         e4wuS         RL5ka        0
#> 19         9ko4v         rbyNj         e4wuS         RL5ka    18735
#> 20         L31Wj         rbyNj         e4wuS         RL5ka    45754
#> 21         kWQhs         rbyNj         e4wuS         RL5ka      773
#> 22         NtQDC         rbyNj         e4wuS         RL5ka     1370
#> 23         TmKPJ         rbyNj         e4wuS         RL5ka    31592
#> 24         QErwb         rbyNj         TmQPJ         RL5ka    21701
#> 25         PmPeb         rbyNj         TmQPJ         RL5ka        1
#> 26         e9vuS         rbyNj         TmQPJ         RL5ka     8376
#> 27         W3k2b         rbyNj         TmQPJ         RL5ka    32150
#> 28         jC9AM         rbyNj         TmQPJ         RL5ka     2736
#> 29         EGQqF         rbyNj         TmQPJ         RL5ka     5105
#> 30         tGPEm         rbyNj         e4wuS         RL5ka    36466
#> 31         oRUmX         rbyNj         e4wuS         RL5ka    36717
#> 32         6wgrf         rbyNj         e4wuS         RL5ka   213504
```
