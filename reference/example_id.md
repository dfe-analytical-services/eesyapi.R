# Example ID

This function returns examples of working IDs that can be used with the
eesyapi functions, such as query_dataset().

## Usage

``` r
example_id(level = "dataset", ees_environment = "prod", group = "absence")
```

## Arguments

- level:

  Level of ID example to return. A range are available, although not
  every example group necessarily contains all possible examples. The
  following are generally available.

  - "all": Return full list of example options for given group.

  - "publication": Return example publication ID

  - "dataset": Return example data set ID

  - "time_period" / "time_periods": Return example time_period(s)

  - "location_id" / "location_ids": Return example location ID(s)

  - "location_code / location_codes": Return example location code(s)

  - "filter": Return example filter column ID

  - "filter_item" / "filter_items_short" / "filter_items_long": Return
    example filter ID or example short / long filter query list.

  - "indicator": Return example indicator ID

- ees_environment:

  Environment to return a working example for: "dev", "test" or "prod"

- group:

  Choose the publication group of examples to use. Options are:

  - "attendance": Large example data set, careful what you ask for

  - "absence": Smaller example data set

## Value

String, vector or list containing example ID(s) present in the API

## Examples

``` r
example_id()
#> [1] "55629501-e98b-0c75-adba-f95a0cfbb5e9"
example_id("all")
#> $publication
#> [1] "9676af6b-d563-41f4-d071-08da8f468680"
#> 
#> $dataset
#> [1] "55629501-e98b-0c75-adba-f95a0cfbb5e9"
#> 
#> $location_id
#> [1] "LA|id|it6Xr"
#> 
#> $location_code
#> [1] "NAT|code|E92000001"
#> 
#> $filter
#> [1] "BT7J3"
#> 
#> $filter_item
#> [1] "oUXmX"
#> 
#> $indicator
#> [1] "uxo41"
#> 
#> $indicators
#> [1] "uxo41"
#> 
example_id("all", ees_environment = "dev")
#> $publication
#> [1] "d823e4df-626f-4450-9b21-08dc8b95fc02"
#> 
#> $dataset
#> [1] "830f9201-9e11-ad75-8dcd-d2efe2834457"
#> 
#> $location_id
#> [1] "LA|id|ml79K"
#> 
#> $location_code
#> [1] "NAT|code|E92000001"
#> 
#> $location_codes
#> [1] "REG|code|E12000001" "REG|code|E12000002"
#> 
#> $filter
#> [1] "01tT5"
#> 
#> $filter_item
#> [1] "wEZcb"
#> 
#> $indicator
#> [1] "PbNeb"
#> 
example_id("publication")
#> [1] "9676af6b-d563-41f4-d071-08da8f468680"
example_id("publication", group = "attendance")
#> [1] "9676af6b-d563-41f4-d071-08da8f468680"
example_id("time_period", group = "attendance")
#> [1] "2025|W3"
example_id("location_ids", group = "attendance")
#> [1] "LA|id|it6Xr"  "REG|id|ACyGK"
example_id("filter_items_short", group = "attendance")
#> $attendance_type
#> [1] "P9Aeb" "VPw5X"
#> 
#> $education_phase
#> [1] "rbyNj" "GBMgr"
#> 
#> $time_frame
#> [1] "5ezdi"
#> 
example_id("indicator", group = "attendance")
#> [1] "X9fKb"
```
