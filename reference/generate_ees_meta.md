# Generate EES meta

This function generates a basic meta data frame approximating what
analysts upload alongside data files to the platform

## Usage

``` r
generate_ees_meta(api_meta)
```

## Arguments

- api_meta:

## Value

data frame

## Examples

``` r
get_meta(example_id()) |> generate_ees_meta()
#>                     col_name  col_type                   label
#> 1            education_phase    Filter         Education phase
#> 2 persistent_absence_percent Indicator Persistent absence rate
#>   indicator_grouping indicator_unit indicator_dp filter_hint
#> 1                                                           
#> 2                                 %            1            
#>   filter_grouping_column filter_default
#> 1                                      
#> 2                                      
```
