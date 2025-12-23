# Convert filter type to API filter type

The API uses a slightly different naming convention for the different
types of filters to what is used by analysts within data files. The
function just converts from the file versions to the API versions.

## Usage

``` r
convert_api_filter_type(filter_type)
```

## Arguments

- filter_type:

  type of filter being queried: "time_periods", "geographic_levels",
  "locations" or "filter_items"

## Value

String containing API friendly filter type descriptor

## Examples

``` r
eesyapi:::convert_api_filter_type("filter_items")
#> [1] "filters"
eesyapi:::convert_api_filter_type("geographic_levels")
#> [1] "geographicLevels"
eesyapi:::convert_api_filter_type("locations")
#> [1] "locations"
eesyapi:::convert_api_filter_type("filter_items")
#> [1] "filters"
```
