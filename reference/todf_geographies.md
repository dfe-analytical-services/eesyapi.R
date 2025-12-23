# Standardise geography inputs

Create a standard data frame containing geographic_level and location
for use by query_dataset().

## Usage

``` r
todf_geographies(geographies)
```

## Arguments

- geographies:

  String, vector, list or data frame containing the geographic levels
  and locations to be queried.

## Value

data.frame containing standardised geography specification

## Examples

``` r
eesyapi:::todf_geographies(c("NAT", "REG"))
#>   geographic_level location_level location_id_type location_id
#> 1              NAT                                            
#> 2              REG                                            
eesyapi:::todf_geographies(c("NAT|code|E92000001", "REG|code|E12000001"))
#>   location_level location_id_type location_id geographic_level
#> 1            NAT             code   E92000001              NAT
#> 2            REG             code   E12000001              REG
eesyapi:::todf_geographies(c("NAT|code|E92000001", "REG"))
#>   location_level location_id_type location_id geographic_level
#> 1            NAT             code   E92000001              NAT
#> 2                                                          REG
eesyapi:::todf_geographies(
  list(
    geographic_level = c("REG", "LA"),
    locations = c("REG|code|E12000001")
  )
)
#>   geographic_level location_level location_id_type location_id
#> 1              REG            REG             code   E12000001
#> 2               LA            REG             code   E12000001
eesyapi:::todf_geographies(
  data.frame(locations = c("REG|code|E12000001", "REG|code|E12000001"))
)
#> # A tibble: 1 × 4
#>   location_level location_id_type location_id geographic_level
#>   <chr>          <chr>            <chr>       <chr>           
#> 1 REG            code             E12000001   REG             
eesyapi:::todf_geographies(
  data.frame(
    geographic_level = c("REG", "LA"),
    locations = c("REG|code|E12000001", "REG|code|E12000001")
  )
)
#> # A tibble: 2 × 4
#>   geographic_level location_level location_id_type location_id
#>   <chr>            <chr>          <chr>            <chr>      
#> 1 REG              REG            code             E12000001  
#> 2 LA               REG            code             E12000001  
eesyapi:::todf_geographies(
  data.frame(geographic_level = c("NAT", "REG"))
)
#>   geographic_level location_level location_id_type location_id
#> 1              NAT                                            
#> 2              REG                                            
```
