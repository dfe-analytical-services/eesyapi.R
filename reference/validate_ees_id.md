# Validate element IDs

Validate element IDs

## Usage

``` r
validate_ees_id(element_id, level = "publication", verbose = FALSE)
```

## Arguments

- element_id:

  ID for publication or a data set

- level:

  ID level: "publication", "dataset", "location", "filter_item" or
  "indicator"

- verbose:

  Run in verbose mode

## Examples

``` r
eesyapi:::validate_ees_id(example_id("publication"), level = "publication")
```
