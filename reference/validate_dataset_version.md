# Validate data set version

Ensures that the data set version is in the correct format, either as a
string following the 'major.minor.patch' format (with optional '\*'
wildcards), or as a numeric value.

## Usage

``` r
validate_dataset_version(dataset_version)
```

## Arguments

- dataset_version:

  Chosen data set version

## Value

Logical

## Details

Numeric values are kept for backwards compatibility, but expect strings
will be more commonly used.

## Examples

``` r
eesyapi:::validate_dataset_version("2.0.0")
```
