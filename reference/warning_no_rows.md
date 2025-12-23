# Warn on zero rows returned

Warn on zero rows returned

## Usage

``` r
warning_no_rows(api_result)
```

## Arguments

- api_result:

  Output from an API get query

## Value

Original input (api_result) unchanged

## Examples

``` r
response_page <- httr::GET(api_url("get-publications", search = "bob")) |>
  httr::content("text") |>
  jsonlite::fromJSON() |>
  eesyapi:::warning_no_rows()
#> Warning: Your query returned zero rows.
```
