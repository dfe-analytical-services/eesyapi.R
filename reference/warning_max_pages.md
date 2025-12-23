# Overshot maximum pages in results

Overshot maximum pages in results

## Usage

``` r
warning_max_pages(api_result)
```

## Arguments

- api_result:

  Output from an API get query

## Value

Original input (api_result) unchanged

## Examples

``` r
response_page <- httr::GET(api_url(page_size = 10, page = 1)) |>
  httr::content("text") |>
  jsonlite::fromJSON() |>
  eesyapi:::warning_max_pages()
```
