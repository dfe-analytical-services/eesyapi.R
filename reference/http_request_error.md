# Contextualise http request errors

Translate a http error code into an error message.

## Usage

``` r
http_request_error(response, verbose = FALSE)
```

## Arguments

- response:

  HTTP response from the API, should be in the form of a list with a
  status element containing the 3 digit HTTP response code

- verbose:

  Run in verbose mode, logical, default = FALSE

## Value

Translation of the response code

## Examples

``` r
eesyapi:::http_request_error(list(status = 200))
#> [1] "Successful API request."
```
