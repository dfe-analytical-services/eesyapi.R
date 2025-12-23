# Query a data set using POST and a query json

This function provides a method for generating and sending a json based
data query to the EES API. As a minimum, it requires the dataset_id flag
and either the indicators flag or a json file containing a query to be
provided.

## Usage

``` r
post_dataset(
  dataset_id,
  indicators = NULL,
  time_periods = NULL,
  geographies = NULL,
  filter_items = NULL,
  json_query = NULL,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  page = NULL,
  page_size = 10000,
  parse = TRUE,
  debug = FALSE,
  verbose = FALSE
)
```

## Arguments

- dataset_id:

  ID of data set to be connected to. This is required if the endpoint is
  one of "get-dataset-versions", "get-summary", "get-meta", "get-csv",
  "get-data" or "post-data"

- indicators:

  Indicators required as a string or vector of strings (required)

- time_periods:

  Time periods required as a string ("period\|code") or vector of
  strings

- geographies:

  String, vector or data frame containing the geographic levels and
  locations to be queried.

- filter_items:

  Filter items required as a string or vector of strings

- json_query:

  Optional path to a json file containing the query parameters

- dataset_version:

  Version of data set to be connected to, in "major.minor.patch" format,
  with optional wildcards, e.g. "*", "2.*", "2.1.\*", "2.1.0". Can also
  be provided as a numeric value

- preview_token:

  Preview token required for access to private data sets

- ees_environment:

  EES ees_environment to connect to: "dev", "test", "preprod" or "prod"

- api_version:

  EES API version

- page:

  Page number of query results to return

- page_size:

  Number of results to return in a single query

- parse:

  Logical flag to activate parsing of the results. Default: TRUE

- debug:

  Run POST query in debug mode. Logical, default = FALSE

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

Data frame containing query results of an API data set

## Examples

``` r
eesyapi:::post_dataset(
  example_id(group = "attendance"),
  json_query = example_json_query()
)
#> Warning: No rows were returned for your query. Set verbose = TRUE to see detailed API response.
#> NULL

# Run post_dataset() to select rows containing either of two geographic locations and a single
# filter item.
eesyapi:::post_dataset(
  example_id(group = "attendance"),
  indicators = example_id("indicator", group = "attendance"),
  time_periods = example_id("time_period", group = "attendance"),
  geographies = example_id("location_code", group = "attendance"),
  filter_items = example_id("filter_item", group = "attendance"),
  page = 1,
  page_size = 32
)
#> Warning: No rows were returned for your query. Set verbose = TRUE to see detailed API response.
#> NULL
```
