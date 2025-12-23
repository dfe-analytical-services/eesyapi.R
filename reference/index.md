# Package index

## Primary API workflow tools for analysts

These are the functions we expect analysts will most likely want to use
to connect to the EES API.

- [`get_publications()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_publications.md)
  : Get publications
- [`get_data_catalogue()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_data_catalogue.md)
  : Get publication specific data set catalogue
- [`get_dataset_versions()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_dataset_versions.md)
  : Get data set versions
- [`get_meta()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_meta.md)
  : Get a parsed version of the API response for a data set's meta data
- [`preview_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/preview_dataset.md)
  : Preview the raw CSV for an API data set
- [`query_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/query_dataset.md)
  : Query a data set

## Support for generating API URLs and interpreting responses

These functions are helpful for deriving urls and handling HTTP
responses and are used widely by the API workflow functions.

- [`api_url()`](https://dfe-analytical-services.github.io/eesyapi/reference/api_url.md)
  : Generate an EES API URL

## Generate example IDs and reference data

These functions are used widely to create working example code and tests

- [`example_data_raw()`](https://dfe-analytical-services.github.io/eesyapi/reference/example_data_raw.md)
  : Example raw data
- [`example_geography_query()`](https://dfe-analytical-services.github.io/eesyapi/reference/example_geography_query.md)
  : Create an example geography-query data frame
- [`example_id()`](https://dfe-analytical-services.github.io/eesyapi/reference/example_id.md)
  : Example ID
- [`example_json_query()`](https://dfe-analytical-services.github.io/eesyapi/reference/example_json_query.md)
  : Create an example json query string
- [`geog_level_lookup`](https://dfe-analytical-services.github.io/eesyapi/reference/geog_level_lookup.md)
  : Look-up for API geographic_level shorthands
