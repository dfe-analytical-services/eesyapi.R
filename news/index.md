# Changelog

## eesyapi 0.6.0

- Allow wildcard versioning of data sets, e.g. “\*” to get the latest
  possible, or “1.\*” to get the latest within a major version.

## eesyapi 0.5.0

- Updated output data parsing to convert geographic_levels to human
  friendly text
- Updated API base URLs for each environment to planned live ones.
- Added `search` parameter to
  [`get_publications()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_publications.md)
  to allow filtering on publication title text.
- Cleaned up behaviour of
  [`query_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/query_dataset.md)
  to only use geographies as a parameter, rather than geographies,
  locations and geographic_levels.
- Moved all background functions to be internal.
- Added a specific warning message that warns if no rows are returned
  from a valid query.

## eesyapi 0.4.0

- Optimisation to parsing of JSON responses in
  [`query_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/query_dataset.md)
- Rename `download_dataset()` to
  [`preview_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/preview_dataset.md)
  and set default n_max to 10
- Move test data and examples across to the EES test environment data
  sets
- Minor tweaks and improvements to the documentation

## eesyapi 0.3.1

- Added parsing of SQIDs in retrieved data to provide human readable
  content
- Created function, `download_dataset()`, to connect to csv endpoint for
  downloading data set csv file
- Added first draft of example workflow for querying a data set

## eesyapi 0.3.0

- Created capacity to query data using POST:
  - [`query_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/query_dataset.md):
    Now defaults to using POST instead of GET
  - [`post_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/post_dataset.md):
    Sends a query of a data set, either using a json file, json string
    or parameters
- Updated how
  [`example_id()`](https://dfe-analytical-services.github.io/eesyapi/reference/example_id.md)
  works to allow more complex examples

## eesyapi 0.2.1

- Created initial
  [`query_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/query_dataset.md)
  function that queries a data set using
  [`get_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_dataset.md)
- Created
  [`get_dataset()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_dataset.md)
  function that queries a data set using GET and URL parameters
- Updated
  [`get_meta()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_meta.md)
  to work with new API meta output (addition of id alongside col_name
  and label)
- Removed redundant function:
  [`parse_meta_filter_columns()`](https://dfe-analytical-services.github.io/eesyapi/reference/parse_meta_filter_columns.md)
- Hex logo added for documentation

## eesyapi 0.2.0

- Creating publication querying functions:
  - `get_publication_catalogue()`: Retrieve the list of available
    publications
  - `get_publication_datasets()`: Retrieve the list of available data
    sets in a given publication
  - [`example_id()`](https://dfe-analytical-services.github.io/eesyapi/reference/example_id.md):
    Provide example publication and data set IDs (largely for example
    code and testing)
  - [`api_url_pages()`](https://dfe-analytical-services.github.io/eesyapi/reference/api_url_pages.md):
    Render string to set paging on API query results
  - `eesapi_url()` -\>
    [`api_url()`](https://dfe-analytical-services.github.io/eesyapi/reference/api_url.md):
    Name change to function and updated to allow publication and data
    set URLs
  - [`warning_max_pages()`](https://dfe-analytical-services.github.io/eesyapi/reference/warning_max_pages.md):
    Check for the query page number exceeding the total query pages
    available
  - Added some test data and a process for maintaining that data
  - Added the `validate_` family of validation helpers

## eesyapi 0.1.0

- Creating meta data retrieval and parsing functions:
  - [`get_meta()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_meta.md):
    primary function for retrieving parsed R-friendly meta data
  - [`get_meta_response()`](https://dfe-analytical-services.github.io/eesyapi/reference/get_meta_response.md):
    underlying function for retrieving meta data without full parsing
  - [`parse_meta_location_ids()`](https://dfe-analytical-services.github.io/eesyapi/reference/parse_meta_location_ids.md):
    convert location data from initial meta response to simple data
    frame
  - [`parse_meta_filter_columns()`](https://dfe-analytical-services.github.io/eesyapi/reference/parse_meta_filter_columns.md):
    create data frame of filter col_name and label from initial meta
    response
  - [`parse_meta_filter_item_ids()`](https://dfe-analytical-services.github.io/eesyapi/reference/parse_meta_filter_item_ids.md):
    convert filter item data from initial meta response to simple data
    frame
  - `parse_meta_indicator_columns()`: create data frame of indicator
    col_name and label from initial meta response
  - [`http_request_error()`](https://dfe-analytical-services.github.io/eesyapi/reference/http_request_error.md):
    render the API url for a give endpoint / data set combination
