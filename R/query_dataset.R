#' Query a data set
#'
#' @description
#' Create and send a query to the EES API. Queries can be supplied and run in one of 4 ways:
#'   - Supplying a json query in a file to be sent with the POST method.
#'   - Supplying a json query in a string variable to be sent with the POST method.
#'   - Supplying parameters (time_periods, geographies, filter_items, indicators) to build a json
#'   query is then sent with the POST method.
#'   - Supplying parameters (time_periods, geographies, filter_items, indicators) to build a json
#'   query is then sent with the GET method.
#'
#' In all cases, the data set id must be supplied explicitly using the dataset_id.
#'
#' Details on the format of each parameter for the latter two methods are as follows.
#'
#' ## indicators
#'
#' This must be supplied as a vector of sqids, which can be identified from the meta data using
#' `get_meta()`.
#'
#' ## time_periods
#'
#' Time periods should be supplied as a vector of periods and codes in the form:
#'   - `"period|code"`
#'
#' For example, selecting the 2023 and 2024 academic years would require:
#' `time_period = c("2023|AY", "2024|AY")`
#'
#' ## geographies
#'
#' Geographies can be supplied as a single string, a vector of strings, a list or a data frame
#' depending on user preference and the complexity of the desired query.
#'
#' The user can supply a **single string** containing the desired geographic level (e.g. `"NAT"`,
#' `"REG"`, `"LA"`) or a specific location (e.g. `"NAT|code|E92000001"`, `"REG|code|E12000001"`)
#' and the query will search for rows at that single level or location.
#'
#' Specific locations are required to be supplied in the format
#' `"location_level|location_id_type|location_id"`, where location_id_type can be either code or id
#' and the corresponding location_id is then the standard ONS code or the sqid given in the meta
#' data respectively. Using England as an example, these would be:
#'   - `"NAT|code|E92000001"`
#'   - `"NAT|id|dP0Zw"`
#'
#' Using a **vector** will run a query returning **any** rows meeting any of the given geographies,
#' i.e. geographies = `c("NAT", "REG")` will return all national and regional level rows, whilst
#' `c("NAT", "REG|code|E120000001")` will return all national level rows and all North
#' East rows.
#'
#' If you require a more complex selection, for example all LAs in a given region, then a **data
#' frame** should be supplied, with a row for each selection. Note however, that this will work
#' best when using the default `POST` method. The `GET` method is much more limited and may
#' struggle to process more complex queries.
#'
#' The geography query data frame should contain the following columns:
#'   - geographic_level: the geographic level to return (e.g. LA in the example above).
#'   - location_level: the geographic level of the search location (e.g. REG in the example above).
#'   - location_id_type: "code" or "id".
#'   - location_id: the code or id (sqid) for the search location (e.g. the code or sqid of the
#'   region in the above example).

#' Further rows can be added to add other geography searches to include in results.
#'
#' An example of a working geographies data frame can be obtained using `example_geography_query()`.
#'
#' A **list** object will also be understood, provided it only contains the named items
#' `geographic_level` and / or `location`. This are defined similarly to the vector form above:
#'   - `list(geographic_level=c("REG", "LA"), locations = c("REG|code|E12000001")`
#'
#' ## filter_items
#'
#' Similarly to geographies, criteria for querying on filter items can be provided either via a
#' vector for simple queries or, for more complex queries, as a list. In both cases, vector or
#' list, filter items can only be supplied as sqids, i.e. the ids found in the meta data using
#' `get_meta(dataset_id)`.
#'
#' Providing a vector of sqids will effectively run a query returning any rows containing any of
#' the listed sqids. This therefore does not allow narrow searches based on a row or set of rows
#' matching multiple criteria.
#'
#' Providing a list structure can provide a more narrow query selecting individual rows based on
#' a combination of criteria. For example if we want rows that contain sqid1 in one column and
#' sqid2 in another, then we would pass a list of:
#' `filter_query <- list(column1 = c("sqid1"), column2 = c("sqid2"))`
#' Note that the naming of the entries in the list is not necessary, but may help in creating more
#' readable code.
#'
#' If we wish to create a query whereby we receive rows containing sqid1 in column 1 and sqid2 or
#' sqid3 in column 2, then the required list would be:
#' `filter_query <- list(column1 = c("sqid1"), column2 = c("sqid2", "sqid3"))`
#'
#' In this way, we can build up combinations of OR and AND criteria for a query across multiple
#' filter columns.
#'
#' *Note again that the more complex querying using a list variable will only function when using
#' the `POST` method*.
#'
#' ## Controlling paging
#'
#' You can request a specific set of rows using the page and page_size parameters. Keeping the
#' default of page = NULL will return all rows matching the query. Setting page and page_size to
#' numerical values will attempt to return a subset of rows, with page_size defining the number of
#' rows and page defining which subset of rows to return from the query (i.e. page = 1, page_size =
#'  20 will return the first 20 rows, page = 2 and page_size = 20 will return the second 20 rows
#'  and so on).
#'
#' @inheritParams api_url
#' @inheritParams parse_tojson_params
#' @param preview_token Preview token required for access to private data sets
#' @param json_query Optional path to a json file containing the query parameters
#' @param parse Logical flag to activate parsing of the results. Default: TRUE
#' @param method The API query method to be used. Can be  "POST" or "GET". Default: "POST".
#'
#' @return Data frame containing query results
#' @export
#'
#' @examples
#' # Run query_dataset() using a json query string input to json_query (this can also be done by
#' # passing a filename of a file containing your json query string).
#' query_dataset(
#'   example_id(group = "attendance"),
#'   json_query = example_json_query()
#' )
#'
#' # If you don't want to have to write your own json query, the rest of the examples illustrate
#' # how to use query_dataset() with parameters to construct queries in R.
#'
#' # Run query_dataset() to select rows containing either of two geographic locations and either of
#' # two filter items.
#' query_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = example_id("time_period", group = "attendance"),
#'   geographies = c("NAT|code|E92000001", "REG|code|E12000001"),
#'   filter_items = example_id("filter_item", group = "attendance"),
#'   page = 1,
#'   page_size = 32
#' )
#'
#' # Run query_dataset() using set parameters giving a combination of filter options
#' example_filter_list_input <- example_id("filter_items_short", group = "attendance")
#' print(example_filter_list_input)
#' query_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = example_id("time_period", group = "attendance"),
#'   geographies = "NAT",
#'   filter_items = example_filter_list_input
#' )
#'
#' # Run a query with a more complex geography selection. Return data for all of:
#' #   - England
#' #   - Yorkshire and the Humber
#' #   - All LAs in Yorkshire and the Humber
#' query_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = example_id("time_period", group = "attendance"),
#'   geographies = data.frame(
#'     geographic_level = c("NAT", "REG", "LA"),
#'     locations = c("NAT|code|E92000001", "REG|code|E12000003", "REG|code|E12000003")
#'   ),
#'   filter_items = example_id("filter_item", group = "attendance")
#' )
#'
#' # Run a basic query using GET instead of POST
#' query_dataset(
#'   example_id(),
#'   method = "GET",
#'   geographies = "NAT",
#'   filter_items = example_id("filter_item"),
#'   indicators = example_id("indicator"),
#'   page = 1,
#'   page_size = 10
#' )
#'
query_dataset <- function(
  dataset_id,
  indicators = NULL,
  time_periods = NULL,
  geographies = NULL,
  filter_items = NULL,
  json_query = NULL,
  method = "POST",
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  page_size = 10000,
  parse = TRUE,
  page = NULL,
  debug = FALSE,
  verbose = FALSE
) {
  if (!(method %in% c("POST", "GET"))) {
    stop(
      paste(
        "Invalid method selected. The keyword method should be set to GET",
        "(an option to use POST is being developed)."
      )
    )
  }
  if (is.null(indicators) && (is.null(json_query) || method == "GET")) {
    warning(
      "No indicators provided, defaulted to using all indicators from meta data"
    )
    indicators <- get_meta(
      dataset_id,
      dataset_version = dataset_version,
      preview_token = preview_token,
      ees_environment = ees_environment,
      api_version = api_version,
      verbose = verbose
    ) |>
      magrittr::extract2("indicators") |>
      dplyr::pull("col_id")
  }
  if (method == "POST") {
    post_dataset(
      dataset_id = dataset_id,
      indicators = indicators,
      time_periods = time_periods,
      geographies = geographies,
      filter_items = filter_items,
      json_query = json_query,
      dataset_version = dataset_version,
      preview_token = preview_token,
      ees_environment = ees_environment,
      api_version = api_version,
      page_size = page_size,
      page = page,
      parse = parse,
      debug = debug,
      verbose = verbose
    )
  } else {
    warning(
      paste(
        "Using GET to query a data set offers limited functionality, we recommend",
        "using POST alongside a JSON structured query instead:\n",
        "  - query_dataset(..., method = 'POST')"
      )
    )
    # Get the geographies input into a standard format
    geographies <- todf_geographies(geographies)
    if (any(geographies$geographic_level != "")) {
      geographic_levels <- geographies |>
        dplyr::distinct() |>
        dplyr::filter(!!rlang::sym("geographic_level") != "") |>
        dplyr::pull("geographic_level")
    } else {
      geographic_levels <- NULL
    }
    geographies <- geographies |>
      dplyr::mutate(
        locations = paste0(
          !!rlang::sym("location_level"),
          "|",
          !!rlang::sym("location_id_type"),
          "|",
          !!rlang::sym("location_id")
        ),
        locations = stringr::str_replace_all(
          !!rlang::sym("locations"),
          "\\|\\|",
          ""
        )
      )
    if (any(geographies$locations != "")) {
      locations <- geographies |>
        dplyr::distinct() |>
        dplyr::filter(locations != "") |>
        dplyr::pull("locations")
    } else {
      locations <- NULL
    }
    toggle_message(
      paste("geographic_levels: ", paste0(geographic_levels, collapse = ",")),
      verbose = verbose
    )
    toggle_message(
      paste("locations: ", paste0(locations, collapse = ",")),
      verbose = verbose
    )
    # Now run the GET query
    get_dataset(
      dataset_id = dataset_id,
      indicators = indicators,
      time_periods = time_periods,
      geographic_levels = geographic_levels,
      locations = locations,
      filter_items = filter_items,
      dataset_version = dataset_version,
      preview_token = preview_token,
      ees_environment = ees_environment,
      api_version = api_version,
      page_size = page_size,
      page = page,
      parse = parse,
      verbose = verbose
    )
  }
}
