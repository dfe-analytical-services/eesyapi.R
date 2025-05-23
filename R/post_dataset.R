#' Query a data set using POST and a query json
#'
#' @description
#' This function provides a method for generating and sending a json based data query to the
#' EES API. As a minimum, it requires the dataset_id flag and either the indicators flag or
#' a json file containing a query to be provided.
#'
#' @inheritParams api_url
#' @inheritParams parse_tojson_params
#' @inheritParams query_dataset
#'
#' @return Data frame containing query results of an API data set
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::post_dataset(
#'   example_id(group = "attendance"),
#'   json_query = example_json_query()
#' )
#'
#' # Run post_dataset() to select rows containing either of two geographic locations and a single
#' # filter item.
#' eesyapi:::post_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = example_id("time_period", group = "attendance"),
#'   geographies = example_id("location_code", group = "attendance"),
#'   filter_items = example_id("filter_item", group = "attendance"),
#'   page = 1,
#'   page_size = 32
#' )
#'
post_dataset <- function(
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
) {
  if (is.null(indicators) && is.null(json_query)) {
    stop("At least one of either indicators or json_query must not be NULL.")
  }
  # Get the geographies input into a standard format
  geographies <- todf_geographies(geographies)
  if (!is.null(json_query)) {
    if (any(!is.null(c(indicators, time_periods, geographies, filter_items)))) {
      warning(
        paste(
          "json_query is set - ignoring indicators, time_periods, geographies",
          "and filter_items params."
        )
      )
    }
    if (json_query |> stringr::str_sub(-5) == ".json") {
      json_body <- readLines(json_query) |>
        paste0(collapse = "\n")
    } else {
      if (verbose) {
        message("Parsing query options")
      }
      json_body <- json_query
    }
  } else {
    json_body <- parse_tojson_params(
      indicators = indicators,
      time_periods = time_periods,
      geographies = geographies,
      filter_items = filter_items,
      page = page,
      page_size = page_size,
      debug = debug,
      verbose = verbose
    )
  }
  if (verbose) {
    json_body |> cat(fill = TRUE)
  }
  response <- api_url(
    "post-data",
    dataset_id = dataset_id,
    dataset_version = dataset_version,
    ees_environment = ees_environment,
    api_version = api_version,
    verbose = verbose
  ) |>
    httr::POST(
      body = json_body,
      encode = "json",
      httr::content_type("application/json"),
      httr::add_headers(`Preview-Token` = preview_token)
    )
  if (verbose) {
    print(response)
    print(
      response |>
        httr::content("text") |>
        jsonlite::fromJSON()
    )
  }
  http_request_error(response, verbose = verbose)
  # Unless the user specifies a specific page of results to get, loop through all available pages.
  response_json <- response |>
    httr::content("text") |>
    jsonlite::fromJSON()
  if (verbose) {
    message(paste("Total number of pages: ", response_json$paging$totalPages))
  }
  dfresults <- response_json |>
    magrittr::extract2("results")

  # Unless the user has requested a specific page, then assume they'd like all pages collated and
  # recursively run the query.
  if (is.null(page) && is.null(json_query)) {
    if (response_json$paging$totalPages > 1) {
      if (response_json$paging$totalPages * page_size > 100000) {
        message(
          paste(
            "Downloading up to",
            response_json$paging$totalPages * page_size,
            "rows.",
            "This may take a while.",
            "We recommend downloading the full data set using preview_dataset()",
            "for large volumes of data."
          )
        )
      }
      for (page in c(2:response_json$paging$totalPages)) {
        json_body <- parse_tojson_params(
          indicators = indicators,
          time_periods = time_periods,
          geographies = geographies,
          filter_items = filter_items,
          page = page,
          page_size = page_size,
          verbose = verbose
        )
        response_page <- api_url(
          "post-data",
          dataset_id = dataset_id,
          dataset_version = dataset_version,
          ees_environment = ees_environment,
          api_version = api_version
        ) |>
          httr::POST(
            body = json_body,
            encode = "json",
            httr::content_type("application/json"),
            httr::add_headers(`Preview-Token` = preview_token)
          ) |>
          httr::content("text") |>
          jsonlite::fromJSON()
        response_page |> warning_max_pages()
        dfresults <- dfresults |>
          dplyr::bind_rows(
            response_page |>
              magrittr::extract2("results")
          )
      }
    }
  }
  if (parse) {
    dfresults <- dfresults |>
      parse_api_dataset(
        dataset_id,
        dataset_version,
        preview_token = preview_token,
        ees_environment = ees_environment,
        verbose = verbose
      )
  }
  return(dfresults)
}
