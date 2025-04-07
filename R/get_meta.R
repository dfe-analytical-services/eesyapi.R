#' Get a parsed version of the API response for a data set's meta data
#'
#' @description
#' Get a list of metadata information for a data set available from the EES API. Provides either
#' look-up tables from human readable labels to ids used in the API, or the raw response from the
#' meta endpoint.
#'
#' @inheritParams api_url
#'
#' @return List of data frames containing a data set's meta data
#' @export
#'
#' @examples
#' get_meta(example_id())
get_meta <- function(
  dataset_id,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  verbose = FALSE
) {
  meta_data_response <- get_meta_response(
    dataset_id,
    dataset_version = dataset_version,
    preview_token = preview_token,
    ees_environment = ees_environment,
    api_version = api_version,
    parse = TRUE,
    verbose = verbose
  )
  meta_data <- list(
    time_periods = parse_meta_time_periods(
      meta_data_response$timePeriods,
      verbose = verbose
    ),
    locations = parse_meta_location_ids(
      meta_data_response$locations,
      verbose = verbose
    ),
    filter_columns = parse_meta_filter_columns(
      meta_data_response$filters,
      verbose = verbose
    ),
    filter_items = parse_meta_filter_item_ids(
      meta_data_response$filters,
      verbose = verbose
    ),
    indicators = parse_meta_filter_columns(
      meta_data_response$indicators,
      verbose = verbose
    )
  )
  return(meta_data)
}

#' Get the base API response for a data set's meta data
#'
#' @description
#' Get the metadata information for a data set available from the EES API.
#'
#' @inheritParams api_url
#' @param parse Parse result into structured list
#'
#' @return Results of query to API meta data endpoint
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::get_meta_response(example_id())
get_meta_response <- function(
  dataset_id,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  parse = TRUE,
  verbose = FALSE
) {
  # Check that the parse flag is valid
  if (is.logical(parse) == FALSE) {
    stop(
      "You have entered an invalid parse argument, this should be a logical TRUE or FALSE only."
    )
  }

  # Use eesyapi_url to retrieve the relevant api url - note that this will perform
  # validation checks on dataset_id, dataset_version and api_version, so haven't
  # added any explicit validation of those to the current function.
  meta_url <- api_url(
    endpoint = "get-meta",
    dataset_id = dataset_id,
    dataset_version = dataset_version,
    ees_environment = ees_environment,
    api_version = api_version,
    verbose = verbose
  )

  response <- httr::GET(
    meta_url,
    httr::add_headers(`Preview-Token` = preview_token)
  )
  http_request_error(response)
  if (parse) {
    result <- response |>
      httr::content("text") |>
      jsonlite::fromJSON()
  } else {
    result <- response
  }
  return(result)
}

#' Parse API meta to give the time periods
#'
#' @inheritParams api_url
#' @param api_meta_time_periods Time periods information provided by the API output
#'
#' @return Data frame containing location item codes matched
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::get_meta_response(example_id())$timePeriods |>
#'   eesyapi:::parse_meta_time_periods()
parse_meta_time_periods <- function(api_meta_time_periods, verbose = FALSE) {
  if (!("code" %in% names(api_meta_time_periods))) {
    stop("Code column not found in timePeriods data")
  }
  time_periods <- api_meta_time_periods |>
    dplyr::mutate(
      code_num = as.numeric(gsub("[a-zA-Z]", "", api_meta_time_periods$code))
    )
  time_periods <- time_periods |>
    dplyr::arrange(time_periods$code_num) |>
    dplyr::select(-c("code_num"))
  return(time_periods)
}


#' Parse API meta to give the locations
#'
#' @inheritParams api_url
#' @param api_meta_locations Locations information provided by the API output
#'
#' @return Data frame containing location item codes matched
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::get_meta_response(example_id())$locations |>
#'   eesyapi:::parse_meta_location_ids()
parse_meta_location_ids <- function(api_meta_locations, verbose = FALSE) {
  nlevels <- nrow(api_meta_locations$level)
  for (i in 1:nlevels) {
    location_items_i <- api_meta_locations$options |>
      magrittr::extract2(i) |>
      dplyr::mutate(
        geographic_level_code = api_meta_locations$level$code[i],
        geographic_level = api_meta_locations$level$label[i]
      ) |>
      dplyr::rename(item_id = "id")
    if (verbose) {
      message(paste0("Location level #", i))
    }
    if (verbose) {
      print(location_items_i)
    }
    if (i == 1) {
      location_items <- location_items_i
    } else {
      location_items <- location_items |>
        dplyr::bind_rows(location_items_i)
    }
  }
  if (verbose) {
    message("Collated location levels into single data frame.")
  }
  return(location_items)
}

#' Parse API meta to give the filter columns
#'
#' @inheritParams api_url
#' @param api_meta_filters Filter information provided by the API output
#'
#' @return data frame containing column names and labels
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::get_meta_response(example_id())$filters |>
#'   eesyapi:::parse_meta_filter_columns()
parse_meta_filter_columns <- function(api_meta_filters, verbose = FALSE) {
  data.frame(
    col_id = api_meta_filters$id,
    col_name = api_meta_filters$column,
    label = api_meta_filters$label
  )
}

#' Parse API meta to give the filter item codes
#'
#' @inheritParams api_url
#' @param api_meta_filters Filter information provided by the API output
#'
#' @return Data frame containing filter item codes matched to filter item labels and col_name
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::get_meta_response(example_id())$filters |>
#'   eesyapi:::parse_meta_filter_item_ids()
parse_meta_filter_item_ids <- function(
  api_meta_filters,
  verbose = FALSE
) {
  id <- label <- col_id <- isAggregate <- . <- NULL
  filter_items <- data.frame(
    col_id = api_meta_filters$id,
    col_name = api_meta_filters$column,
    label = api_meta_filters$label
  ) |>
    merge(
      data.table::rbindlist(
        lapply(seq_along(api_meta_filters$options), function(i) {
          data.table::data.table(
            cbind(
              api_meta_filters$options[[i]],
              col_id = api_meta_filters$id[i]
            )
          )
        })
      )[, .(item_id = id, item_label = label, col_id)],
      by = "col_id",
      all.x = TRUE
    ) |>
    data.table::as.data.table()
  if (!("isAggregate" %in% names(filter_items))) {
    filter_items[, isAggregate := NA]
  }
  return(as.data.frame(filter_items))
}
