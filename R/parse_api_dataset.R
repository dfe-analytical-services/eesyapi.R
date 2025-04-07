#' Parse contents of API data set json output
#'
#' @description
#' This takes the json results output from the API and converts it into a usable data frame.
#' It's primary use is as a helper to get_dataset and post_dataset, but can be used in
#' isolation from those by running an API query and passing the following to
#' parse_api_dataset():
#'    response |>
#'       httr::content("text") |>
#'       jsonlite::fromJSON() |>
#'       parse_api_dataset()
#'
#' @param api_data_result A json data result list as returned from the API
#' @inheritParams api_url
#'
#' @return Data frame containing API data results
#'
#' @keywords internal
#'
#' @examples
#' example_data_raw(group = "attendance") |>
#'   eesyapi:::parse_api_dataset(example_id(group = "attendance"))
parse_api_dataset <- function(
  api_data_result,
  dataset_id,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  verbose = FALSE
) {
  if (!is.null(dataset_id)) {
    validate_ees_id(dataset_id, level = "dataset")
  }
  if ("results" %in% names(api_data_result)) {
    api_data_result <- api_data_result$results
  }
  if (length(api_data_result) == 0) {
    # TODO: It would be nice to have the original API warning messages to pass directly here
    if (!verbose) {
      warning(
        call. = FALSE, # Function is internal only so we don't want to show the call
        "No rows were returned for your query. Set verbose = TRUE to see detailed API response."
      )
    } else {
      warning(
        call. = FALSE, # Function is internal only so we don't want to show the call
        "No rows were returned for your query."
      )
    }
    return(invisible(NULL)) # Preventing any further parsing as we would get errors
  }
  if (verbose) {
    print(names(api_data_result))
    print(names(api_data_result$locations))
    print(names(api_data_result$filters))
  }
  meta <- get_meta(
    dataset_id,
    dataset_version = dataset_version,
    ees_environment = ees_environment,
    api_version = api_version,
    preview_token = preview_token
  )
  dplyr::bind_cols(
    api_data_result$timePeriod |>
      parse_time_codes(),
    api_data_result$geographicLevel |>
      parse_geographic_level_codes(),
    api_data_result$locations |>
      parse_sqids_locations(meta),
    api_data_result$filters |>
      parse_sqids_filters(meta),
    api_data_result$values |>
      parse_sqids_indicators(meta),
  )
  # Next aim here is to pull in the meta data automatically at this point to translate
  # all the API codes...
}
