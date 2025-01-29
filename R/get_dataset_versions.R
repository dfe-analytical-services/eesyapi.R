#' Get data set versions
#'
#' @inheritParams api_url
#' @param detail Level of detail to return. Given as a character string, it should be one of:
#' "light" (default) or "full".
#'
#' @return Data frame listing all available versions of the given data set
#' @export
#'
#' @examples
#' get_dataset_versions(dataset_id = example_id(group="attendance"))
get_dataset_versions <- function(
    dataset_id,
    detail = "light",
    ees_environment = NULL,
    api_version = NULL,
    page_size = 40,
    page = NULL,
    verbose = FALSE) {
  # Do some basic validation
  validate_page_size(page_size)

  detail <- tolower(detail)
  if(!(detail %in% c("light", "full"))){
    stop("The detail parameter should be either \"light\" or \"full\". Value passed: ", detail)
  }

  # Make the initial API request
  response <- httr::GET(
    api_url(
      dataset_id = dataset_id,
      endpoint = "get-dataset-versions",
      ees_environment = ees_environment,
      api_version = api_version,
      page_size = page_size,
      page = page,
      verbose = verbose
    )
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
  # Unless the user specifies a specific page of results to get, loop through all available pages.
  if (is.null(page)) {
    if (response$paging$totalPages > 1) {
      for (page in c(2:response$paging$totalPages)) {
        response_page <- httr::GET(
          api_url(
            dataset_id = dataset_id,
            endpoint = "get-dataset-versions",
            ees_environment = ees_environment,
            api_version = api_version,
            page_size = page_size,
            page = page,
            verbose = verbose
          )
        ) |>
          httr::content("text") |>
          jsonlite::fromJSON()
        response$results <- response$results |>
          rbind(response_page$results)
      }
    }
  }
  response |> warning_max_pages()
    results <- response$results |>
      dplyr::select("version", "type", total_rows = "totalResults") |>
      cbind(date_released = as.Date(response$results$published))|>
      cbind(version_title = response$results$release$title)|>
      cbind(time_period_start = response$results$timePeriod$start)|>
      cbind(time_period_end = response$results$timePeriod$end)
  if(detail == "full"){
      results <- results |>
        cbind(version_id = response$results$file$id)|>
        cbind(
          response$results |>
            dplyr::select(
              "status",
              "notes",
              geographic_levels = "geographicLevels",
              "filters",
              "indicators"
              )
          )
    }
  return(results)
}
