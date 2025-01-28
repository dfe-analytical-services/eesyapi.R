#' Get data set versions
#'
#' @inheritParams api_url
#'
#' @return Data frame listing all available versions of the given data set
#' @export
#'
#' @examples
#' get_dataset_versions(dataset_id = example_id(group="attendance"))
get_dataset_versions <- function(
    dataset_id,
    ees_environment = NULL,
    api_version = NULL,
    page_size = 40,
    page = NULL,
    verbose = FALSE) {
  validate_page_size(page_size)
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
  return(response$results)
}
