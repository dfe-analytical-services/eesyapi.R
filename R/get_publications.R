#' Get publications
#'
#' @inheritParams api_url
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
#' get_publications()
get_publications <- function(
    ees_environment = NULL,
    api_version = NULL,
    search = NULL,
    page_size = 40,
    page = NULL,
    verbose = FALSE) {
  validate_page_size(page_size)
  response <- httr::GET(
    api_url(
      ees_environment = ees_environment,
      api_version = api_version,
      search = search,
      page_size = page_size,
      page = page,
      verbose = verbose
    )
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
  if (!is.null(search)) {
    if (stringr::str_length(search) < 3) {
      stop("Search string must be 3 characters or longer.")
    }
  }
  # Unless the user specifies a specific page of results to get, loop through all available pages.
  if (is.null(page)) {
    if (response$paging$totalPages > 1) {
      for (page in c(2:response$paging$totalPages)) {
        response_page <- httr::GET(
          api_url(
            ees_environment = ees_environment,
            api_version = api_version,
            search = search,
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
  response |>
    warning_max_pages() |>
    warning_no_rows()
  return(response$results)
}
