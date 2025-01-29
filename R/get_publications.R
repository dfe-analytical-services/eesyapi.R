#' Get publications
#'
#' @inheritParams api_url
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
#' get_publications()
#' get_publications(search = "attendance")
get_publications <- function(
    ees_environment = NULL,
    api_version = NULL,
    search = NULL,
    page_size = 40,
    page = NULL,
    verbose = FALSE) {
  validate_page_size(page_size)
  if (!is.null(search)) {
    # Replacing any spaces with dashes, this will combine separate strings as an OR query.
    search <- stringr::str_replace_all(search, " ", "-")
    search_substr <- search |>
      strsplit("-") |>
      magrittr::extract2(1)
    search_substr_lengths <- search_substr |>
      stringr::str_length()
    # Check for any of the search strings being shorten than 3 characters.
    if (all(search_substr_lengths < 3)) {
      stop(
        "Individual search string(s) must be 3 characters or longer: \"",
        paste0(search_substr[search_substr_lengths < 3], collapse = "\", \""),
        "\"."
      )
    } else if (any(search_substr_lengths < 3)) {
      warning(
        "The API search will ignore any search strings less than 3 chars in length: \"",
        paste0(search_substr[search_substr_lengths < 3], collapse = "\", \""),
        "\"."
      )
    }
  }
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
