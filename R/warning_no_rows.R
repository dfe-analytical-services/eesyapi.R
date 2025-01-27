#' Warn on zero rows returned
#'
#' @param api_result Output from an API get query
#'
#' @return Original input (api_result) unchanged
#'
#' @keywords internal
#'
#' @examples
#' response_page <- httr::GET(api_url("get-publications", search = "bob")) |>
#'   httr::content("text") |>
#'   jsonlite::fromJSON() |>
#'   eesyapi:::warning_no_rows()
warning_no_rows <- function(api_result) {
  if (api_result$paging$totalResults == 0) {
    warning(
      paste0(
        "Your query returned zero rows."
      )
    )
  }
  return(api_result)
}
