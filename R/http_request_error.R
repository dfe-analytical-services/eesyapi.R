#' Contextualise http request errors
#'
#' @description
#' Translate a http error code into an error message.
#'
#' @param response HTTP response from the API, should be in the form of a list with a
#' status element containing the 3 digit HTTP response code
#' @param verbose Run in verbose mode, logical, default = FALSE
#'
#'
#' @return Translation of the response code
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::http_request_error(list(status = 200))
http_request_error <- function(
    response,
    verbose = FALSE) {
  status_lookup <- data.frame(
    response_group = c(
      2,
      4,
      5
    ),
    response_text = c(
      "Successful API request.",
      paste(
        "Invalid query, data set ID, data set version or API version submitted to API."
      ),
      paste(
        "Internal server error encountered - please contact the EES API team at",
        "explore.statistics@education.gov.uk",
        "providing the query you were attempting to submit."
      )
    )
  )

  status_group <- trunc(response$status / 100.)
  if (status_group %in% status_lookup$response_group) {
    status_response_text <- status_lookup |>
      dplyr::filter(status_lookup$response_group == status_group) |>
      dplyr::pull("response_text")
    if (!(status_group %in% c(2, 5))) {
      api_error <- response |>
        httr::content("text") |>
        jsonlite::fromJSON() |>
        magrittr::extract2("errors")
      if (!is.null(api_error)) {
        error_message <- api_error |>
          dplyr::pull("message") |>
          unique()
        error_detail <- api_error |>
          dplyr::pull("detail")
        toggle_message(api_error |> dplyr::pull("message"), verbose = verbose)
        status_response_text <- paste0(
          error_message,
          ifelse(
            "items" %in% names(error_detail),
            paste0("\n     Error items: ",error_detail |>
              dplyr::pull("items") |>
              unlist() |>
              paste0(collapse = ", ")
              ),
            ""
          ),
          ifelse(
            "value" %in% names(error_detail),
            paste0(
              "\n     Provided values: ",
              error_detail |>
                dplyr::pull("value") |>
                unlist() |>
                paste0(collapse = ", ")
            ),
            ""
          ),
          ifelse(
            "allowed" %in% names(error_detail),
            paste0(
              "\n     Allowed values: ",
              error_detail |>
                dplyr::pull("allowed") |>
                unlist() |>
                paste0(collapse = ", ")
            ),
            ""
          )
        )
      }
    }
  } else {
    status_response_text <- "API http response code not recognised."
  }
  if (status_group != 2) {
    stop(
      paste0(
        "\nHTTP connection error: ",
        response$status,
        "\n",
        status_response_text
      )
    )
  }
  return(status_response_text)
}
