#' Parse geographies for data set query
#'
#' @description
#' Create a json query sub-string based on location constraints
#'
#' @param geographies String, vector or data frame containing the geographic levels and
#' locations to be queried.
#'
#' @return String containing json form query for geographies
#' @export
#'
#' @examples
#' parse_todf_geographies(c("NAT", "REG")) |>
#'   cat()
#' parse_todf_geographies(c("NAT|id|dP0Zw", "REG|id|rg3Nj")) |>
#'   cat()
#' parse_todf_geographies(c("NAT|id|dP0Zw", "REG")) |>
#'   cat()
#' parse_todf_geographies(c("NAT|id|dP0Zw", "REG")) |>
#'   cat()
parse_todf_geographies <- function(geographies) {
  standard_columns <- c(
    "geographic_level",
    "location_level",
    "location_id_type",
    "location_id"
  )
  if (is.null(geographies)) {
    return(NULL)
  } else if (is.vector(geographies) || is.character(geographies)) {
    geographies <- geographies |>
      stringr::str_split("\\|", simplify = TRUE) |>
      as.data.frame()
    if (ncol(geographies) == 1) {
      geographies <- geographies |>
        dplyr::mutate(
          V2 = "",
          V3 = ""
        )
    }
    geographies <- geographies |>
      dplyr::rename(location_level = "V1", location_id_type = "V2", location_id = "V3") |>
      dplyr::mutate(geographic_level = !!rlang::sym("location_level"))
  } else if (is.data.frame(geographies)) {
    if (
      all(
        c(
          "location_level",
          "location_id_type",
          "location_id"
        ) %in%
          colnames(geographies)
      )
    ) {
      if (!("geographic_level" %in% colnames(geographies))) {
        geographies <- geographies |>
          dplyr::mutate(geographic_level = !!rlang::sym("location_level"))
      }
    } else if (all(colnames(geographies) == "geographic_level")) {
      geographies <- geographies |>
        dplyr::mutate(
          location_level = "",
          location_id_type = "",
          location_id = !!rlang::sym("geographic_level")
        )
    } else {
      stop("Invalid geographies data-frame provided - please check the geographies guide.")
    }
  } else {
    stop("The geographies parameter should be given as either a data frame, vector or string.")
  }
  return(geographies)
}
