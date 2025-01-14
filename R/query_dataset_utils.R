#' Standardise geography inputs
#'
#' @description
#' Create a standard data frame containing geographic_level and location for use by
#' query_dataset().
#'
#' @param geographies String, vector, list or data frame containing the geographic levels and
#' locations to be queried.
#'
#' @return data.frame containing standardised geography specification
#'
#' @keywords internal
#'
#' @examples
#' eesyapi:::todf_geographies(c("NAT", "REG"))
#' eesyapi:::todf_geographies(c("NAT|code|E92000001", "REG|code|E12000001"))
#' eesyapi:::todf_geographies(c("NAT|code|E92000001", "REG"))
#' eesyapi:::todf_geographies(
#'   list(
#'     geographic_level = c("REG", "LA"),
#'     location = c("REG|code|E12000001")
#'   )
#' )
#' eesyapi:::todf_geographies(
#'   data.frame(location = c("REG|code|E12000001", "REG|code|E12000001"))
#' )
#' eesyapi:::todf_geographies(
#'   data.frame(
#'     geographic_level = c("REG", "LA"),
#'     location = c("REG|code|E12000001", "REG|code|E12000001")
#'   )
#' )
#' eesyapi:::todf_geographies(
#'   data.frame(geographic_level = c("NAT", "REG"))
#' )
todf_geographies <- function(geographies) {
  if (is.null(geographies)) {
    return(NULL)
  } else if (is.data.frame(geographies)) {
    if ("location" %in% names(geographies)) {
      geographies <- geographies |>
        tidyr::separate_wider_delim(
          "location",
          "|",
          names = c("location_level", "location_id_type", "location_id")
        )
    }
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
          location_id = ""
        )
    } else {
      stop("Invalid geographies data-frame provided - please check the geographies guide.")
    }
  } else if (is.list(geographies)) {
    if (any(!(names(geographies) %in% c("geographic_level", "location")))) {
      stop(
        paste(
          "Input geographies list should contain only \"geographic_level\" and / or ",
          "\"location\""
        )
      )
    }
    if ("location" %in% names(geographies)) {
      locations <- geographies$location |>
        stringr::str_split("\\|", simplify = TRUE) |>
        as.data.frame() |>
        dplyr::rename(location_level = "V1", location_id_type = "V2", location_id = "V3") |>
        dplyr::distinct()
    } else {
      locations <- data.frame(location_level = "", location_id_type = "", location_id = "")
    }
    if ("geographic_level" %in% names(geographies)) {
      geographic_levels <- data.frame(
        geographic_level = geographies |>
          magrittr::extract2("geographic_level")
      )
    } else {
      geographic_levels <- locations |>
        dplyr::pull("location_level")
    }
    if (nrow(geographic_levels) != nrow(locations)) {
      geographies <- dplyr::cross_join(geographic_levels, locations)
    } else {
      geographies <- dplyr::bind_cols(geographic_levels, locations)
    }
    geographies <- geographies |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ dplyr::if_else(is.na(.), "", .)
        )
      )
  } else if (is.vector(geographies) || is.character(geographies)) {
    geographies <- geographies |>
      stringr::str_split("\\|", simplify = TRUE) |>
      as.data.frame()
    if (ncol(geographies) == 1) {
      geographies <- geographies |>
        dplyr::mutate(
          location_level = "",
          location_id_type = "",
          location_id = ""
        ) |>
        dplyr::rename(geographic_level = "V1") |>
        dplyr::distinct()
    } else if (ncol(geographies) == 3) {
      geographies <- geographies |>
        dplyr::rename(location_level = "V1", location_id_type = "V2", location_id = "V3") |>
        dplyr::mutate(
          geographic_level = !!rlang::sym("location_level"),
          location_level = dplyr::case_when(
            !!rlang::sym("location_id_type") == "" &
              !!rlang::sym("location_id") == "" ~ "",
            .default = !!rlang::sym("location_level")
          )
        ) |>
        dplyr::distinct()
    } else {
      stop(
        paste(
          "Geographies should contain either",
          "geographic_levels in the format \"NAT\", \"REG\", etc or",
          "locations in the format \"NAT|code|E92000001\", \"NAT|id|dP0Zw\", etc)"
        )
      )
    }
  } else {
    stop("The geographies parameter should be given as either a data frame, vector or string.")
  }
  geographies <- geographies |> dplyr::distinct()
  return(geographies)
}
