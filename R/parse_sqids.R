#' Parse API time codes
#'
#' @description
#' The API returns some abbreviated versions of time periods in it's base
#' output. This function converts those to more human-readable versions.
#'
#' @param time_periods data frame of API returned time periods and indicators
#' @inheritParams parse_sqids_filters
#'
#' @returns Data frame of expanded time codes
#'
#' @keywords internal
#'
#' @examples
#' data.frame(
#'   code = c("W1", "W12", "Academic year"),
#'   period = c("2024", "2025", "202425")
#' ) |>
#'   eesyapi:::parse_time_codes()
parse_time_codes <- function(time_periods, verbose = FALSE) {
  if (!is.data.frame(time_periods)) {
    stop(
      "time_periods should be a data frame, but has been provided as a ",
      typeof(time_periods)
    )
  }
  unique_identifiers <- time_periods |>
    dplyr::pull("code") |>
    unique()
  time_periods_out <- time_periods |>
    dplyr::select(
      time_period = "period",
      time_identifier = "code"
    )
  if (any(grepl("W([0-9]{1-2})", unique_identifiers))) {
    time_periods_out <- time_periods_out |>
      dplyr::mutate(
        time_identifier = stringr::str_replace(
          !!rlang::sym("time_identifier"),
          "W([0-9]+)",
          "Week \\1"
        )
      )
  }
  if (any(grepl("([ACF])Y", unique_identifiers))) {
    time_periods_out <- time_periods_out |>
      dplyr::mutate(
        time_identifier = dplyr::case_when(
          time_identifier == "AY" ~ "Academic year",
          time_identifier == "FY" ~ "Financial year",
          time_identifier == "CY" ~ "Calendar year",
          .default = !!rlang::sym("time_identifier")
        ),
        time_period = stringr::str_replace(
          !!rlang::sym("time_period"),
          "([0-9]+)/20([0-9]+)",
          "\\1/\\2"
        )
      )
  }
  return(time_periods_out)
}

#' Parse API geographic_levels
#'
#' @description
#' The API generally returns abbreviated and acronym versions of geographic_levels in it's base
#' output. This function converts those to more hum-readable versions.
#'
#' @param geographic_levels Vector of API returned geographic_levels
#' @inheritParams parse_sqids_filters
#'
#' @returns Data frame of expanded geographic levels
#'
#' @keywords internal
#'
#' @examples
#' c("NAT", "NAT", "REG", "LA", "LAD") |>
#'   eesyapi:::parse_geographic_level_codes()
parse_geographic_level_codes <- function(geographic_levels, verbose = FALSE) {
  if (any(c(is.list(geographic_levels), is.data.frame(geographic_levels)))) {
    stop(
      "geographic_levels should be a vector, but has been provided as a ",
      typeof(geographic_levels)
    )
  }
  api_glevels <- eesyapi::geog_level_lookup |>
    magrittr::extract2("api_friendly")
  if (!all(unique(geographic_levels) %in% api_glevels)) {
    warning(
      "The following geographic_levels were returned by your query, ",
      "but are not a part of the standard data set: ",
      paste0(setdiff(geographic_levels, api_glevels), collapse = ",")
    )
  }
  data.frame(api_friendly = geographic_levels) |>
    dplyr::left_join(
      eesyapi::geog_level_lookup |>
        dplyr::rename(geographic_level = "human_friendly"),
      by = dplyr::join_by("api_friendly")
    ) |>
    dplyr::mutate(
      geographic_level = dplyr::case_when(
        is.na(geographic_level) ~ !!rlang::sym("api_friendly"),
        .default = !!rlang::sym("geographic_level")
      )
    ) |>
    dplyr::select("geographic_level")
}

#' Parse location sqids
#'
#' @description
#' The API uses unique IDs (sqids) to identify each location in a data set. This function parses
#' those into the corresponding location codes and names based on the meta data stored on the API
#' for the data set.
#'
#' @inheritParams parse_sqids_filters
#' @param locations A set of location columns as taken from a data set downloaded from the API
#'
#' @return Data frame of parsed geography information
#'
#' @keywords internal
#'
#' @examples
#' example_data_raw() |>
#'   magrittr::use_series("locations") |>
#'   eesyapi:::parse_sqids_locations(get_meta(example_id(group = "attendance")))
parse_sqids_locations <- function(locations, meta, verbose = FALSE) {
  lookup <- meta |>
    magrittr::use_series("locations") |>
    dplyr::filter(
      !!rlang::sym("geographic_level_code") %in% names(locations)
    ) |>
    dplyr::rename(name = "label")
  for (level in names(locations)) {
    locations <- locations |>
      dplyr::rename("item_id" = !!rlang::sym(level)) |>
      dplyr::left_join(
        lookup |>
          dplyr::filter(!!rlang::sym("geographic_level_code") == level) |>
          dplyr::select(
            -dplyr::all_of(c("geographic_level_code", "geographic_level"))
          ) |>
          dplyr::rename_with(
            ~ paste0(tolower(level), "_", .x),
            !dplyr::matches("item_id")
          ),
        by = dplyr::join_by("item_id")
      ) |>
      dplyr::select(-"item_id")
  }
  return(
    locations |>
      dplyr::select(
        dplyr::where(
          ~ !all(is.na(.x))
        )
      )
  )
}

#' Parse IDs in a set of filters
#'
#' @description
#' The API uses unique IDs (sqids) to identify each filter column and its contents (filter items).
#' This function parses those into the data creators' id and item labels based on the meta data
#' stored on the API for the data set.
#'
#' @param filters A set of filter item columns as taken from a data set downloaded from the API
#' @param meta Meta data for the data set as provided by `get_meta()`
#' @param verbose Run in verbose mode with debugging messages
#'
#' @return Data frame
#'
#' @keywords internal
#'
#' @examples
#' example_data_raw() |>
#'   magrittr::use_series("filters") |>
#'   eesyapi:::parse_sqids_filters(get_meta(example_id(group = "attendance")))
parse_sqids_filters <- function(filters, meta, verbose = FALSE) {
  filter_ids <- meta |>
    magrittr::use_series("filter_columns") |>
    dplyr::filter(!!rlang::sym("col_id") %in% colnames(filters)) |>
    dplyr::pull("col_id")
  data_filter_ids <- filters |> names()
  if (any(!(data_filter_ids %in% filter_ids))) {
    warning(
      "The following filter IDs were not found in the associated meta data: ",
      paste(setdiff(data_filter_ids, filter_ids), collapse = ", ")
    )
  }
  if (verbose) {
    print(filter_ids)
  }
  for (column_sqid in filter_ids) {
    col_name <- meta |>
      magrittr::use_series("filter_columns") |>
      dplyr::filter(!!rlang::sym("col_id") == column_sqid) |>
      dplyr::pull("col_name")
    if (verbose) {
      message("Matched ", column_sqid, " to ", col_name)
    }
    lookup <- meta |>
      magrittr::use_series("filter_items") |>
      dplyr::filter(!!rlang::sym("col_id") == column_sqid) |>
      dplyr::select("item_label", "item_id") |>
      dplyr::rename(
        !!rlang::sym(col_name) := "item_label",
        !!rlang::sym(column_sqid) := "item_id"
      )
    filters <- filters |>
      dplyr::left_join(lookup, by = column_sqid) |>
      dplyr::select(-dplyr::all_of(column_sqid))
  }
  return(filters)
}

#' Parse IDs in a set of indicators
#'
#' @description
#' The API uses unique IDs (sqids) to identify each indicator column. This function parses those
#' into the data creators' column names based on the meta data stored on the API for the data set.
#'
#' @inheritParams parse_sqids_filters
#' @param indicators A set of indicator columns as taken from a data set downloaded from the API
#'
#' @return Data frame
#'
#' @keywords internal
#'
#' @examples
#' example_data_raw(group = "attendance") |>
#'   magrittr::use_series("values") |>
#'   eesyapi:::parse_sqids_indicators(get_meta(example_id(group = "attendance")))
parse_sqids_indicators <- function(indicators, meta, verbose = FALSE) {
  indicator_ids <- meta |>
    magrittr::use_series("indicators") |>
    dplyr::filter(!!rlang::sym("col_id") %in% colnames(indicators))
  indicator_lookup <- indicator_ids |>
    dplyr::pull("col_id")
  names(indicator_lookup) <- indicator_ids |> dplyr::pull("col_name")
  indicators <- indicators |>
    dplyr::rename(dplyr::all_of(indicator_lookup))
  return(indicators)
}
