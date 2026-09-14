#' Generate EES meta
#'
#' This function generates a basic meta data frame approximating what analysts upload alongside
#' data files to the platform
#' @param api_meta
#'
#' @returns data frame
#'
#' @export
#' @examples
#' get_meta(example_id()) |> generate_ees_meta()
generate_ees_meta <- function(api_meta) {
  dplyr::bind_rows(
    data.frame(
      col_name = api_meta$filter_columns$col_name,
      label = api_meta$filter_columns$label
    ) |>
      dplyr::mutate(
        col_type = "Filter",
        filter_default = "",
      ),
    data.frame(
      col_name = api_meta$indicators$col_name,
      label = api_meta$indicators$label
    ) |>
      dplyr::mutate(
        col_type = "Indicator",
        indicator_unit = dplyr::case_when(
          grepl("percent", col_name) ~ "%",
          .default = ""
        ),
        indicator_dp = dplyr::case_when(
          grepl("count", col_name) ~ "0",
          grepl("percent", col_name) ~ "1",
          .default = ""
        )
      )
  ) |>
    dplyr::mutate(
      filter_hint = "",
      indicator_grouping = "",
      filter_grouping_column = "",
      filter_hint = "",
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(.), "", .)
      )
    ) |>
    dplyr::select(
      "col_name",
      "col_type",
      "label",
      "indicator_grouping",
      "indicator_unit",
      "indicator_dp",
      "filter_hint",
      "filter_grouping_column",
      "filter_default"
    )
}
