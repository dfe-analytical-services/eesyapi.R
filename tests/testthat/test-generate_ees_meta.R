test_that("Generate EES meta produces expected output", {
  attendance_meta <- example_id(group = "attendance") |> get_meta()

  # Check we've got the expected meta data columns
  expect_equal(
    names(attendance_meta |> generate_ees_meta()),
    c(
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
  )

  # Check col_type only contains "Filter" and "Indicator" entries
  expect_equal(
    attendance_meta |> generate_ees_meta() |> dplyr::pull("col_type") |> unique(),
    c(
      "Filter",
      "Indicator"
    )
  )
})
