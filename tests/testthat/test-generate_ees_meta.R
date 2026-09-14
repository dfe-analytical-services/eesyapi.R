test_that("Generate EES meta produces expected output", {
  attendance_meta <- example_id(group = "attendance") |> get_meta()

  # Check we've got the expected meta data columns
  expect_true(
    all(
      eesyscreener::req_meta_cols %in%
        names(attendance_meta |> generate_ees_meta())
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
