test_that("get-data-versions returns results", {
  expect_gt(
    get_dataset_versions(example_id(group = "attendance")) |>
      nrow(),
    0
  )
})

test_that("Check sensible error returns if no dataset_id given", {
  expect_error(
    get_dataset_versions(),
    "argument \"dataset_id\" is missing, with no default"
  )
})

test_that("get-data-versions light returns expected columns", {
  expect_equal(
    get_dataset_versions(example_id(group = "attendance")) |>
      names(),
    c(
      "version", "type", "total_rows", "release_date", "release_name",
      "time_period_start", "time_period_end"
    )
  )
})
