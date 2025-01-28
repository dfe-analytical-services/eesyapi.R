test_that("get-data-versions returns results", {
  expect_gt(
    get_data_versions(example_id(group="attendance")) |>
      nrow(),
    0)
})

test_that("Check sensible error returns if no dataset_id given", {
  expect_error(
    get_data_versions(),
    "argument \"dataset_id\" is missing, with no default"
    )
})
