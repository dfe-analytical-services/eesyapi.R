test_that("Get publications returns warning when no rows returned", {
  expect_warning(
    x <- get_publications(search = "z3x4c5v6b7"),
    "Your query returned zero rows."
  )
})

test_that("Get nice error when no rows are returned", {
  expect_error(
    query_dataset(
      example_id(group = "attendance"),
      ees_environment = test_env,
      indicators = example_id("indicator", group = "attendance"),
      time_periods = eesyapi::example_id("time_period", group = "attendance"),
      geographies = c("SCH"),
      filter_items = eesyapi::example_id("filter_item", group = "attendance")
    ),
    "No rows were returned for your query. Set verbose = TRUE to see detailed API response."
  )
})
