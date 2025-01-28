test_that("Get publications returns warning when no rows returned", {
  expect_warning(
    x <- get_publications(search = "dashboardy"),
    "Your query returned zero rows.")
})
