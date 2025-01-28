test_that("Get publications returns warning when no rows returned", {
  expect_warning(
    x <- get_publications(search = "z3x4c5v6b7"),
    "Your query returned zero rows."
  )
})
