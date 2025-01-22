# Check the get_publication_catalogue() function returns the expected data
# WARNING: This depends on live data, so may fail due to real-life changes.
#          If that's the case, take a new snapshot by running seed_tests()
test_that("Retrieve publication list", {
  expect_equal(
    get_publications(),
    readRDS("testdata/example_publication_catalogue.rds")
  )
})

# Check the get_publication_datasets() function returns the expected data
# WARNING: This depends on live data, so may fail due to real-life changes.
#          If that's the case, take a new snapshot by running seed_tests()
test_that("Retrieve data set list for publication", {
  expect_equal(
    get_data_catalogue(example_id("publication")),
    readRDS("testdata/example_publication_datasets.rds")
  )
})

test_that("Search works as expected", {
  expect_equal(
    get_publications(search = "API") |>
      dplyr::filter(!grepl("API", title)) |>
      nrow(),
    0
  )
})

test_that("Search throws an error if the search term is less than 3 characters", {
  expect_error(
    get_publications(search = "AP"),
    "Search string must be 3 characters or longer."
  )
})
