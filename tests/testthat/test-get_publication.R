# Check the get_publication_catalogue() function returns the expected data
test_that("Retrieve publication list on each environment", {
  expect_gt(
    get_publications(ees_environment = "test") |> nrow(),
    0
  )
  expect_gt(
    get_publications(ees_environment = "dev") |> nrow(),
    0
  )
  expect_gt(
    get_publications(ees_environment = "preprod") |> nrow(),
    0
  )
  expect_gt(
    get_publications(ees_environment = "prod") |> nrow(),
    0
  )
})

# Check the get_publication_datasets() function returns the expected data
# WARNING: This depends on live data, so may fail due to real-life changes.
#          If that's the case, take a new snapshot by running seed_tests()
test_that("Retrieve data set list for publication", {
  expect_equal(
    get_data_catalogue(
      example_id("publication", ees_environment = test_env),
      ees_environment = test_env,
    ),
    readRDS("testdata/example_publication_datasets.rds")
  )
})

test_that("Search doesn't return anything it shouldn't", {
  expect_equal(
    get_publications(search = "attendance", ees_environment = test_env, ) |>
      dplyr::filter(
        !grepl("attendance", title, ignore.case = TRUE),
        !grepl("attendance", summary, ignore.case = TRUE)
      ) |>
      nrow(),
    0
  )
})


test_that("Search doesn't return anything it shouldn't", {
  result <- get_publications(search = "attendance", ees_environment = test_env)
  expect_equal(
    result |>
      dplyr::mutate(title_summary = paste(title, summary)) |>
      dplyr::filter(
        grepl("attendance", title_summary, ignore.case = TRUE)
      ) |>
      nrow(),
    nrow(result)
  )
})

test_that("Search throws an error if all search terms are less than 3 characters", {
  expect_error(
    get_publications(search = "AP", ees_environment = test_env)
  )
})

test_that("Search throws a warning if any search term is less than 3 characters", {
  expect_warning(
    get_publications(search = "api d", ees_environment = test_env)
  )
})
