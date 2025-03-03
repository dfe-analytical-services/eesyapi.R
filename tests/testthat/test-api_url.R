test_that("api_url", {
  # Set the default environment for the tests
  test_env <- default_ees_environment()
  if(test_env %in% c("test", "dev", "preprod")){
    expected_base <- paste0("https://pp-api.education.gov.uk/statistics-",test_env,"/")

  } else {
    expected_base <- "https://api.education.gov.uk/statistics/"

  }
  expect_equal(
    api_url(),
    paste0(expected_base, "v",
      default_api_version(),
      "/publications?")
  )
  expect_error(
    api_url(api_version = "1.x")
  )
  expect_error(
    api_url(
      endpoint = "query",
      dataset_id = example_id()
    )
  )
  expect_error(
    api_url(endpoint = "post-data")
  )
  expect_equal(
    api_url(
      endpoint = "post-data",
      dataset_id = example_id()
    ),
    paste0(
      expected_base,
      "v",
      default_api_version(),
      "/data-sets/",
      example_id(ees_environment = test_env),
      "/query"
    )
  )
  expect_error(
    api_url(
      endpoint = "post-data",
      dataset_id = example_id(),
      dataset_version = "x"
    )
  )
  expect_equal(
    api_url(
      endpoint = "post-data",
      dataset_id = example_id(),
      dataset_version = 2.1
    ),
    paste0(
      expected_base,
      "v",
      default_api_version(),
      "/data-sets/",
      example_id(ees_environment = test_env),
      "/query?dataSetVersion=2.1"
    )
  )
  expect_error(
    api_url(
      ees_environment = "invalid-endpoint"
    ),
    paste(
      "You have entered invalid EES environment. The environment should be one of:\n",
      "   - dev, test, preprod or prod"
    )
  )

  expect_error(
    api_url("get-csv"),
    "The variable dataset_id is NULL, please provide a valid dataset_id."
  )

  expect_warning(
    api_url(
      "get-csv",
      dataset_id = example_id(
        "dataset"
      ),
      indicators = "qwerty"
    )
  )

  expect_equal(
    api_url("get-csv", dataset_id = example_id("dataset")),
    paste0(
      expected_base,
      "v",
      default_api_version(),
      "/data-sets/",
      example_id("dataset", ees_environment = test_env),
      "/csv"
    )
  )
})
