test_that("Invalid method selected", {
  expect_error(
    query_dataset(
      example_id(ees_environment = test_env),
      indicators = example_id("indicator", ees_environment = test_env),
      ees_environment = test_env,
      method = "QUERY"
    )
  )
})

test_that("Invalid dataset_id", {
  expect_error(
    query_dataset(
      "kjdhf873kdjf",
      indicators = example_id("indicator"),
      ees_environment = test_env
    )
  )
})

test_that("No indicator supplied", {
  expect_warning(
    query_dataset(
      example_id(ees_environment = test_env),
      ees_environment = test_env
    ),
    "No indicators provided, defaulted to using all indicators from meta data"
  )
})

test_that("Run query from file", {
  query_result <- query_dataset(
    example_id(group = "attendance", ees_environment = test_env),
    ees_environment = test_env,
    json_query = "testdata/test_query.json"
  )
  query_result <- query_result |>
    dplyr::arrange(dplyr::across(colnames(query_result)))
  expect_equal(
    query_result,
    readRDS("testdata/example_json-from-file_dataset.rds") |>
      dplyr::select(dplyr::all_of(colnames(query_result))) |>
      dplyr::arrange(dplyr::across(colnames(query_result)))
  )
})

test_that("Run query from string", {
  query_result <- query_dataset(
    example_id(group = "attendance", ees_environment = test_env),
    ees_environment = test_env,
    json_query = example_json_query(ees_environment = test_env)
  )
  query_result <- query_result |>
    dplyr::arrange(dplyr::across(colnames(query_result)))
  expect_equal(
    query_result,
    readRDS("testdata/example_json-from-string_dataset.rds") |>
      dplyr::select(dplyr::all_of(colnames(query_result))) |>
      dplyr::arrange(dplyr::across(colnames(query_result)))
  )
})

test_that("Time period query returns expected time periods", {
  expect_equal(
    post_dataset(
      example_id(group = "attendance", ees_environment = test_env),
      indicators = example_id(
        "indicator",
        group = "attendance",
        ees_environment = test_env
      ),
      time_periods = eesyapi::example_id(
        "time_periods",
        group = "attendance",
        ees_environment = test_env
      ),
      geographies = eesyapi::example_id(
        "location_ids",
        group = "attendance",
        ees_environment = test_env
      ),
      filter_items = eesyapi::example_id(
        "filter_item",
        group = "attendance",
        ees_environment = test_env
      ),
      ees_environment = test_env
    ) |>
      dplyr::select("time_period", "time_identifier") |>
      dplyr::distinct() |>
      dplyr::arrange(time_period, time_identifier),
    data.frame(
      time_period = c("2024", "2024"),
      time_identifier = c("Week 24", "Week 25")
    )
  )
})

test_that("Time period query errors on badly formatted time period", {
  expect_error(
    post_dataset(
      example_id(group = "attendance", ees_environment = test_env),
      indicators = example_id(
        "indicator",
        group = "attendance",
        ees_environment = test_env
      ),
      time_periods = c("2024W21", "2024|W23"),
      geographies = eesyapi::example_id(
        "location_ids",
        group = "attendance", ees_environment = test_env
      ),
      filter_items = eesyapi::example_id(
        "filter_item",
        group = "attendance", ees_environment = test_env
      ),
      ees_environment = test_env
    )
  )
})

test_that("Geography query works with NAT", {
  expect_equal(
    query_dataset(
      example_id(group = "attendance", ees_environment = test_env),
      indicators = example_id(
        "indicator",
        group = "attendance", ees_environment = test_env
      ),
      time_periods = eesyapi::example_id(
        "time_period",
        group = "attendance", ees_environment = test_env
      ),
      geographies = "NAT",
      filter_items = eesyapi::example_id(
        "filter_item",
        group = "attendance", ees_environment = test_env
      ),
      ees_environment = test_env
    ) |>
      dplyr::select("geographic_level") |>
      dplyr::distinct(),
    data.frame(
      geographic_level = c("National")
    )
  )
})

test_that("Geography query works with National", {
  expect_equal(
    query_dataset(
      example_id(group = "attendance", ees_environment = test_env),
      indicators = example_id(
        "indicator",
        group = "attendance", ees_environment = test_env
      ),
      time_periods = eesyapi::example_id(
        "time_period",
        group = "attendance", ees_environment = test_env
      ),
      geographies = "National",
      filter_items = eesyapi::example_id(
        "filter_item",
        group = "attendance", ees_environment = test_env
      ),
      ees_environment = test_env
    ) |>
      dplyr::select("geographic_level") |>
      dplyr::distinct(),
    data.frame(
      geographic_level = c("National")
    )
  )
})


test_that("Geography query returns expected geographies", {
  expect_equal(
    query_dataset(
      example_id(group = "attendance", ees_environment = test_env),
      indicators = example_id(
        "indicator",
        group = "attendance", ees_environment = test_env
      ),
      time_periods = eesyapi::example_id(
        "time_period",
        group = "attendance", ees_environment = test_env
      ),
      geographies = eesyapi::example_id(
        "location_ids",
        group = "attendance", ees_environment = test_env
      ),
      filter_items = eesyapi::example_id(
        "filter_item",
        group = "attendance", ees_environment = test_env
      ),
      ees_environment = test_env
    ) |>
      dplyr::select("geographic_level", "nat_code", "reg_code", "la_code") |>
      dplyr::distinct() |>
      dplyr::arrange(geographic_level),
    data.frame(
      geographic_level = c("Local authority", "Regional"),
      nat_code = rep("E92000001", 2),
      reg_code = rep("E12000003", 2),
      la_code = c("E06000014", NA)
    )
  )
})

test_that("Non-standard geographic level", {
  expect_error(
    query_dataset(
      example_id(ees_environment = test_env),
      geographies = "Nat",
      indicators = example_id("indicator", ees_environment = test_env),
      ees_environment = test_env
    ),
    paste0(
      "\nHTTP connection error: 400\nMust be one of the allowed values.",
      "\n     Provided values: Nat\n     Allowed values: ",
      "EDA, INST, LA, LAD, LEP, LSIP, MAT, MCA, NAT, OA, PA, PCON, PROV, REG, RSC, SCH, SPON, WARD"
    )
  )
})

test_that("Test filter-combinations POST dataset query", {
  query_result <- query_dataset(
    example_id(group = "attendance", ees_environment = test_env),
    indicators = example_id(
      "indicator",
      group = "attendance", ees_environment = test_env
    ),
    time_periods = eesyapi::example_id(
      "time_period",
      group = "attendance", ees_environment = test_env
    ),
    geographies = eesyapi::example_id(
      "location_ids",
      group = "attendance", ees_environment = test_env
    ),
    filter_items = eesyapi::example_id(
      "filter_items_long",
      group = "attendance", ees_environment = test_env
    ),
    ees_environment = test_env
  )
  query_result <- query_result |>
    dplyr::arrange(dplyr::across(colnames(query_result)))
  expect_equal(
    query_result,
    readRDS("testdata/example_post_dataset.rds") |>
      dplyr::select(dplyr::all_of(colnames(query_result))) |>
      dplyr::arrange(dplyr::across(colnames(query_result)))
  )
  query_result <- query_dataset(
    example_id(group = "attendance", ees_environment = test_env),
    indicators = eesyapi::example_id(
      "indicator",
      group = "attendance", ees_environment = test_env
    ),
    time_periods = eesyapi::example_id(
      "time_period",
      group = "attendance", ees_environment = test_env
    ),
    geographies = eesyapi::example_id(
      "location_ids",
      group = "attendance", ees_environment = test_env
    ),
    filter_items = eesyapi::example_id(
      "filter_items_short",
      group = "attendance", ees_environment = test_env
    ),
    ees_environment = test_env
  ) |>
    dplyr::select(
      "attendance_status",
      "attendance_type",
      "day_number",
      "education_phase",
      "attendance_reason"
    ) |>
    dplyr::distinct()
  expect_equal(
    query_result,
    data.frame(
      attendance_status = rep("Absence", 4),
      attendance_type = rep(c("Authorised", "Unauthorised"), 2),
      day_number = rep("Total", 4),
      education_phase = c(rep("Secondary", 2), rep("Total", 2)),
      attendance_reason = rep("Total", 4)
    )
  )
})

test_that("Indicators not found in data set", {
  expect_error(
    query_dataset(
      example_id(ees_environment = test_env),
      indicators = c("uywet", "uywed"),
      ees_environment = test_env
    ),
    paste0(
      "\nHTTP connection error: 400\nOne or more indicators could not be found.",
      "\n     Error items: uywet, uywed"
    )
  )
})

test_that("Query data set runs on dev!", {
  expect_equal(
    query_dataset(
      example_id(group = "attendance", ees_environment = "dev"),
      indicators = example_id("indicator", group = "attendance", ees_environment = "dev"),
      time_periods = example_id("time_periods", group = "attendance", ees_environment = "dev"),
      geographies = example_id("location_codes", group = "attendance", ees_environment = "dev"),
      filter_items = example_id(
        "filter_items_short",
        group = "attendance",
        ees_environment = "dev"
      ),
      page = 1,
      page_size = 12,
      ees_environment = "dev"
    ) |> nrow(),
    12
  )
})

test_that("Query data set runs on test!", {
  expect_equal(
    query_dataset(
      example_id(group = "attendance", ees_environment = "test"),
      indicators = example_id("indicator", group = "attendance", ees_environment = "test"),
      time_periods = example_id("time_periods", group = "attendance", ees_environment = "test"),
      geographies = example_id("location_codes", group = "attendance", ees_environment = "test"),
      filter_items = example_id(
        "filter_items_short",
        group = "attendance",
        ees_environment = "test"
      ),
      page = 1,
      page_size = 12,
      ees_environment = "test"
    ) |> nrow(),
    12
  )
})
