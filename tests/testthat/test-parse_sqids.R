test_that("parsing geographic levels", {
  expect_equal(
    parse_geographic_level_codes(c("NAT", "REG", "LA")),
    data.frame(geographic_level = c("National", "Regional", "Local authority"))
  )

  expect_warning(
    parse_geographic_level_codes(c("NAT", "REG", "LA", "NA")),
    paste0(
      "The following geographic_levels were returned by your query, ",
      "but are not a part of the standard data set: NA"
    )
  )
})

test_that("parsing time codes", {
  expect_error(
    parse_time_codes(c("NAT", "REG", "LA", "NA"))
  )

  expect_equal(
    parse_time_codes(
      data.frame(code = c("W21", "AY", "FY"), period = c("2024", "2024/2025", "2016/2017"))
    ),
    data.frame(
      time_period = c("2024", "2024/25", "2016/17"),
      time_identifier = c("Week 21", "Academic year", "Financial year")
    )
  )
})
