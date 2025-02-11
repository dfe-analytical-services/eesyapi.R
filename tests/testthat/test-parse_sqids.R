test_that("parsing geographic levels", {
  expect_equal(
    parse_geographic_level_codes(c("NAT", "REG", "LA")),
    data.frame(geographic_level = c("National", "Regional", "Local authority"))
  )

  expect_warning(
    parse_geographic_level_codes(c("NAT", "REG", "LA", "NA")),
    "The following geographic_levels were returned by your query, but are not a part of the standard data set: NA"
  )

  })
