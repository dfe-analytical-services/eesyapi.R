# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look-up of equivalent human to API geography_level names
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create a vector of ble time and geography column names
geog_level_lookup <- data.frame(
  api_friendly = c(
    "EDA", "INST", "LA", "LAD",
    "LEP", "LSIP",
    "MAT", "MCA",
    "NAT", "OA",
    "PA", "PCON", "PROV",
    "REG", "RSC", "SCH", "SPON", "WARD"
  ),
  human_friendly = c(
    "English devolved area", "Institution", "Local authority", "Local authority district",
    "Local enterprise partnership", "Local skills improvement plan area",
    "Multi-academy trust", "MCA",
    "National", "Opportunity area",
    "Planning area", "Parliamentary constituency", "Provider",
    "Regional", "Regional school commissioner region", "School", "Sponsor", "Ward"
  )
)
# write it out to the data folder

usethis::use_data(geog_level_lookup, overwrite = TRUE)
