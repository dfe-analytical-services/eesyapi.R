# Create geographic_level API shorthand to natural language look-up
#
# Script for updating the package data file geog_level_lookup.rda with the look-up between API
# shorthands and natural language versions of geographic levels.
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

usethis::use_data(geog_level_lookup, overwrite = TRUE)
