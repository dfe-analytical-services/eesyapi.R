#' Example ID
#' @description
#' This function returns examples of working IDs that can be used with the eesyapi functions, such
#' as query_dataset().
#'
#' @param level Level of ID example to return. A range are available, although not every example
#' group necessarily contains all possible examples. The following are generally available.
#'   - "all": Return full list of example options for given group.
#'   - "publication": Return example publication ID
#'   - "dataset": Return example data set ID
#'   - "time_period" / "time_periods": Return example time_period(s)
#'   - "location_id" / "location_ids": Return example location ID(s)
#'   - "location_code / location_codes": Return example location code(s)
#'   - "filter": Return example filter column ID
#'   - "filter_item" / "filter_items_short" / "filter_items_long": Return example filter ID or
#'     example short / long filter query list.
#'   - "indicator": Return example indicator ID
#' @param ees_environment Environment to return a working example for: "dev", "test" or "prod"
#' @param group Choose the publication group of examples to use. Options are:
#'   - "attendance": Large example data set, careful what you ask for
#'   - "public-api-testing": Smaller example data set
#'
#' @return String, vector or list containing example ID(s) present in the API
#' @export
#'
#' @examples
#' example_id()
#' example_id("all")
#' example_id("all", ees_environment = "dev")
#' example_id("publication")
#' example_id("publication", group = "attendance")
#' example_id("time_period", group = "attendance")
#' example_id("location_ids", group = "attendance")
#' example_id("filter_items_short", group = "attendance")
#' example_id("indicator", group = "attendance")
example_id <- function(
    level = "dataset",
    ees_environment = "prod",
    group = "absence") {
  example_id_list <- list(
    attendance = list(
      dev = list(
        publication = "b6d9ed96-be68-4791-abc3-08dcaba68c04",
        dataset = "7c0e9201-c7c0-ff73-bee4-304e731ec0e6",
        time_period = "2024|W23",
        time_periods = c("2024|W21", "2024|W23"),
        location_id = "NAT|id|dP0Zw",
        location_ids = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
        location_code = "NAT|code|E92000001",
        location_codes = c("REG|code|E12000001", "REG|code|E12000002"),
        filter = "4kdUZ",
        filter_item = "5UNdi",
        filter_items_long = list(
          attendance_status = c("pmRSo", "7SdXo"),
          attendance_type = c("CvuId", "6AXrf", "0k3T5", "YdkHK"),
          education_phase = c("ThDPJ", "crH31"),
          day_number = c("uLQo4"),
          reason = c("bBrtT")
        ),
        filter_items_short = list(
          attendance_status = c("pmRSo"),
          attendance_type = c("CvuId", "6AXrf"),
          education_phase = c("ThDPJ", "crH31"),
          day_number = c("uLQo4"),
          reason = c("bBrtT")
        ),
        indicator = "bqZtT"
      ),
      test = list(
        publication = "25d0e40b-643a-4f73-3ae5-08dcf1c4d57f",
        dataset = "57b69201-033a-2c77-a19f-abcce2b11341",
        time_period = "2024|W23",
        time_periods = c("2024|W24", "2024|W25"),
        location_id = "NAT|id|mRj9K",
        location_ids = c("LA|id|arLPb", "REG|id|zecFQ"),
        location_code = "NAT|code|E92000001",
        location_codes = c("REG|code|E12000001", "REG|code|E12000002"),
        filter = "5Zdi9",
        filter_item = "rQkNj",
        filter_items_long = list(
          attendance_status = c("BfP7J", "zvUFQ"),
          attendance_type = c("TuxPJ", "tj0Em", "5Tsdi", "fzaYF"),
          education_phase = c("Poqeb", "dPE0Z"),
          day_number = c("AOhGK"),
          reason = c("9Ru4v")
        ),
        filter_items_short = list(
          attendance_status = c("qGJjG"),
          attendance_type = c("cZO31", "jgoAM"),
          education_phase = c("Poqeb", "dPE0Z"),
          day_number = c("AOhGK"),
          reason = c("9Ru4v")
        ),
        indicator = "tj0Em",
        indicators = c("tj0Em", "fzaYF")
      ),
      prod = list(
        publication = "9676af6b-d563-41f4-d071-08da8f468680",
        dataset = "63629501-d3ca-c471-9780-ec4cb6fdf172",
        time_period = "2025|W3",
        time_periods = c("2025|W3", "2025|W4"),
        location_id = "LA|id|it6Xr",
        location_ids = c("LA|id|it6Xr", "REG|id|ACyGK"),
        location_code = "NAT|code|E92000001",
        location_codes = c("REG|code|E12000001", "REG|code|E12000002"),
        filter = "z4FQE",
        filter_item = "y2daB",
        filter_items_long = list(
          attendance_status = c("e4wuS", "TmQPJ"),
          attendance_type = c("P9Aeb", "VPw5X", "uUIo4", "ls5cB"),
          education_phase = c("rbyNj", "GBMgr"),
          time_frame = c("RL5ka"),
          reason = c("S0OVx")
        ),
        filter_items_short = list(
          attendance_type = c("P9Aeb", "VPw5X"),
          education_phase = c("rbyNj", "GBMgr"),
          time_frame = c("5ezdi")
        ),
        indicator = "X9fKb",
        indicators = c("X9fKb", "cg31S")
      )
    ),
    absence = list(
      dev = list(
        publication = "d823e4df-626f-4450-9b21-08dc8b95fc02",
        dataset = "830f9201-9e11-ad75-8dcd-d2efe2834457",
        location_id = "LA|id|ml79K",
        location_code = "NAT|code|E92000001",
        location_codes = c("REG|code|E12000001", "REG|code|E12000002"),
        filter = "01tT5",
        filter_item = "wEZcb",
        indicator = "PbNeb"
      ),
      test = list(
        publication = "25d0e40b-643a-4f73-3ae5-08dcf1c4d57f",
        dataset = "e1ae9201-2fff-d376-8fa3-bd3c3660d4c8",
        location_id = "NAT|id|mRj9K",
        location_code = "NAT|code|E92000001",
        filter = "arLPb",
        filter_item = "VN5XE",
        filter_items = c("VN5XE", "PEebW"),
        indicator = "dPe0Z",
        indicators = c("OBXCL", "7YFXo")
      ),
      prod = list(
        publication = "9676af6b-d563-41f4-d071-08da8f468680",
        dataset = "55629501-e98b-0c75-adba-f95a0cfbb5e9",
        location_id = "LA|id|it6Xr",
        location_code = "NAT|code|E92000001",
        filter = "BT7J3",
        filter_item = "oUXmX",
        indicator = "uxo41",
        indicators = c("uxo41") # only one in this file
      )
    )
  )
  if (!(group %in% names(example_id_list))) {
    stop(paste0("Chosen group (", group, ") not found in examples list."))
  }
  if (!(ees_environment %in% c("dev", "test", "prod"))) {
    stop(paste0(
      "Chosen ees_environment (",
      ees_environment,
      ") should be one of: dev, test or prod."
    ))
  }

  group_examples <- example_id_list |>
    magrittr::extract2(group) |>
    magrittr::extract2(ees_environment)

  if (any(level == "all")) {
    return(group_examples)
  } else {
    if (any(!(level %in% names(group_examples)))) {
      stop(
        paste0(
          "Non-valid element level received by validate_id.\n",
          "Should be one of:\n\"",
          paste(names(group_examples), collapse = "\", \""),
          "\"."
        )
      )
    }
    return(
      if (length(level) > 1) {
        group_examples |>
          magrittr::extract(level) |>
          unlist()
      } else {
        group_examples |>
          magrittr::extract2(level)
      }
    )
  }
}

#' Example raw data
#'
#' @description
#' Download some example raw data. Mainly intended for use in developing / testing the sqid parsing
#' or as an example of getting raw data if any end users would prefer to do the sqid parsing
#' themselves.
#'
#' @inheritParams example_id
#' @param size Number of rows to return (max = 1000)
#'
#' @return Nested list form of example data from the API
#' @export
#'
#' @examples
#' example_data_raw()
example_data_raw <- function(
    group = "attendance",
    size = 32) {
  api_url(
    "get-data",
    dataset_id = example_id(group = group),
    indicators = example_id("indicator", group = group),
    page = 1, page_size = size
  ) |>
    httr::GET() |>
    httr::content("text") |>
    jsonlite::fromJSON() |>
    magrittr::use_series("results")
}

#' Create an example json query string
#' @description
#' Create an example json query string for use in examples and tests
#'
#' @param ees_environment EES environment to connect to: "test", or "prod"
#'
#' @return String containing an example json query
#' @export
#'
#' @examples
#' example_json_query() |> cat()
example_json_query <- function(ees_environment = "prod") {
  parse_tojson_params(
    indicators = example_id(
      "indicator",
      group = "attendance", ees_environment = ees_environment
    ),
    time_periods = example_id(
      "time_period",
      group = "attendance", ees_environment = ees_environment
    ),
    geographies = todf_geographies(
      example_id(
        "location_codes",
        group = "attendance", ees_environment = ees_environment
      )
    ),
    filter_items = example_id(
      "filter_items_short",
      group = "attendance", ees_environment = ees_environment
    )
  )
}

#' Create an example geography-query data frame
#'
#' @param level Query level within available options, can be one of \"nat_yorks\" or
#' \"nat_yorks_yorkslas\"
#'
#' @return Data frame containing an example geography query
#' @export
#'
#' @examples
#' example_geography_query()
example_geography_query <- function(level = "nat_yorks") {
  example_geography_queries <- list(
    nat_yorks =
      data.frame(
        geographic_level = c("NAT", "REG"),
        location_level = c("NAT", "REG"),
        location_id_type = c("code", "code"),
        location_id = c("E92000001", "E12000003")
      ),
    nat_yorks_yorkslas = data.frame(
      geographic_level = c("NAT", "REG", "LA"),
      location_level = c("NAT", "REG", "REG"),
      location_id_type = c("code", "code", "code"),
      location_id = c("E92000001", "E12000003", "E12000003")
    )
  )
  example_geography_queries |>
    magrittr::extract2(level)
}
