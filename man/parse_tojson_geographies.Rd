% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_dataset_utils.R
\name{parse_tojson_geographies}
\alias{parse_tojson_geographies}
\title{Parse geographies to json}
\usage{
parse_tojson_geographies(geographies)
}
\arguments{
\item{geographies}{String, vector or data frame containing the geographic levels and
locations to be queried.}
}
\value{
String containing json form query for geographies
}
\description{
Create a json query sub-string based on location constraints
}
\examples{
eesyapi:::parse_tojson_geographies(c("NAT", "REG")) |>
  cat()
eesyapi:::parse_tojson_geographies(c("NAT|id|dP0Zw", "REG|id|rg3Nj")) |>
  cat()
eesyapi:::parse_tojson_geographies(c("NAT|id|dP0Zw", "REG")) |>
  cat()
eesyapi:::parse_tojson_geographies(c("NAT|id|dP0Zw", "REG")) |>
  cat()
}
\keyword{internal}
