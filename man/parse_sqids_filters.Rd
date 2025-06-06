% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_sqids.R
\name{parse_sqids_filters}
\alias{parse_sqids_filters}
\title{Parse IDs in a set of filters}
\usage{
parse_sqids_filters(filters, meta, verbose = FALSE)
}
\arguments{
\item{filters}{A set of filter item columns as taken from a data set downloaded from the API}

\item{meta}{Meta data for the data set as provided by \code{get_meta()}}

\item{verbose}{Run in verbose mode with debugging messages}
}
\value{
Data frame
}
\description{
The API uses unique IDs (sqids) to identify each filter column and its contents (filter items).
This function parses those into the data creators' id and item labels based on the meta data
stored on the API for the data set.
}
\examples{
example_data_raw() |>
  magrittr::use_series("filters") |>
  eesyapi:::parse_sqids_filters(get_meta(example_id(group = "attendance")))
}
\keyword{internal}
