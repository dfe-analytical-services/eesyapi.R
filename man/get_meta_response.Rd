% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_meta.R
\name{get_meta_response}
\alias{get_meta_response}
\title{Get the base API response for a data set's meta data}
\usage{
get_meta_response(
  dataset_id,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  parse = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{dataset_id}{ID of data set to be connected to. This is required if the endpoint is one
of "get-dataset-versions", "get-summary", "get-meta", "get-csv", "get-data" or "post-data"}

\item{dataset_version}{Version of data set to be connected to}

\item{preview_token}{Preview token required for access to private data sets}

\item{ees_environment}{EES ees_environment to connect to: "dev", "test", "preprod" or "prod"}

\item{api_version}{EES API version}

\item{parse}{Parse result into structured list}

\item{verbose}{Run with additional contextual messaging. Logical, default = FALSE}
}
\value{
Results of query to API meta data endpoint
}
\description{
Get the metadata information for a data set available from the EES API.
}
\examples{
eesyapi:::get_meta_response(example_id())
}
\keyword{internal}
