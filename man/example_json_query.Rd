% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examples.R
\name{example_json_query}
\alias{example_json_query}
\title{Create an example json query string}
\usage{
example_json_query(ees_environment = "prod")
}
\arguments{
\item{ees_environment}{EES environment to connect to: "test", or "prod"}
}
\value{
String containing an example json query
}
\description{
Create an example json query string for use in examples and tests
}
\examples{
example_json_query() |> cat()
}
