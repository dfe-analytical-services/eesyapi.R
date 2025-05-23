% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation_rules.R
\name{validate_time_periods}
\alias{validate_time_periods}
\title{Validate time periods}
\usage{
validate_time_periods(time_periods)
}
\arguments{
\item{time_periods}{Time periods required as a string ("period|code") or vector of strings}
}
\description{
Validate time periods
}
\examples{
eesyapi:::validate_time_periods(c("2023|AY", "2024|AY"))
}
\keyword{internal}
