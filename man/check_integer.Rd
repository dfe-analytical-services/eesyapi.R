% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_integer}
\alias{check_integer}
\title{Check if a value is an integer}
\usage{
check_integer(x)
}
\arguments{
\item{x}{a value to test}
}
\value{
logical, false if not an integer, true if an integer
}
\description{
is.integer checks the object class, not the value, so credit to VitoshKa
on stack overflow for the core of this function...
}
\details{
https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer

looks like it's been adopted in installr too, avoiding needing that as a
dependency by putting the code we need here.
}
\keyword{internal}
