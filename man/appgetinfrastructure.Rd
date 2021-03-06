% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appgetinfrastructure.R
\name{appgetinfrastructure}
\alias{appgetinfrastructure}
\title{Retrieves infrastructure from the DataMuster database}
\usage{
appgetinfrastructure(property, timezone, username, password)
}
\arguments{
\item{property}{the name of the property to search the database}

\item{timezone}{the timezone of the property to display the weekly weight data}

\item{username}{a username to access the DataMuster database}

\item{password}{a password to access the DataMuster database}
}
\value{
a dataframe of infrastructure and associated information
}
\description{
This function retreives a list of ALMS infrastructure for a property for configuration of the DataMuster website
}
\author{
Dave Swain \email{d.swain@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}
