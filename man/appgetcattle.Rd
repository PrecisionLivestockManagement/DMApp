% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appgetcattle.R
\name{appgetcattle}
\alias{appgetcattle}
\title{Retrieves cattle information from the DataMuster database}
\usage{
appgetcattle(
  property,
  sex,
  category,
  paddockselect,
  paddock,
  weightselect,
  minwt,
  maxwt,
  username = NULL,
  password = NULL
)
}
\arguments{
\item{property}{the name of the property to search the database}

\item{username}{a username to access the DataMuster database, contact Lauren O'Connor for database access}

\item{password}{a password to access the DataMuster database}
}
\value{
a spatialpointsdataframe with a list of the RFID numbers and a number of other data points
}
\description{
This function allows cattle information to be retreived from the DataMuster database via the DataMuster website app
}
\author{
Dave Swain \email{d.swain@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}