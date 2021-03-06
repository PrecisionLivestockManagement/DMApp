% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appupdatedraftdirection.R
\name{appupdatedraftdirection}
\alias{appupdatedraftdirection}
\title{Change auto draft direction for cattle in the DataMuster database}
\usage{
appupdatedraftdirection(
  RFID,
  MTag,
  property,
  draftdirection,
  username,
  password
)
}
\arguments{
\item{RFID}{a list of cattle RFID numbers}

\item{property}{the name of the property that cattle belong}

\item{draftdirection}{the name of the property that cattle belong}

\item{username}{a username to access the DataMuster database}

\item{password}{a password to access the DataMuster database}
}
\value{
a message that indicates whether or not the information has been successfully recorded
}
\description{
This function allowsthe user to change the auto draft  direction for individuals or groups of cattle in the DataMuster database via the DataMuster website
}
\author{
Dave Swain \email{d.swain@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}
