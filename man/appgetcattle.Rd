% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appgetcattle.R
\name{appgetcattle}
\alias{appgetcattle}
\title{Retrieves cattle from the DataMuster database}
\usage{
appgetcattle(property, sex, category, paddock, zoom, username, password)
}
\arguments{
\item{property}{the name of the property to search the database}

\item{sex}{the sex of the cattle to be returned, determined by the "Males or Females" filter}

\item{category}{the category of cattle to be returned, determined by the "Breeders or Growers" filter}

\item{paddock}{the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map}

\item{zoom}{indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter}

\item{username}{a username to access the DataMuster database}

\item{password}{a password to access the DataMuster database}

\item{id}{identification choice to be displayed}
}
\value{
a dataframe with a list of cattle numbers by paddock
}
\description{
This function retreives a list of current cattle RFIDs that are on an ALMS unit for display on the DataMuster website
}
\author{
Dave Swain \email{d.swain@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}
