#' Retreives carrying capacity information from the DataMuster database
#'
#' This function retrieves property information from the DataMuster database for configuration of the DataMuster website
#' @name appgetcarryingcapacity
#' @param property a list of property names associated with the user
#' @param period the time period for the data required, either "12months" or "seasonal"
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe showing paddock names and carrying capacity information
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appgetcarryingcapacity <- function(property, period, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  carryingcapacity12months <- mongo(collection = "CarryingCapacity12months", db = "DMApp", url = pass, verbose = T)
  carryingcapacityseasonal <- mongo(collection = "CarryingCapacitySeasonal", db = "DMApp", url = pass, verbose = T)

  stationname <- sprintf('"Property":"%s",', property)

  # Set up query to search for cattle

  filter <- paste0("{", stationname,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"_id":false}')

  if(period == "12months"){info <- carryingcapacity12months$find(query = filter, fields = lookfor)}
  if(period == "seasonal"){info <- carryingcapacityseasonal$find(query = filter, fields = lookfor)}

  return(info)

}





