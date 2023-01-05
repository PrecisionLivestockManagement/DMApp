#' Retreives property information from the DataMuster database
#'
#' This function retrieves property information from the DataMuster database for configuration of the DataMuster website
#' @name appgetstations
#' @param property a list of property names associated with the user
#' @param accesslevel the users database accesslevel, either "admin" or "user"
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe showing property names and timezone
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appgetstations <- function(property, accesslevel, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  station <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

  lookfor <- sprintf('{"stationname":true, "timezone":true, "_id":false}')

  if (accesslevel == "admin"){
    filter <- sprintf('{}')}else{
    property <- paste(unlist(property), collapse = '", "' )
    filter <- sprintf('{"stationname":{"$in":["%s"]}}', "TRUE", property)}

  propertyinfo <- station$find(query = filter, fields = lookfor)

  return(propertyinfo)

}





