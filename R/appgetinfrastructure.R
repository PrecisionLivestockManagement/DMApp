#' Retrieves infrastructure from the DataMuster database
#'
#' This function provides a list of ALMS infrastructure for a property
#' @name  appgetinfrastructure
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe of infrastructure and associated information
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appgetinfrastructure <- function(property, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  property <- sprintf('"stationname":"%s",', property)
  active <- sprintf('"properties.datarecording":"%s",', "TRUE")
  type <- sprintf('"properties.type":"%s",', "Walk-over-Weighing Unit")

  # Set up query to search for cattle

  filter <- paste0("{", property, active, type,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"stationname":true, "properties.asset_id":true, "properties.filename":true, "_id":false}')

  infsinfo <- infrastructure$find(query = filter, fields = lookfor)

  if (nrow(infsinfo) == 0){infsinfo <- infrastructure$find(query = '{"stationname":"xxxxxx"}', fields = lookfor)}

  infsinfo <- cbind(infsinfo[-1], infsinfo$properties)

  return(infsinfo)

}


