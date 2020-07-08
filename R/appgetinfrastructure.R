#' Retrieves infrastructure from the DataMuster database
#'
#' This function retreives a list of ALMS infrastructure for a property for configuration of the DataMuster website
#' @name  appgetinfrastructure
#' @param property the name of the property to search the database
#' @param timezone the timezone of the property to display the weekly weight data
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe of infrastructure and associated information
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appgetinfrastructure <- function(property, timezone, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  property <- sprintf('"stationname":"%s",', property)
  active <- sprintf('"properties.datarecording":"%s",', "TRUE")

  currenttime <- Sys.time()
  attr(currenttime,"tzone") <- timezone

  # Set up query to search for cattle

  filter <- paste0("{", property, active,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"stationname":true, "properties.asset_id":true, "properties.type":true, "properties.filename":true, "properties.lastsignal":true, "properties.Paddock":true, "_id":false}')

  infsinfo <- infrastructure$find(query = filter, fields = lookfor)

  if (nrow(infsinfo) == 0){infsinfo <- infrastructure$find(query = '{"stationname":"xxxxxx"}', fields = lookfor)}

  infsinfo <- cbind(infsinfo[-1], infsinfo$properties)

  infsinfo <- infsinfo%>%
              mutate(lastsignal = format(as.POSIXct(lastsignal), tz = timezone),
              status = ifelse(as.numeric(difftime(currenttime, lastsignal, units = "mins")) < 60, "Active", ifelse(as.numeric(difftime(currenttime, lastsignal, units = "mins")) <= 180, "Check", "Not Active"))) %>%
              filter(asset_id != "xxxxxx")

  return(infsinfo)

}


