#' Retreives forage information from the DataMuster database
#'
#' This function retrieves forage information from the DataMuster database for display on the DataMuster website
#' @name appgetforage
#' @param property a list of property names associated with the user
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe showing property names and timezone
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appgetforage <- function(property, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  forage <- mongo(collection = "Forage", db = "DataMuster", url = pass, verbose = T)

  lookfor <- sprintf('{"property":true, "paddock":true, "DM_ha":true, "coordinates":true, "_id":false}')

  property <- paste(unlist(property), collapse = '", "' )
  filter <- sprintf('{"property":{"$in":["%s"]}}', property)

  forageinfo <- forage$find(query = filter, fields = lookfor)

  if (nrow(forageinfo) == 0){forageinfo <- forage$find(query = '{"property":"xxxxxx"}', fields = lookfor)}

  forageinfo <- forageinfo%>%
                filter(property != "xxxxxx")%>%
                mutate(long = as.numeric(unlist(lapply(forageinfo$coordinates, `[[`, 1))),
                lat = as.numeric(unlist(lapply(forageinfo$coordinates, `[[`, 2))))%>%
                select(-coordinates)

  return(forageinfo)

}






