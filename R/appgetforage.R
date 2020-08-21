#' Retreives property information from the DataMuster database
#'
#' This function retrieves property information from the DataMuster database for configuration of the DataMuster website
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

  for(i in 1:nrow(forageinfo)){

    forageinfo$long[i] <- forageinfo$coordinates[[i]][1]
    forageinfo$lat[i] <- forageinfo$coordinates[[i]][2]

    }

  forageinfo <- forageinfo%>%
                filter(property != "xxxxxx")%>%
                select(-coordinates)%>%
                mutate(forcategory = cut(DM_ha, breaks = c(-Inf, 1000, 2000, 3000, 4000, 5000, Inf), labels = c("< 1000", "1000 - 2000", "2001 - 3000", "3001 - 4000", "4001 - 5000", "> 5000")))

  return(forageinfo)

}





