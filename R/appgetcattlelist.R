#' Retrieves cattle from the DataMuster database
#'
#' This function retreives a list of cattle and their paddock allocations for display on the DataMuster website
#' @name appgetcattlelist
#' @param property the name of the property to search the database
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param paddock the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle numbers by paddock
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


appgetcattlelist <- function(property, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattlelist <- mongo(collection = "Cattlelist", db = "DMApp", url = pass, verbose = T)

  property <- sprintf('"Property":"%s",', property)
  #if(sex == "all"){sex <- NULL} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  #if(category == "all"){category <- NULL} else {category <- sprintf('"properties.category":"%s",', category)}
  #if(is.null(paddock)||zoom == 1){paddock <- NULL}else{paddock <- sprintf('"Paddock":"%s",', paddock)}

  # Set up query to search for cattle

  filter <- paste0("{", property, "}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"Paddock":true, "category":true, "cattle":true, "AE":true, "avweight":true, "lastdate":true, "_id":false}')

  cattleinfo <- cattlelist$find(query = filter, fields = lookfor)

  #if(nrow(cattleinfo) == 0){
  #  cattleinfo <- cattlelist$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = lookfor)}

  return(cattleinfo)

}
