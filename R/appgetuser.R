#' Retrieves user information from the DataMuster database
#'
#' This function retreives user information from the DataMuster database for configuration of the DataMuster website
#' @name appgetuser
#' @param email the email address of the registered user
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe showing the users database accesslevel, writeaccess and list of stations
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appgetuser <- function(email, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  users <- mongo(collection = "Users", db = "DataMuster", url = pass, verbose = T)

  lookfor <- sprintf('{"loginemail":true, "accesslevel":true, "writeaccess":true, "stations":true, "_id":false}')
  filter <- sprintf('{"loginemail":"%s"}', email)

  userinfo <- users$find(query = filter, fields = lookfor)

  if(nrow(userinfo) != 0){
  userinfo <- userinfo%>%
              select(loginemail, accesslevel, writeaccess, stations)}

  return(userinfo)

}
