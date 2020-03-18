#' Retrieves user settings from the DataMuster database
#'
#' This function retreives user website settings from the DataMuster database for configuration of the DataMuster website
#' @name appgetusersettings
#' @param email the email address of the registered user
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe showing the users last used language and property
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


appgetusersettings <- function(email, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  usersettings <- mongo(collection = "UserSettings", db = "DataMuster", url = pass, verbose = T)

  lookfor <- sprintf('{"User":true, "Language":true, "LastProperty":true, "_id":false}')
  filter <- sprintf('{"User":"%s"}', email)

  userinfo <- usersettings$find(query = filter, fields = lookfor)

  return(userinfo)

}
