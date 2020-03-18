#' Updates user settings in the DataMuster database
#'
#' This function allows user settings to be updated from the DataMuster database via the DataMuster website
#' @name appupdateusersettings
#' @param email the email address of the registered user
#' @param language the last language selected by the user, detemined by the selected "Language Options:" button
#' @param lastprop the last property selected by the user, detemined by the selected "Property Choice:" dropdown list
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


appupdateusersettings <- function(email, language, lastprop, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  usersettings <- mongo(collection = "UserSettings", db = "DataMuster", url = pass, verbose = T)

  usersinfo <- sprintf('{"User":"%s"}', email)
  datainfo <- sprintf('{"$set":{"Language":"%s", "LastProperty":"%s", "createdAt":{"$date":"%s"}}}', language, lastprop, paste0(substr(Sys.time(),1,10),"T",substr(Sys.time(),12,19),"+1000"))

  usersettings$update(usersinfo, datainfo, upsert = TRUE)

}
