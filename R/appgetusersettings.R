#' Retrieves user settings from the DataMuster database
#'
#' This function allows user settings to be retreived from the DataMuster database via the DataMuster website app
#' @name appgetusersettings
#' @param email the email address of the registered user
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a dataframe showing the information associated with the user settings
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


appgetusersettings <- function(email, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  userinf <- mongo(collection = "UserSettings", db = "DataMuster", url = pass, verbose = T)

  lookfor <- sprintf('{"_id":false}')
  filterusers <- sprintf('{"User":"%s"}', email)

  userinfo <- userinf$find(query = filterusers, fields=lookfor)

  return(userinfo)

}