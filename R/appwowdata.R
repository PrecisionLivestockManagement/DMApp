#' Retrieve daily WoW data from the DataMuster database via the DataMuster website app
#'
#' This function allows daily ALMS data to be retreived from the DataMuster database via the DataMuster website app
#' @name appwowdata
#' @param start a start date to be returned in date format
#' @param end an end date to be returned in date format.
#' @param location the name of the alms unit, if NULL data for all units will be returned
#' @param username required for access. Please email \email{info@@datamuster.net.au} to acquire a username
#' @param password required for access. Please email \email{info@@datamuster.net.au} to acquire a password
#' @return a dataframe of WoW weights that have been recorded during the specified time period
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


appwowdata <- function(start=NULL, end=NULL, location=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  wowdata <- mongo(collection = "DailyWts", db = "DataMuster", url = pass, verbose = T)

  data <- wowdata$find(query = '{}', fields='{"RFID":true, "Wt":true, "datetime":true, "Location":true, "_id":false}')

  if(is.null(start)){}else{
    if(is.null(end)){data <- data %>% filter(between(as.Date(datetime, tz = "Australia/Brisbane"),start,Sys.Date()))}
    else{data <- data %>% filter(between(as.Date(datetime, tz = "Australia/Brisbane"),start,end))}}

  data <- data%>%
    mutate(datetime = as.POSIXct(strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")))%>%
    rename(Weight = "Wt", Datetime = "datetime")%>%
    select("RFID", "Weight", "Datetime", "Location")

  if(is.null(location)){}else{
    data <- data%>%
      filter(Location %in% location)
  }

  return(data)

}




