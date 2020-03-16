#' Retrieve daily WoW data from the DataMuster database via the DataMuster website app
#'
#' This function allows daily ALMS data to be retreived from the DataMuster database via the DataMuster website app
#' @name apptodaysdata
#' @param property the name of the property to search the database
#' @param alms the name of the alms unit
#' @param username required for access. Please email \email{info@@datamuster.net.au} to acquire a username
#' @param password required for access. Please email \email{info@@datamuster.net.au} to acquire a password
#' @return a dataframe of WoW weights that have been recorded during the specified time period
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import keyring
#' @import dplyr
#' @export


apptodaysdata <- function(alms, timezone, start, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  wowdata <- mongo(collection = "DailyWts", db = "DataMuster", url = pass, verbose = T)

  if(length(alms) == 0){dataf <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("RFID", "Weight", "Datetime"))}else{

    alms <- sprintf('"Location":"%s",', alms)

      if(timezone == "Australia/Brisbane"){
        start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(substr(start,1,10), "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
          if(timezone == "America/Argentina/Buenos_Aires"){
            start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(substr(start,1,10), "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}

    # Set up query to search for data

    filter <- paste0("{", alms, start,"}")
    filter <- substr(filter, 1 , nchar(filter)-2)
    filter <- paste0(filter, "}")

    lookfor <- sprintf('{"RFID":true, "Wt":true, "datetime":true, "_id":false}')

    data <- wowdata$find(query = filter, fields = lookfor)

    dataf <- data

    collist <- colnames(dataf)

    if(nrow(dataf) != 0){
      for(i in 1:length(collist)){
        if("POSIXt" %in% class(dataf[,i])){
          attributes(dataf[,i])$tzone <- timezone}}
    }

    if(nrow(dataf) == 0){dataf <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("RFID", "Weight", "Datetime"))}else{
    dataf <- dataf%>%
      mutate(#datetime = as.POSIXct(strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = timezone)),
             datetime = as.character(datetime, format = "%Y-%m-%d %H:%M:%S"),
             Wt = round(as.numeric(Wt),0))%>%
      rename(Weight = "Wt", Datetime = "datetime")
    }
    }

  return(dataf)

}




