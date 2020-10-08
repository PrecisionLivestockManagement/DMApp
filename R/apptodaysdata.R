#' Retrieves daily WoW data from the DataMuster database
#'
#' This function retrieves daily ALMS data from the DataMuster for table display on the DataMuster website
#' @name apptodaysdata
#' @param property the name of the property to search the database
#' @param alms the name of the alms unit
#' @param start the minimum date of data to be returned
#' @param timezone the timezone of the property to display the weight data
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe of WoW weights that have been recorded during the specified time period
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


apptodaysdata <- function(property, alms, timezone, start, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  wowdata <- mongo(collection = "DailyWts", db = "DataMuster", url = pass, verbose = T)

  if(alms == "No Data"){dataf <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("RFID", "Weight", "Datetime"))}else{

    unit <- appgetinfrastructure(property = property, timezone = "Australia/Brisbane", username = username, password = password) # Timezone doesn't matter here as it is not used

    unit <- unit %>% filter(asset_id == alms)

    alms <- sprintf('"Location":"%s",', unit$filename)

    if(substr(timezone,1,9) == "Australia"){
        start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', paste0(substr(start-1,1,10), "T14:00:00Z"))}else{
          if(timezone == "America/Argentina/Buenos_Aires"){
            start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', paste0(substr(start,1,10), "T03:00:00Z"))}}

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




