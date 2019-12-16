#' Retreive cattle ALMS use data from the DataMuster database
#'
#' This function allows cattle ALMS usage data to be retreived from the DataMuster database via the DataMuster website
#' @name appalmsuse
#' @param property the name of the property to search the database
#' @param start the start date of data to be returned, in date format
#' @param end the end date of data to be returned, in date format
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a dataframe showing individual cattle and a daily indicator of whether or not they were recorded by the ALMS, 1 = recorded and 0 = not recorded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import DMMongoDB
#' @export


appalmsuse <- function(property, start=NULL, end=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  inf <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  if(is.null(start)){start <- as.Date("2014-09-01")}
  if(is.null(end)){end <- Sys.Date()}

  wowunits <- infsearch(property, infstype = "Walk-over-Weighing Unit", username = username, password = password)

  cows <- propsearchfull(property, archives = TRUE, username = username, password = password)

  cows <- cows%>%
          filter(RFID != "xxx xxxxxxxxxxxx")

  cattlehistory <- almshistsearch(property, start = start, end = end, username = username, password = password)

  cattlehistory <- bind_rows(cattlehistory$ALMSHistory, .id = "RFID")%>%
    filter(RFID != "xxx xxxxxxxxxxxx")%>%
    mutate(dateOFF = as.character(dateOFF),
           dateOFF = ifelse(is.na(dateOFF), as.character(Sys.Date()), dateOFF))

  cattleweights <- dailywtsNEW(cows$RFID, start = start, end = end, username = username, password = password)

  cattleweights <- bind_rows(cattleweights$DailyWeights, .id = "RFID")%>%
    mutate(Date = as.Date(Date, tz = "Australia/Brisbane"))%>%
    select(RFID, Date)%>%
    distinct()

  usehistory <- data.frame()

  if (nrow(cattlehistory) == 0){}else{

    newstart <- min(cattlehistory$dateON)

    dates <- seq.Date(newstart, end, by = "days")

    for (k in 1:length(dates)){

      tempdf <- cattlehistory %>%
        filter(as.Date(dateON) <= dates[k] & as.Date(dateOFF) >= dates[k])%>%
        mutate(Date = dates[k])%>%
        select(Date, Property,ALMS, RFID)

      if (nrow(tempdf) == 0) {} else {
        usehistory <- rbind(usehistory, tempdf)
      }}

    usehistory$Count <- ifelse(paste0(usehistory$RFID, usehistory$Date) %in% paste0(cattleweights$RFID, cattleweights$Date), 1, 0)

    cattleinfo <- left_join(usehistory, cows, by = "RFID")%>%
      select(Date, Property, ALMS.x, RFID, Management, category, sex, Count)%>%
      rename(ALMS = "ALMS.x", Category = "category", Sex = "sex")
  }

  return(cattleinfo)

}
