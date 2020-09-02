#' Retrieves data for the ALMS Growth graph from the DataMuster database
#'
#' This function retreives weekly weight data from the DataMuster database and prepares the data for graphical display on the DataMuster website
#' @name appalmswts
#' @param property the name of the property to search the database
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param paddock the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map
#' @param start the minimum date of data to be returned, determined by the "Period for ALMS graphs" filter
#' @param rangewt1 the lower value of the ALMS live weight range scale on the "ALMS live weight range" filter
#' @param rangewt2 the upper value of the ALMS live weight range scale on the "ALMS live weight range" filter
#' @param timezone the timezone of the property to display the weekly weight data
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return A dataframe of summarised data showing the average weight of cattle by date and the number of cattle included in the analysis
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import tidyr
#' @export


appalmswts <- function(property, sex, category, paddock, zoom, start, rangewt1, rangewt2, timezone, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  weeklywts <- mongo(collection = "WeeklyWts", db = "DataMuster", url = pass, verbose = T)

  property <- sprintf('"stationname":"%s",', property)
  if(sex == "all"){sex <- NULL} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  if(category == "all"){category <- NULL} else {category <- sprintf('"properties.category":"%s",', category)}
  if(is.null(paddock)||zoom == 1){paddock <- NULL}else{paddock <- sprintf('"properties.Paddock":"%s",', paddock)}

  dates <- seq(as.Date(paste0(start)), as.Date(paste0(Sys.Date())), by = "day")

  if(substr(timezone,1,9) == "Australia"){
  weighdays <- dates[weekdays(dates) == "Sunday"]}else{
    if(timezone == "America/Argentina/Buenos_Aires"){
      weighdays <- dates[weekdays(dates) == "Saturday"]}}

  # Set up query to search for cattle

  filter <- paste0("{", property, sex, category, paddock,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"RFID":true, "_id":false}')

  cattleinfo <- cattle$find(query = filter, fields = lookfor)

  # Set up query to search for weeklywts

  RFID <- paste(unlist(cattleinfo$RFID), collapse = '", "' )
  RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)
  start <- sprintf('"Date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

  filter <- paste0("{", RFID, start,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <-  sprintf('{"RFID":true,"avweight":true, "Date":true, "_id":false}')

  weights <- weeklywts$find(query = filter, fields = lookfor)

  # Find cattle that have had a weekly weight each week and summarise the data by week

  if(nrow(weights) == 0){

    cattleweights <- data.frame()}else{

      cattleweights <- weights%>%
                       mutate(Date = as.Date(Date, tz = timezone),
                              Group = ifelse(avweight == 0, "zerowts", ifelse(avweight > rangewt2, "abovewt", ifelse(avweight < rangewt1, "belowwt", "withinwt"))))%>%
                       group_by(Date, Group)%>%
                       summarise(Number = n())%>%
                       ungroup()

      cattleweights <-  cattleweights%>%
                        spread(key = Group, value = Number)%>%
                        arrange(Date)

      groups <- c("zerowts","belowwt","withinwt","abovewt")
      cattleweights[setdiff(groups, names(cattleweights))] <- NA

      missingdates <- weighdays[which(!(weighdays %in% cattleweights$Date))]

      if(length(missingdates) >= 1){
        toadd <- data.frame(Date = missingdates, "belowwt" = rep(NA, length(missingdates)), "withinwt" = rep(NA, length(missingdates)),
                            "zerowts" = rep(NA, length(missingdates)), "abovewt" = rep(NA, length(missingdates)))
        cattleweights <- rbind(cattleweights, toadd)}

      cattleweights <- cattleweights%>%
        arrange(Date)%>%
        mutate(Date = as.character(as.Date(Date, tz = timezone), format = "%b %d"))

    }

  return(cattleweights)

}
