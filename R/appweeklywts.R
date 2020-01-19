#' Retrieves cattle weekly ALMS weights from the DataMuster database via the DataMuster website app
#'
#' This function allows cattle ALMS weekly weight data to be retreived from the DataMuster database via the DataMuster website app
#' @name appweeklywts
#' @param RFID this is a list of cattle RFID numbers
#' @param start a start date to be returned, this has to be in date format
#' @param end an end date to be returned, this has to be in date format
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return A dataframe of the RFID numbers that have been returned and weekly average weights for each animal
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


appweeklywts <- function(RFID, start, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)

  lookfor <- sprintf('{"RFID":true, "stationname":true, "properties.Management":true, "properties.Paddock":true, "properties.sex":true,
                       "properties.category":true,  "wkwthist.date":true, "wkwthist.avweight":true, "_id":false}')


  jan2 <- cattle$find(query = filterstation, fields = lookfor)

  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){

    weeklywts <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan2$wkwthist$date[[i]]))), c("Date", "Weight"))

    if(nrow(weeklywts) >= 1) {
    weeklywts$Date <- as.Date(jan2$wkwthist$date[[i]], tz = "Australia/Brisbane")
    weeklywts$Weight <- jan2$wkwthist$avweight[[i]]}

    weeklywts <- weeklywts %>% filter(between(as.Date(Date),start,Sys.Date()))

    #This is the section where we can apply further filters based on breed, class, etc.

  cattleinfo[[jan2$RFID[i]]] <- as.data.frame(weeklywts)}

  RFID <- jan2[which(jan2$RFID!="xxxx"),]

  cattleinfo <- list(RFID=RFID$RFID, WeeklyWeights=cattleinfo)

  cattleinfo <- bind_rows(cattleinfo$WeeklyWeights, .id = "RFID")

  cattleinfo <- inner_join(cattleinfo, jan2, by = "RFID")

  if (nrow(cattleinfo) != 0){

    cattleinfo$properties$RFID <- cattleinfo$RFID
    cattleinfo$properties$Date <- cattleinfo$Date
    cattleinfo$properties$Weight <- cattleinfo$Weight
    cattleinfo$properties$stationname <- cattleinfo$stationname
    cattleinfo <- cattleinfo$properties}

  cattleinfo$Date <- as.Date(cattleinfo$Date, tz = "Australia/Brisbane")

  cattleinfo <- cattleinfo %>% rename(property = stationname)

  return(cattleinfo)

}
