#' Retrieves cattle weekly ALMS weights from the DataMuster database via the DataMuster website app
#'
#' This function allows cattle ALMS weekly weight data to be retreived from the DataMuster database via the DataMuster website app
#' @name appweeklywts
#' @param RFID this is a list of cattle RFID numbers
#' @param start a start date to be returned, this has to be in date format
#' @param end an end date to be returned, this has to be in date format
#' @param values this is the minimum number of weight values that need to be recorded to be uploaded, the default is 1
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return A dataframe of the RFID numbers that have been returned and weekly average weights for each animal
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


appweeklywts <- function(RFID, start=NULL, end=NULL, values=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}
  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  if(is.null(values)||values < 1 ){values <- 1}

  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan2 <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "wkwthist.date":true, "wkwthist.avweight":true, "_id":false}')

  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){

    weeklywts <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan2$wkwthist$date[[i]]))), c("Date", "Weight"))

    if(nrow(weeklywts) >= 1) {
    weeklywts$Date <- as.Date(jan2$wkwthist$date[[i]], tz = "Australia/Brisbane")
    weeklywts$Weight <- jan2$wkwthist$avweight[[i]]}

    if(is.null(start)) {}
    else{if(is.null(end)){weeklywts <- weeklywts %>% filter(between(as.Date(Date),start,Sys.Date()))}
      else{weeklywts <- weeklywts %>% filter(between(as.Date(Date),start,end))}}

    #This is the section where we can apply further filters based on breed, class, etc.

    if(nrow(weeklywts)<values){jan2$RFID[[i]] <- "xxxx"}
    else{cattleinfo[[jan2$RFID[i]]] <- as.data.frame(weeklywts)}
  }

  RFID <- jan2[which(jan2$RFID!="xxxx"),]
  cattleinfo <- list(RFID=RFID$RFID, Property=RFID$stationname, WeeklyWeights=cattleinfo)

  cattleinfo <- bind_rows(cattleinfo$WeeklyWeights, .id = "RFID")

  cattleinfo$Date <- as.Date(cattleinfo$Date, tz = "Australia/Brisbane")

  return(cattleinfo)

}
