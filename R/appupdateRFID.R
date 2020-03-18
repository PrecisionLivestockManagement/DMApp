#' Updates cattle RFID tag numbers in the DataMuster database
#'
#' This function allows cattle RFID numbers to be updated in the DataMuster database via the DataMuster website
#' @name appupdateRFID
#' @param RFID a list of the previous cattle RFID numbers
#' @param MTag a list of cattle management tag numbers
#' @param newRFID a list of the new cattle RFID numbers
#' @param date the date that the new RFID tag was applied, in date format. Default is today's date.
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the RFID tag number has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export

appupdateRFID <- function(RFID, MTag, newRFID, date, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  # Checks that the RFID numbers are in the correct format for the database

  if("TRUE" %in% (nchar(as.character(newRFID))!= 16)) {
    stop(paste0("One or more of the new RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  # Checks that the new RFID numbers are not already registered in the database

  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checknewRFID)
  check <- cattle$count(query = filtercattle)

  if (check != 0) {

    stop("One or more of the new RFID numbers are already registered in the database. Please check that the RFID numbers are correct and try again")}

  for (i in 1:length(RFID)){

    if(RFID[i] != "xxx xxxxxxxxxxxx"){
      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{
        RFIDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

          banger <- cattle$find(query = RFIDS, fields='{"RFIDhist.date":true, "_id":false}')
          arrpos <- length(banger$RFIDhist$date[[1]])
          RFIDIlast <- sprintf('{"$set":{"RFID":"%s"}}', newRFID[i])
          RFIDI <- sprintf('{"$set":{"RFIDhist.date.%s":{"$date":"%s"}, "RFIDhist.ID.%s":"%s"}}', arrpos, paste0(substr(date,1,10),"T","00:00:00","+1000"), arrpos, newRFID[i])

      cattle$update(RFIDS, RFIDI) # Has to update RFIDI first otherwise won't update RFIDhist
      cattle$update(RFIDS, RFIDIlast)

  }

  }


