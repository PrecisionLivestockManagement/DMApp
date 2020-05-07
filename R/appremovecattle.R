#' Archive cattle in the DataMuster database
#'
#' This function allows individuals or groups of cattle to be archived in the DataMuster database via the DataMuster website
#' @name appremovecattle
#' @param RFID a list of cattle RFID numbers
#' @param MTag a list of cattle management tag numbers
#' @param property the name of the property that cattle belong
#' @param paddock the name of the paddock that the cattle belong
#' @param date the date that the animal was removed from the property in date format, the default is today's date
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the information has been successfully recorded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


appremovecattle <- function(RFID, MTag, property, paddock, date, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  paddockhistory <- mongo(collection = "PaddockHistory", db = "DataMuster", url = pass, verbose = T)
  almshistory <- mongo(collection = "ALMSHistory", db = "DataMuster", url = pass, verbose = T)

  for (i in 1:length(RFID)){

    if(RFID[i] != "xxx xxxxxxxxxxxx"){
    IDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{
    IDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

    banger <- cattle$find(query = IDS, fields ='{"RFID":true, "properties.Management":true, "stationname":true, "properties.ALMS":true, "pdkhist.dateOUT":true, "almshist.dateOFF":true, "_id":true}')
    arrpos <- length(banger$pdkhist$dateOUT[[1]])
    arrpos1 <- length(banger$almshist$dateOFF[[1]])

    # Update Cattle properties
    IDI <- sprintf('{"$set":{"stationname":"%s", "stationID":"%s", "active":"%s", "exstation":"%s", "geometry.coordinates.0":%s, "geometry.coordinates.1":%s, "properties.Paddock":"%s",
                   "properties.PrevPaddock":"%s", "properties.PaddockID":"%s", "properties.exitDate":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                   "xxxxxx", "xxxxxx", "FALSE", property, 0.0, 0.0, "xxxxxx", paddock[i], "xxxxxx", paste0(date,"T","00:00:00","+1000"), "FALSE", "xxxxxx", "xxxxxx")

    #Update Cattle PdkHist
    IDL <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}}}', arrpos, paste0(date,"T","00:00:00","+1000"))
    cattle$update(IDS, IDL)

    #Update Cattle ALMSHist
    if(banger$properties$ALMS == "TRUE"){
    IDLI <- sprintf('{"$set":{"almshist.dateOFF.%s":{"$date":"%s"}}}', arrpos1, paste0(date,"T","00:00:00","+1000"))
    cattle$update(IDS, IDLI)
    }

    #Update history collections

    # Update PaddockHistory collection
    padhist <- DMMongoDB::get_paddockhistory(RFID = banger$RFID, MTag = banger$properties$Management, property = banger$stationname, currentPaddock = "TRUE", username = username, password = password)
    RFIDI <- sprintf('{"_id":{"$oid":"%s"}}', padhist$`_id`)
    RFIDS <- sprintf('{"$set":{"currentPaddock":"%s", "dateOUT":{"$date":"%s"}}}', "FALSE", paste0(substr(date,1,10),"T","00:00:00","+1000"))
    paddockhistory$update(RFIDI, RFIDS)

    # Update ALMSHistory collection
    if(banger$properties$ALMS == "TRUE"){
      almshist <- DMMongoDB::get_almshistory(RFID = banger$RFID, MTag = banger$properties$Management, property = banger$stationname, currentALMS = "TRUE", username = username, password = password)
      RFIDII <- sprintf('{"_id":{"$oid":"%s"}}', almshist$`_id`)
      RFIDSI <- sprintf('{"$set":{"currentALMS":"%s", dateOFF":{"$date":"%s"}}}', "FALSE", paste0(substr(date,1,10),"T","00:00:00","+1000"))
      almshistory$update(RFIDII, RFIDSI)}

    cattle$update(IDS, IDI) # Have to do this one last because stationname changes to "xxxxxx"

    }


}



