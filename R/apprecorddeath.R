#' Record a cattle death to the DataMuster database
#'
#' This function allows death information to be recorded for individuals or groups of cattle to the DataMuster database via the DataMuster website.
#' @name apprecorddeath
#' @param RFID a list of cattle RFID numbers
#' @param MTag a list of cattle management tag numbers
#' @param date the date that the animal died in date format, the default is today's date
#' @param cause the cause of death
#' @param property the name of the property that the cattle belong
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not information has been successfully recorded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


apprecorddeath <- function(RFID, MTag, date, cause, property, paddock, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  for (i in 1:length(RFID)){

    if(RFID[i] != "xxx xxxxxxxxxxxx"){
      IDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{
      IDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

    banger <- cattle$find(query = IDS, fields= sprintf('{"RFID":true,"pdkhist.dateOUT":true,"almshist.dateOFF":true, "_id":false}'))
    arrpos1 <- length(banger$pdkhist$dateOUT[[1]])
    arrpos2 <- length(banger$almshist$dateOFF[[1]])

    IDI <- sprintf('{"$set":{"stationname":"%s", "stationID":"%s", "active":"%s", "exstation":"%s", "geometry.coordinates.0":%s, "geometry.coordinates.1":%s,
                     "properties.Paddock":"%s", "properties.PaddockID":"%s", "properties.PrevPaddock":"%s", "properties.deathcause":"%s", "properties.deathDate":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                     "xxxxxx", "xxxxxx", "FALSE", property, 0.0, 0.0, "xxxxxx", "xxxxxx", paddock[i], cause, paste0(date,"T","00:00:00","+1000"),"FALSE", "xxxxxx", "xxxxxx")

    IDL <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}}}', arrpos1, paste0(date,"T","00:00:00","+1000"))

    cattle$update(IDS, IDL) # Have to do this one first before stationname changes to "xxxxxx
    cattle$update(IDS, IDI)


    }

}



