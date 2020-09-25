#' Change auto draft direction for cattle in the DataMuster database
#'
#' This function allowsthe user to change the auto draft  direction for individuals or groups of cattle in the DataMuster database via the DataMuster website
#' @name appupdatedraftdirection
#' @param RFID a list of cattle RFID numbers
#' @param property the name of the property that cattle belong
#' @param draftdirection the name of the property that cattle belong
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the information has been successfully recorded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


appupdatedraftdirection <- function(RFID, MTag, property, draftdirection, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  draftdirection <- ifelse(draftdirection == "Straight", "S", ifelse(draftdirection == "Left", "L", "R"))

  for (i in 1:length(RFID)){

    if(RFID[i] != "xxx xxxxxxxxxxxx"){
    IDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{
    IDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

    IDI <- sprintf('{"$set":{"properties.AUTODRAFT_dir":"%s"}}', draftdirection)

    cattle$update(IDS, IDI)

    }
}



