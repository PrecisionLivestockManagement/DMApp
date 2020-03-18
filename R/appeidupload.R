#' Upload DataMuster EID data to the DataMuster database
#'
#' This function allows DataMuster EID data to be uploaded to the 'appwow' collection in the DataMuster DMIoT database via the DataMuster website
#' @name appeidupload
#' @param RFID a list of the cattle RFID numbers
#' @param datetime a list of timestamp data in POSIXct format
#' @param ALMS a list of ALMS identification codes as character entries
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the data has been successfully uploaded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


appeidupload <- function(RFID, datetime, ALMS, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  wowdata <- mongo(collection = "appwow", db = "DMIoT", url = pass, verbose = T)

  RFID[is.na(RFID)] <- ""

  weight <- rep(0,length(RFID))

  data <- sprintf('{"RFID":"%s", "datetime":{"$date":"%s"}, "Wt":%s, "Location": "%s" , "createdAt":{"$date":"%s"}}', RFID, paste0(substr(datetime,1,10),"T",substr(datetime,12,19),"+1000"), weight, ALMS, paste0(substr(Sys.time(),1,10),"T",substr(Sys.time(),12,19),"+1000"))

  wowdata$insert(data)

}
