#' Upload Gallagher TSi session data to the 'apptsi' collection in the DataMuster DMIoT database
#'
#' This function uploads Gallagher TSi session data to the DataMuster database via the DataMuster website
#' @name apptsisessionupload
#' @param Tag a list of cattle management tag numbers
#' @param EID a list of cattle RFID numbers
#' @param date a list of dates in date format
#' @param liveweight a list of cattle weights
#' @param property the name of the property that the cattle belong
#' @param NLIS a list of cattle NLIS numbers
#' @param pdkfrom a list of paddock names that the cattle have moved from
#' @param pdkto a list of paddock names that the cattle have moved to
#' @param notes observations made by the stationhand as a character entry
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the data has been successfully uploaded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


apptsisessionupload <- function(Tag, EID, date, liveweight, property, NLIS=NULL, pdkfrom=NULL, pdkto=NULL, notes=NULL, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  tsidata <- mongo(collection = "apptsi", db = "DMIoT", url = pass, verbose = T)

  date <- as.Date(date, format = "%d/%m/%Y")
  liveweight <- ifelse(liveweight == "", 0, liveweight)
  NLIS <- ifelse(is.null(NLIS), rep("",length(EID)), NLIS)
  pdkfrom <- ifelse(is.null(pdkfrom), rep("",length(EID)), pdkfrom)
  pdkto <- ifelse(is.null(pdkto), rep("",length(EID)), pdkto)
  notes <- ifelse(is.null(notes), rep("",length(EID)), notes)

  data <- sprintf('{"Tag Number":"%s", "Electronic ID":"%s", "Last Seen Date":{"$date":"%s"}, "Live Weight (kg)":%s, "NLIS":"%s", "Pdk from":"%s", "Pdk to":"%s", "Notes":"%s", "stationname":"%s", "createdAt":{"$date":"%s"}}',
                  Tag, EID, paste0(substr(date,1,10),"T","00:00:00","+1000"), liveweight, NLIS, pdkfrom, pdkto, notes, property, paste0(substr(Sys.time(),1,10),"T",substr(Sys.time(),12,19),"+1000"))

  tsidata$insert(data)

}
