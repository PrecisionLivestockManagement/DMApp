#' Retrieves cattle from the DataMuster database
#'
#' This function provides a list of cattle for a property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name appgetcattle
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a spatialpointsdataframe with a list of cattle RFID numbers, management tags, category and sex information, current paddock allocations and weight data
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


appgetcattle <- function(property, timezone, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  filter <- sprintf('{"stationname":"%s"}', property)
  #lookfor <- sprintf('{"stationname":true, "RFID":true, "properties.Management":true, "geometry":true, "properties.Paddock":true, "properties.sex":true, "properties.category":true, "properties.stweight":true, "properties.stwtdate":true, "properties.weight":true, "properties.recordedtime":true, "properties.wkweight":true, "properties.wkwtdate":true, "properties.ALMS":true, "_id":false}')
  lookfor <- sprintf('{"RFID":true, "geometry":true, "_id":false}')

  cattleinfo <- cattle$find(query = filter, fields = lookfor)

  if(nrow(cattleinfo) == 0){
    cattleinfo <- cattle$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = lookfor)
    }

  cattleinfo <- cattleinfo%>%
                mutate(geom = geometry$coordinates)%>%
                mutate_at(vars(ends_with("date")), as.Date, tz = timezone)

  #cattleinfo <- cbind(cattleinfo[-c(1,2)], cattleinfo$properties)
  cattleinfo <- cattleinfo[-1]

  cattleinfospatial <- SpatialPointsDataFrame(data.frame(matrix(unlist(cattleinfo$geom), nrow=length(cattleinfo$geom), byrow=T)), cattleinfo%>%select(-"geom"))

  #cattleinfospatial <- cattleinfospatial%>%
  #                     rename(property = stationname)

  return(cattleinfospatial)

}
