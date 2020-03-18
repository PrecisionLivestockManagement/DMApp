#' Update cattle coordinates in the DataMuster database
#'
#' This function updates cattle spatial coordinates, after moving or removing cattle, for display on the property map on the DataMuster website
#' @name appmovecattle
#' @param property the name of the property to search the database
#' @param paddock the paddock allocation of the cattle
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the data has been updated successfully
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @import splancs
#' @import spdplyr
#' @export


appmovecattle <- function(property, paddock, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "' )

  if (is.null(paddock)){
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property)}else{

  paddock <- paste(unlist(paddock), collapse = '", "' )
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}, "properties.Paddock":{"$in":["%s"]}}', property, paddock)}

  lookfor <- sprintf('{"stationname":true, "RFID":true, "properties.Management":true, "geometry":true, "properties.Paddock":true, "_id":true}')
  cattleinfo <- cattle$find(query = filterstation, fields=lookfor)


  cattleinfospatial <- SpatialPointsDataFrame(data.frame(matrix(unlist(cattleinfo$geometry$coordinates), nrow=length(cattleinfo$geometry$coordinates), byrow=T)), cattleinfo$properties)

  cattleinfospatial@data["RFID"] <- cattleinfo$RFID
  cattleinfospatial@data["property"] <- cattleinfo$stationname
  cattleinfospatial@data["ID"] <- cattleinfo$`_id`

  pad <- apppaddocks(property, username = username, password = password)
  cat <- cattleinfospatial

  kipper <- lapply(1:nrow(cat), function(x) {
    snicker <- pad%>%filter(paddname==cat$Paddock[x])
    csr(snicker@polygons[[1]]@Polygons[[1]]@coords, 1)
  })

  for(i in 1:nrow(cat)){
    catman <- cat$ID[i]

  RFIDS <- sprintf('{"_id" : {"$oid":"%s"}}', catman)

  RFIDI <- sprintf('{"$set":{"geometry.coordinates.0":%s, "geometry.coordinates.1":%s}}', kipper[[i]][1],kipper[[i]][2])

  cattle$update(RFIDS, RFIDI)

  }

}
