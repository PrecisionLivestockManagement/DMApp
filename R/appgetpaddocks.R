#' Retrieves paddock information from the DataMuster database
#'
#' This function allows a list of paddock polygons to be retreived from the DataMuster database via the DataMuster website
#' @name appgetpaddocks
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a spatialpolygonsdataframe with a list of the paddock names and associated polygon coordinates
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import sp
#' @export


appgetpaddocks <- function(property, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

  lookfor <- sprintf('{"stationname":true, "geometry.coordinates":true, "properties.hectares":true, "paddname":true, "_id":false}')
  filter <- sprintf('{"stationname":"%s"}', property)

  tempadds <- paddocks$find(query = filter, fields = lookfor)

  tempadds <- tempadds%>%
              mutate(hectares = properties$hectares,
              geom = geometry$coordinates)%>%
              select("stationname", "paddname", "hectares", "geom")

  cattle = SpatialPolygons(lapply(1:nrow(tempadds), function(x) Polygons(list(Polygon(matrix(tempadds$geom[[x]], ncol = 2))), paste0("ID",x))), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  cattleadd <- tempadds%>%
               select(stationname, paddname, hectares)

  PropPadds <- SpatialPolygonsDataFrame(cattle, cattleadd, match.ID=FALSE)

  return(PropPadds)

}