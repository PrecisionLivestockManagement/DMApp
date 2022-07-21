#' Retrieves paddock information from the DataMuster database
#'
#' This function retrieves a list of paddock polygons for display on the property map on the DataMuster website
#' @name appgetpaddocks
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a spatialpolygonsdataframe with a list of the paddock names and associated polygon coordinates
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import sp
#' @export


appgetpaddocks <- function(property, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

  property <- sprintf('"stationname":"%s",', property)

  # Set up query to search for cattle

  filter <- paste0("{", property, "}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"stationname":true, "geometry.coordinates":true, "properties.hectares":true, "paddname":true, "cattle":true, "_id":false}')

  padds <- paddocks$find(query = filter, fields = lookfor)

  padds <- padds %>%
    mutate(hectares = properties$hectares,
           geom = geometry$coordinates)%>%
    select("stationname", "paddname", "hectares", "cattle", "geom")

  paddinfo <- subset(padds, select = -geom)

  cattle = SpatialPolygons(lapply(1:nrow(padds), function(x) Polygons(list(Polygon(matrix(padds$geom[[x]], ncol = 2))), paste0("ID",x))),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  PropPadds <- SpatialPolygonsDataFrame(cattle, paddinfo, match.ID=FALSE)

  for(i in 1:nrow(PropPadds@data)){

    PropPadds$long[i] <- PropPadds@polygons[[i]]@labpt[1]
    PropPadds$lat[i] <- PropPadds@polygons[[i]]@labpt[2]
  }

  return(PropPadds)

}
