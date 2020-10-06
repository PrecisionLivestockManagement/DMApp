#' Retrieves paddock information from the DataMuster database
#'
#' This function retrieves a list of paddock polygons for display on the property map on the DataMuster website
#' @name appgetpaddocklist
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a spatialpolygonsdataframe with a list of the paddock names and associated polygon coordinates
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import sp
#' @export


appgetpaddocklist <- function(property, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddocklist <- mongo(collection = "Paddocklist", db = "DMApp", url = pass, verbose = T)

  property <- sprintf('"stationname":"%s",', property)

  # Set up query to search for cattle

  filter <- paste0("{", property, "}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"_id":false}')

  padds <- paddocklist$find(query = filter, fields = lookfor)

  paddinfo <- subset(padds, select = -geom) %>%
              mutate(forcategory = factor(forcategory, levels= c("< 1000", "1000 - 2000", "2001 - 3000", "3001 - 4000", "4001 - 5000", "> 5000"))) #%>%
              #arrange(forcategory)

  cattle = SpatialPolygons(lapply(1:nrow(padds), function(x) Polygons(list(Polygon(matrix(padds$geom[[x]], ncol = 2))), paste0("ID",x))),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  PropPadds <- SpatialPolygonsDataFrame(cattle, paddinfo, match.ID=FALSE)

  for(i in 1:nrow(PropPadds@data)){

    PropPadds$long[i] <- PropPadds@polygons[[i]]@labpt[1]
    PropPadds$lat[i] <- PropPadds@polygons[[i]]@labpt[2]
  }

  return(PropPadds)

}
