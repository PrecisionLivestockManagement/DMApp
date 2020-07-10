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

  prop <- appgetstations(property = property, accesslevel = "0", username = username, password = password)

  lookfor <- sprintf('{"stationname":true, "geometry.coordinates":true, "properties.hectares":true, "paddname":true,
                     "ALMSrating":true, "LTCC_A":true, "LTCC_B":true, "LTCC_C":true, "LTCC_D":true, "padduse":true, "condition":true,
                     "estDM_ha":true, "estDM_date":true, "DMsamples_n":true, "DMsamples_ha":true, "DMsamples_date":true, "availDM_ha":true, "DMI_ha":true,"_id":false}')

  filter <- sprintf('{"stationname":"%s"}', property)

  tempadds <- paddocks$find(query = filter, fields = lookfor)

  tempadds$LTCC <- 0
  for(i in 1:nrow(tempadds)){
    colname <- paste0("LTCC_", tempadds$condition[i])
    colindex <- which(colnames(tempadds) == colname)
    tempadds$LTCC[i] <- tempadds[i, colindex]
  }

  tempadds <- tempadds %>%
    mutate(hectares = properties$hectares,
           geom = geometry$coordinates,
           DMsamples_date = as.Date(DMsamples_date, tz = prop$timezone),
           DMsamples_date = as.character(DMsamples_date),
           DMsamples_date = ifelse(DMsamples_date == "1970-01-01", NA, DMsamples_date),
           estDM_date = as.Date(estDM_date, tz = prop$timezone),
           estDM_date = as.character(estDM_date),
           estDM_date = ifelse(estDM_date == "1970-01-01", "", estDM_date),
           DMsamples_ha = ifelse(DMsamples_ha == 0, NA, DMsamples_ha),
           estDM_ha = ifelse(estDM_ha == 0, NA, estDM_ha),
           availDM_ha = ifelse(availDM_ha == 0, NA, availDM_ha),
           DMI_ha = ifelse(DMI_ha == 0, NA, DMI_ha))%>%
  select("stationname", "paddname", "hectares", "LTCC_A", "LTCC_B", "LTCC_C", "LTCC_D", "ALMSrating", "padduse", "condition", "LTCC",
         "estDM_ha", "estDM_date", "DMsamples_n", "DMsamples_ha", "DMsamples_date", "availDM_ha", "DMI_ha", "geom")

  cattle = SpatialPolygons(lapply(1:nrow(tempadds), function(x) Polygons(list(Polygon(matrix(tempadds$geom[[x]], ncol = 2))), paste0("ID",x))), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  cattleadd <- tempadds%>%
               select(stationname, paddname, hectares, LTCC_A, LTCC_B, LTCC_C, LTCC_D, ALMSrating, padduse, condition, LTCC,
                      estDM_ha, estDM_date, DMsamples_n, DMsamples_ha, DMsamples_date, availDM_ha, DMI_ha)

  PropPadds <- SpatialPolygonsDataFrame(cattle, cattleadd, match.ID=FALSE)

  cattlelist <- DMApp::appgetcattle(property = property, sex = "all", category = "all", zoom = NULL, paddock = NULL, username = username, password = password)

  for(i in 1:nrow(PropPadds@data)){

    PropPadds$long[i] <- PropPadds@polygons[[i]]@labpt[1]
    PropPadds$lat[i] <- PropPadds@polygons[[i]]@labpt[2]

    PropPadds$cattle[i] <- ifelse(PropPadds$paddname[i] %in% cattlelist$Paddock, sum(cattlelist$cattle[PropPadds$paddname[i] == cattlelist$Paddock]), 0)
    PropPadds$AE[i] <- ifelse(PropPadds$paddname[i] %in% cattlelist$Paddock, sum(cattlelist$AE[PropPadds$paddname[i] == cattlelist$Paddock]), 0)
    PropPadds$daysleft[i] <- ifelse(PropPadds$AE[i] != 0, round(PropPadds$availDM_ha[i]/PropPadds$DMI_ha[i],0), NA)

    if(!is.na(PropPadds$daysleft[i])){PropPadds$destockdate[i] <- as.character(Sys.Date()+as.numeric(PropPadds$daysleft[i]))}else{PropPadds$destockdate[i] <- NA}

  }

  return(PropPadds)

}
