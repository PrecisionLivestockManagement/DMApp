#' Retrieves cattle information from the DataMuster database
#'
#' This function allows cattle information to be retreived from the DataMuster database via the DataMuster website app
#' @name appcattle
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a spatialpointsdataframe with a list of the RFID numbers and a number of other data points
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import rgdal
#' @export


appcattle <- function(property, sex, category, paddockselect, paddock, weightselect, minwt, maxwt, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "' )

  if(sex == "all"){sex <- paste(unlist(c("male", "female")), collapse = '", "')}else{sex <- paste(unlist(sex), collapse = '", "' )}

  if(category == "all"){category <- paste(unlist(c("growing", "breeding")), collapse = '", "')}else{category <- paste(unlist(category), collapse = '", "' )}

  if(weightselect==1){
    minwt <- 0
    maxwt <- 2000}

  if(paddockselect==1||is.null(paddock)){

  filterdata <- sprintf('{"stationname":{"$in":["%s"]}, "properties.sex":{"$in":["%s"]}, "properties.category":{"$in":["%s"]}, "properties.wkweight":{"$gte":%s}, "properties.wkweight":{"$lte":%s}}', property, sex, category, minwt, maxwt)}else{
  paddock <- paste(unlist(paddock), collapse = '", "' )
  filterdata <- sprintf('{"stationname":{"$in":["%s"]}, "properties.sex":{"$in":["%s"]}, "properties.category":{"$in":["%s"]}, "properties.wkweight":{"$gte":%s}, "properties.wkweight":{"$lte":%s}, "properties.Paddock":{"$in":["%s"]}}', property, sex, category, minwt, maxwt, paddock)}

  lookfor <- sprintf('{"stationname":true, "RFID":true, "properties.Management":true, "geometry":true, "properties.Paddock":true, "properties.sex":true, "properties.category":true, "properties.stweight":true, "properties.stwtdate":true, "properties.weight":true, "properties.recordedtime":true, "properties.wkweight":true, "properties.wkwtdate":true, "properties.ALMS":true, "_id":false}')
  cattleinfo <- cattle$find(query = filterdata, fields=lookfor)

  if (nrow(cattleinfo) != 0){

  cattleinfo$properties$RFID <- cattleinfo$RFID
  cattleinfo$properties$stationname <- cattleinfo$stationname
  cattleinfo$properties$geom <- cattleinfo$geometry$coordinates
  cattleinfo <- cattleinfo$properties}

  if(length(unique(cattleinfo$stationname)) != length(property)){

  missingprops <- property[!(property %in% cattleinfo$stationname)]

  cattleinfo1 <- cattle$find(query = sprintf('{"RFID":"xxxxxx"}'), fields=lookfor)
  cattleinfo1 <- cattleinfo1[rep(1:nrow(cattleinfo1),each=length(missingprops)),]

  cattleinfo1$stationname <- missingprops

  cattleinfo1$properties$RFID <- cattleinfo1$RFID
  cattleinfo1$properties$stationname <- cattleinfo1$stationname
  cattleinfo1$properties$geom <- cattleinfo1$geometry$coordinates
  cattleinfo1 <- cattleinfo1$properties

  cattleinfo <- rbind(cattleinfo, cattleinfo1)
  }

  cattleinfo$stwtdate <- as.Date(cattleinfo$stwtdate, tz = "Australia/Brisbane")
  cattleinfo$wkwtdate <- as.Date(cattleinfo$wkwtdate, tz = "Australia/Brisbane")

  cattleinfo <- cattleinfo %>% rename(property = stationname)

  cattleinfospatial <- SpatialPointsDataFrame(data.frame(matrix(unlist(cattleinfo$geom), nrow=length(cattleinfo$geom), byrow=T)), cattleinfo%>%select(-"geom"))

  #cattleinfospatial <- SpatialPointsDataFrame(data.frame(matrix(c(0, 0), nrow=1, ncol=2, byrow = TRUE)), cattleinfo)

  return(cattleinfospatial)

}
