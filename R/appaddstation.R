#' Add a station to the DataMuster MongoDB database via the DataMuster dashboard.
#'
#' This function adds a station to the DataMuster MongoDB database from the DataMuster online dashboard. You can only access this function if you have read and write permissions.
#' @name appaddstation
#' @param email the login email of the dashboard user
#' @param stationname the name of the property
#' @param lat the latitude of a coordinate point to locate the property
#' @param long the longitude of a coordinate point to locate the property
#' @param area the area of the station in hectares, default is 100 ha
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the station has been successfully added
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


appaddstation <- function(email, stationname, lat, long, area, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
  users <- mongo(collection = "Users", db = "DataMuster", url = pass, verbose = T)

    prop <- stations$find(query = '{}', fields = '{"_id":false}')

    if(stationname == ""){
      returnmessage <<- "Sorry! A property name has not been detected. Please try again or contact the team at info@datamuster.net.au with your property details for assistance."}else{

    if(stationname %in% prop$stationname) {
      returnmessage <<- "Sorry! A matching property has been detected. Your property may already exist in the database or have the same name as another property. Please contact the team at info@datamuster.net.au with your property details for assistance."}else{

    if(is.null(lat) | is.na(as.numeric(lat))){lat <- 0.0}else{lat <- as.numeric(lat)}
    if(is.null(long) | is.na(as.numeric(long))){long <- 0.0}else{long <- as.numeric(long)}
    if(is.null(area) | is.na(as.numeric(area))){area <- 100}else{area <- as.numeric(area)}

    PIC <- "xxxxxx"
    timezone <- "Australia/Brisbane"

    template <- prop[prop$stationname == "CQIRP", ]

    #Input new station details into a template dataframe and insert into database --------

    template$stationname <- stationname
    template$longitude <- long
    template$latitude <- lat
    template$PIC <- PIC
    template$hectares <- area
    template$timezone <- timezone
    template$shortname <- substr(stationname, 1, 3)


    rownames(template)<-c()
    rownames(template$geometry)<-c()

    stations$insert(template)

    #Generate the polygon coordinates and update the Stations collection

    areasqm <- area * 10000
    distfromcentre <- ((areasqm^0.5)/2)/100000

    c1lat <- lat + distfromcentre
    c1long <- long + distfromcentre
    c2lat <-  lat - distfromcentre
    c2long <- long + distfromcentre
    c3lat <- lat - distfromcentre
    c3long <- long - distfromcentre
    c4lat <- lat + distfromcentre
    c4long <- long - distfromcentre
    c5lat <-  c1lat
    c5long <- c1long

    coords <- paste0("[",c1long,",",c1lat,"],[",c2long,",",c2lat,"],[",c3long,",",c3lat,"],[",c4long,",",c4lat,"],[",c5long,",",c5lat,"]")

    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
    IDI <- sprintf('{"stationname":"%s"}', stationname)
    IDS <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', coords)
    stations$update(IDI, IDS)

    #Create a temporary paddock in the Paddocks collection using the polygon coordinates. This will be removed if other paddocks are added.

    propertyinfo <- stations$find(query = sprintf('{"stationname":"%s"}', stationname), fields = '{}')
    #propertyinfo <- get_stations(stationname, username = username, password = password,
    #                             fields = c("_id", "longitude", "latitude", "reports", "PIC", "timezone", "stationname"))
    propertyid <- propertyinfo$`_id`

    template <- paddocks$find(query = '{"stationname":"xxxxxx"}', fields = '{"_id":false}')
    template$stationname <- stationname
    template$stationID <- propertyid
    template$properties$datasource <- "stationpolygon"
    template$properties$hectares <- area
    template$paddname <- stationname
    template$paddnum <- 1
    template$cattle <- 0

    rownames(template)<-c()
    rownames(template$geometry)<-c()

    paddocks$insert(template)

    IDS <- sprintf('{"stationname":"%s"}', stationname)
    IDI <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', coords)
    paddocks$update(IDS, IDI)

    userinfo <- DMApp::appgetuser(email = email, username = username, password = password)

    if(length(userinfo$stations[[1]]) > 0){  #admin users don't have stations listed in this database so don't need the station added to their user info
      propnum <- as.numeric(length(userinfo$stations[[1]]))

      IDS <- sprintf('{"loginemail":"%s"}', email)
      IDI <- sprintf('{"$set":{"stations.%s":"%s"}}', propnum, stationname)
      users$update(IDS, IDI)
    }

    returnmessage <<- "Success! Your property has been uploaded. Your dashboard will automatically refresh when you close this box and you will be able to select your property from the 'Property Choice' box on the left side pane"}

}}

