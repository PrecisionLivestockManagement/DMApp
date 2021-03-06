#' Retrieves cattle from the DataMuster database
#'
#' This function retreives a list of cattle, their paddock allocations and spatial coordinates for display on the property map on the DataMuster website
#' @name appgetcattleinfo
#' @param property the name of the property to search the database
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param paddock the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle numbers by paddock
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


appgetcattleinfo <- function(property, sex, category, paddock, zoom, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

  stationname <- sprintf('"stationname":"%s",', property)
  if(sex == "all"){sex <- NULL} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  if(category == "all"){category <- NULL} else {category <- sprintf('"properties.category":"%s",', category)}
  if(is.null(paddock)||zoom == 1){paddock <- NULL}else{paddock <- sprintf('"properties.Paddock":"%s",', paddock)}

  # Set up query to search for cattle

  filter <- paste0("{", stationname, sex, category, paddock,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"properties.Paddock":true, "properties.AE":true, "properties.category":true, "properties.stweight":true, "properties.stwtdate":true,
                     "properties.wkweight":true, "properties.wkwtdate":true, "_id":false}')

  cattleinfo <- cattle$find(query = filter, fields = lookfor)

  if(nrow(cattleinfo) == 0){
    cattleinfo <- cattle$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = lookfor)}

  cattleinfo1 <- cattleinfo$properties %>%
    mutate(weight = ifelse(wkwtdate >= stwtdate, wkweight, stweight),
           weightdate = ifelse(wkwtdate >= stwtdate, as.character(wkwtdate), as.character(stwtdate)),
           weightdate = as.Date(weightdate),
           weight = ifelse(weight == 0, NA, weight))

  countcattle <- cattleinfo1 %>%
    group_by(Paddock, category) %>%
    summarise(cattle = n(),
              AE = round(sum(AE),0),
              avweight = round(mean(as.numeric(weight), na.rm = T),0),
              lastdate = max(weightdate)) %>%
    mutate(avweight = ifelse(is.nan(avweight), 0, avweight))

  # searchpads <- paste(unlist(countcattle$Paddock), collapse = '", "' )
  #
  # filterpaddock <- sprintf('{"stationname":"%s", "paddname":{"$in":["%s"]}}', property, searchpads)
  #
  # paddinfo <- paddocks$find(query = filterpaddock, fields = sprintf('{"paddname":true, "geometry":true, "_id":false}'))
  #
  # if(nrow(paddinfo) == 0){
  #   paddinfo <- paddocks$find(query = sprintf('{"stationname":"xxxxxx"}'), fields = sprintf('{"paddname":true, "geometry":true, "_id":false}'))}
  #
  # paddinfo <- paddinfo %>%
  #             mutate(long = as.numeric(unlist(lapply(paddinfo$geometry$coordinates, `[[`, 1))),
  #                    lat = as.numeric(unlist(lapply(paddinfo$geometry$coordinates, `[[`, 2))),) %>%
  #             select(-geometry)
  #
  # countcattle <- left_join(countcattle, paddinfo, by = c("Paddock" = "paddname")) %>%
  #                filter(Paddock != "xxxxxx")

  return(countcattle)

}
