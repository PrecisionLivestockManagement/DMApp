#' Retrieves data from the DataMuster database for the Table of Cattle Weights Data
#'
#' This function retreives the latest cattle weight data from the DataMuster database and prepares the data for table display on the DataMuster website
#' @name appweightstable
#' @param property the name of the property to search the database
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param paddock the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map
#' @param timezone the timezone of the property to display the weekly weight data
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle RFID numbers, management tags, category and sex information, current paddock allocations and the latest ALMS and crush weights and dates
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appweightstable <- function(property, sex, category, paddock, zoom, timezone, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  property <- sprintf('"stationname":"%s",', property)
  if(sex == "all"){sex <- NULL} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  if(category == "all"){category <- NULL} else {category <- sprintf('"properties.category":"%s",', category)}
  if(is.null(paddock)||zoom == 1){paddock <- NULL}else{paddock <- sprintf('"properties.Paddock":"%s",', paddock)}

  # Set up query to search for cattle

  filter <- paste0("{", property, sex, category, paddock,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"RFID":true, "properties.Management":true, "properties.Paddock":true, "properties.sex":true, "properties.category":true,
                     "properties.recordedtime":true, "properties.wkweight":true, "properties.wkwtdate":true, "properties.stweight":true, "properties.stwtdate":true, "_id":false}')

  cattleinfo <- cattle$find(query = filter, fields = lookfor)

  if(nrow(cattleinfo) == 0){
    cattleinfo <- cattle$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = lookfor)}

  cattleinfo <- cbind(cattleinfo[-1], cattleinfo$properties)

  collist <- colnames(cattleinfo)

    for(i in 1:length(collist)){
    if("POSIXt" %in% class(cattleinfo[,i])){
      attributes(cattleinfo[,i])$tzone <- timezone}}

  s <- Sys.time()
  attr(s,"tzone") <- timezone

  cattleinfof <- cattleinfo%>%
                 mutate(wkwtdate = as.character(wkwtdate, format = "%b %d %Y"),
                        wkwtdate = ifelse(wkwtdate == "Jan 01 1970" | wkwtdate == "Dec 31 1969", "", wkwtdate),
                        wkweight = round(as.numeric(wkweight), 0),
                        wkweight = ifelse(wkweight == 0, as.character(""), as.character(wkweight)),
                        stwtdate = as.character(stwtdate, format = "%b %d %Y"),
                        stwtdate = ifelse(stwtdate == "Jan 01 1970" | stwtdate == "Dec 31 1969", "", stwtdate),
                        stweight = round(as.numeric(stweight), 0),
                        stweight = ifelse(stweight == 0, as.character(""), as.character(stweight)),
                        recordedtime = round(as.numeric(difftime(s, recordedtime, units = "hours")),0),
                        recordedtime = ifelse(recordedtime > 1000, NA, recordedtime))%>%
                 rename("Tag" = Management, "Sex" = sex, "Category" = category, "Hours since last ALMS record" = recordedtime,
                       "ALMS Weight Date" = wkwtdate, "Last Average ALMS Weight (kg)" = wkweight, "Crush Weight Date" = stwtdate, "Last Crush Weight (kg)" = stweight)%>%
                 select(RFID, Tag, Sex, Category, Paddock, `Hours since last ALMS record`, `Last Average ALMS Weight (kg)`, `ALMS Weight Date`, `Last Crush Weight (kg)`, `Crush Weight Date`)%>%
                 filter(RFID != "xxxxxx")

  return(cattleinfof)

}