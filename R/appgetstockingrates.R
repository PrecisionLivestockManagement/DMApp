#' Retreives property information from the DataMuster database
#'
#' This function retrieves property information from the DataMuster database for configuration of the DataMuster website
#' @name appgetstockingrates
#' @param property a list of property names associated with the user
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe showing the number of daily AE's by paddock
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appgetstockingrates <- function(property, start, timezone, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  stockingrates <- mongo(collection = "StockingRates", db = "DataMuster", url = pass, verbose = T)

  paddocks <- appgetpaddocks(property = property, username = username, password = password)

  property <- paste(unlist(property), collapse = '", "' )
  property <- sprintf('"stationname":{"$in":["%s"]},', property)

    if(timezone == "Australia/Brisbane"){
      start <- sprintf('"date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
        if(timezone == "America/Argentina/Buenos_Aires"){
          start <- sprintf('"date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}

  # Set up find query

  search <-paste0("{", property, start,"}")

  if(nchar(search)==2){}else{
    search <- substr(search, 1 , nchar(search)-2)
    search <- paste0(search, "}")}

  lookfor <- sprintf('{"stationname":true, "Paddock":true, "date":true, "TotalAE":true, "_id":false}')

  stockinfo <- stockingrates$find(query = search, fields = lookfor)

  if (nrow(stockinfo) == 0){stockinfo <- stockingrates$find(query = '{"property":"xxxxxx"}', fields = lookfor)}

  stockinfo <- stockinfo%>%
               filter(stationname != "xxxxxx") %>%
               group_by(Paddock) %>%
               summarise(TotalAE = round(sum(TotalAE),0))

  missingpads <- paddocks@data%>%
                 filter(!(paddname %in% stockinfo$Paddock))%>%
                 select(paddname)%>%
                 rename(Paddock = paddname)%>%
                 mutate(TotalAE = 0)

  stockinfo1 <- rbind(stockinfo, missingpads)%>%
                arrange(Paddock)

  return(stockinfo1)

}





