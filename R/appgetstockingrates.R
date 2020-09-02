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

  todaysdate <- Sys.time()
  attr(todaysdate, "tzone") <- timezone
  todaysdate <- as.Date(todaysdate, tz = timezone)

  days <- as.numeric(todaysdate - as.Date(start, tz = timezone))

  paddocks <- appgetpaddocks(property = property, username = username, password = password)

  grazingpaddocks <- paddocks$paddname[paddocks$padduse == "grazing"]

  property <- paste(unlist(property), collapse = '", "' )
  property <- sprintf('"stationname":{"$in":["%s"]},', property)

    if(substr(timezone,1,9) == "Australia"){
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

  #if (nrow(stockinfo) == 0){stockinfo <- stockingrates$find(query = '{"property":"xxxxxx"}', fields = lookfor)}

  paddinfo <- data.frame()

  for(i in 1:length(grazingpaddocks)){
    stockAEs <- stockinfo$TotalAE[stockinfo$Paddock == grazingpaddocks[i]]
    grazingdays <- length(stockAEs)
    spelldays <- rep(0, each = days - length(stockAEs))
    spelldaysto12months <-  rep(0, each = 365 - days)
    STCC <- round(mean(c(stockAEs, spelldays)), 0)
    STCCto12months <- round(mean(c(stockAEs, spelldays, spelldaysto12months)), 0)

    row <- data.frame("Paddock" = grazingpaddocks[i], "grazingdays" = grazingdays, "spelldays" = length(spelldays), "spelldaysto12months" = length(spelldaysto12months), "STCC" = STCC, "STCCto12months" = STCCto12months, stringsAsFactors = FALSE)
    paddinfo <- rbind(paddinfo, row)
  }

  paddinfo1 <- left_join(paddinfo, paddocks@data, by = c("Paddock" = "paddname"))%>%
               select(Paddock, grazingdays, spelldays, spelldaysto12months, STCC, STCCto12months, LTCC) %>%
               mutate(STCCperc = round((STCCto12months/LTCC)*100,0))%>%
               mutate_at(vars(STCCperc), ~replace(., is.nan(.), 0)) %>%
               mutate(STCCcategory = cut(STCCperc, breaks = c(-Inf, 25, 50, 75, 100, 125, Inf), labels = c("0 - 25%", "26 - 50%", "51 - 75%", "76 - 100%", "101 - 125%", "125%+")))

  return(paddinfo1)

}





