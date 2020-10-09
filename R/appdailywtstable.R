#' Retrieves data from the DataMuster database for the Daily Weights table
#'
#' This function retreives cattle daily weight data from the DataMuster database and prepares the data for table display on the DataMuster website
#' @name appdailywtstable
#' @param property the name of the property to search the database
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param paddock the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle RFID numbers, management tags and daily weight data
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import tidyr
#' @export


appdailywtstable <- function(property, sex, category, paddock, zoom, timezone, start, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  dailywts <- mongo(collection = "DailyWts", db = "DataMuster", url = pass, verbose = T)

  property <- sprintf('"stationname":"%s",', property)
  if(sex == "all"){sex <- NULL} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  if(category == "all"){category <- NULL} else {category <- sprintf('"properties.category":"%s",', category)}
  if(is.null(paddock)||zoom == 1){paddock <- NULL}else{paddock <- sprintf('"properties.Paddock":"%s",', paddock)}
  alms <- sprintf('"properties.ALMS":"%s",', "TRUE")

  dates <- as.character(seq.Date(start, as.Date(Sys.time(), tz = timezone), by = "days"))

  # Set up query to search for cattle

  filter <- paste0("{", property, sex, category, paddock, alms,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"RFID":true, "properties.Management":true, "_id":true}')

  cattleinfo <- cattle$find(query = filter, fields = lookfor)

  if(nrow(cattleinfo) == 0){
  weights3 <- setNames(data.frame(matrix(ncol = length(dates)+2, nrow = 0)), c("RFID", "Tag", format(as.Date(dates, tz = timezone, format = "%Y-%m-%d"), "%d %b %Y")))}else{

  # This bit of code unlists dataframes within the dataframe

  for(i in 1:ncol(cattleinfo)){
    class <- class(cattleinfo[,i])
    if(class == "data.frame"){
      cattleinfo <- cbind(cattleinfo, cattleinfo[,i])
      cattleinfo <- cattleinfo[,-i]}
  }

  ids <- paste(unlist(cattleinfo$`_id`), collapse = '", "' )
  filter1 <- sprintf('{"cattle_id":{"$in":["%s"]}}', ids)

  lookfor1 <- sprintf('{"_id":false}')

  weights <- dailywts$find(query = filter1 , fields = lookfor1)

  if(nrow(weights) == 0){weights3 <- setNames(data.frame(matrix(ncol = length(dates)+2, nrow = 0)), c("RFID", "Tag", format(as.Date(dates, tz = timezone, format = "%Y-%m-%d"), "%d %b %Y")))}else{

  attributes(weights[,3])$tzone <- timezone

  weights1 <- weights %>%
              filter(Wt != 0) %>%
              mutate (date = as.Date(datetime, tz = timezone))%>%
              filter(date >= start) %>%
              group_by(cattle_id, date) %>%
              summarise(meanwt = round(mean(Wt),0))

  #weights1 <- left_join(weights1, cattleinfo, by = c("cattle_id" = "_id"))

  weights2 <- left_join(weights1, cattleinfo, by = c("cattle_id" = "_id"))%>%
              spread(key = date, value = meanwt) %>%
              ungroup()

  missingcattle <- cattleinfo$`_id`[!(cattleinfo$`_id` %in% weights2$cattle_id)]

  toadd <- setNames(data.frame(matrix(ncol = length(colnames(weights2)), nrow = length(missingcattle))), colnames(weights2))%>%
           mutate(cattle_id = missingcattle, RFID = cattleinfo$RFID[cattleinfo$`_id` %in% missingcattle], Management = cattleinfo$Management[cattleinfo$`_id` %in% missingcattle])

  weights2 <- rbind(weights2, toadd)%>%
              select(-cattle_id)

  collist <- colnames(weights2)

  missingdates <- dates[!(dates %in% collist)]

  if(length(missingdates) != 0){
    weights2 <- cbind(weights2, setNames(lapply(missingdates, function(x) x=NA), missingdates))
  }

  weights3 <- weights2 %>%
              select(1, 2, order(as.Date(names(weights2[c(-1, -2)]), tz = timezone))+2)

  colnames(weights3) <- c("RFID", "Tag", format(as.Date(colnames(weights3[c(-1, -2)]), tz = timezone, format = "%Y-%m-%d"), "%d %b %Y"))
  }}

  return(weights3)

}
