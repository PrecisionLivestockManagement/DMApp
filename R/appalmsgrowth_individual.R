#' Retrieves data for the ALMS Growth graph from the DataMuster database
#'
#' This function retreives individual weekly weight data from the DataMuster database and prepares the data for graphical display on the DataMuster website
#' @name appalmsgrowth_individual
#' @param property the name of the property to search the database
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param alms the ALMS allocation of the cattle to be returned, determined by selecting an ALMS from the drop down menu
#' @param start the minimum date of data to be returned, determined by the "Period for ALMS graphs" filter
#' @param timezone the timezone of the property to display the weekly weight data
#' @param cattleprop the minimum number of cattle required, as a percentage
#' @param identification the selected identification (RFID or Tag)
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return A dataframe of summarised data showing the average weight of cattle by date and the number of cattle included in the analysis
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import tidyr
#' @import highcharter
#' @import purrr
#' @export


appalmsgrowth_individual <- function(RFID, property, sex, category, alms, zoom, start, timezone, cattleprop, identification, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  weeklywts <- mongo(collection = "WeeklyWts", db = "DataMuster", url = pass, verbose = T)

  mobdata <- appalmsgrowth(property = property, sex = sex, category = category, alms = alms, zoom = zoom, start = start, timezone = timezone,
                             cattleprop = cattleprop, username = username, password = password)

  mob_dates <- mobdata[[1]]$date[which(!is.na(mobdata[[1]]$data))]
  mob_dates <- as.POSIXct(mob_dates, format = "%B %d")

  if(is.null(RFID)){cattleweights4 <- list()}else{

  RFID <- paste(unlist(RFID), collapse = '", "' )
  RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)

  property <- sprintf('"stationname":"%s",', property)
  if(sex == "all"){sex <- NULL} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  if(category == "all"){category <- NULL} else {category <- sprintf('"properties.category":"%s",', category)}
  if(is.null(alms)){alms <- NULL}else{alms <- sprintf('"properties.ALMSasset_id":"%s",', alms)}

  dates <- seq(as.Date(paste0(start)), as.Date(paste0(Sys.Date())), by = "day")

  if(substr(timezone,1,9) == "Australia"){
  weighdays <- dates[weekdays(dates) == "Sunday"]}else{
    if(timezone == "America/Argentina/Buenos_Aires"){
      weighdays <- dates[weekdays(dates) == "Saturday"]}}

  # Set up query to search for cattle

  filter <- paste0("{", RFID, property, sex, category, alms,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"RFID":true, "properties.Management":true, "_id":true}')

  cattleinfo <- cattle$find(query = filter, fields = lookfor)

  # Set up query to search for weeklywts

  ID <- paste(unlist(cattleinfo$`_id`), collapse = '", "' )
  ID <- sprintf('"cattle_id":{"$in":["%s"]},', ID)
  start <- sprintf('"Date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

  filter <- paste0("{", ID, start,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <-  sprintf('{"cattle_id":true, "avweight":true, "Date":true, "_id":false}')

  weights <- weeklywts$find(query = filter, fields = lookfor)

  # This section of code selects only the cattle included in the mob average trand line
  # ids <- unique(weights$cattle_id)
  #
  # for(i in 1:length(ids)){
  #   dts <- weights$Date[weights$cattle_id == ids[i] & weights$avweight != 0]
  #   if("FALSE" %in% (mob_dates %in% dts))
  #     weights <- weights[weights$cattle_id != ids[i],]}

  # Find cattle that have had a weekly weight each week and summarise the data by week

  if(nrow(weights) == 0){

    cattleweights4 <- list()}else{

      cattleweights2 <- left_join(weights, cattleinfo, by = c("cattle_id" = "_id"))%>%
        mutate(avweight = ifelse(avweight == 0, NA, round(avweight,0)),
               Date = format(as.Date(Date, tz = timezone, format = "%Y-%m-%d"), "%d %b"),
               Management = properties$Management)

      if(identification == "RFID"){
        cattleweights2 <- cattleweights2 %>%
                          rename(ID = RFID)%>%
                          select(Date, avweight, ID)
      }else{
        cattleweights2 <- cattleweights2 %>%
                          rename(ID = Management)%>%
                          select(Date, avweight, ID)
        }


      cattleweights3 <- cattleweights2 %>%
        group_by(name = ID) %>%
        do(data = .$avweight) %>%
        ungroup() %>%
        mutate(lineWidth = 5,
               type = "line",
               color = "rgb(240,90,40)")

      cattleweights4 <- list_parse(cattleweights3)

      # cattleweights5 <- cattleweights4 %>%
      #   group_by(RFID)%>%
      #   do(cattleweights5 = list(
      #     data = list_parse2(data.frame(.$Date, .$avweight)))) %>%
      #   {map2(.$RFID, .$cattleweights5, function(x, y){append(list(name = x), y)})}
    }

  return(cattleweights4)

}}

