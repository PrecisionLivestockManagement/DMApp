#' Retrieves data for the ALMS Use graph from the DataMuster database
#'
#' This function retreives daily weight data from the DataMuster database and prepares the data for graphical display on the DataMuster website
#' @name appalmsusegraph
#' @param property the name of the property to search the database
#' @param start the minimum date of data to be returned, determined by the "Period for ALMS graphs" filter
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param alms the allocated alms unit of the cattle to be returned, determined from the "ALMS Access:" drop down selector
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle and a daily visit indicator, 0 = not recorded and 1 = recorded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


appalmsusegraph <- function(property, start, sex, category, alms, zoom, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  almsuse <- mongo(collection = "ALMSUse", db = "DataMuster", url = pass, verbose = T)

  unit <- appgetinfrastructure(property = property, timezone = "Australia/Brisbane", username = username, password = password) # timezone doesn't matter here as it won't be used

  #if(is.null(paddock)){
  # alms <- unit$asset_id
   #alms <- paste(unlist(alms), collapse = '", "' )
   #alms <- sprintf('"ALMS":{"$in":["%s"]},', alms)}else{
   #unit <- unit %>% filter(Paddock == paddock)
   #alms <- sprintf('"ALMS":"%s",', unit$asset_id)}

  if(nrow(unit) == 0){allcattle <- data.frame()}else{

  property <- sprintf('"Property":"%s",', property)
  if(sex == "all"){sex <- NULL} else {sex <- sprintf('"Sex":"%s",', sex)}
  if(category == "all"){category <- NULL} else {category <- sprintf('"Category":"%s",', category)}
  if(is.null(alms)){alms <- NULL}else{alms <- sprintf('"ALMS":"%s",', alms)}
  start <- sprintf('"Date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

  # Set up query to search for data

  filter <- paste0("{", property, sex, category, alms, start,"}")
  filter <- substr(filter, 1 , nchar(filter)-2)
  filter <- paste0(filter, "}")

  lookfor <- sprintf('{"Date":true, "Category":true, "Sex":true, "Count":true, "_id":false}')

  almsinfo <- almsuse$find(query = filter, fields = lookfor)

  if(nrow(almsinfo) == 0){allcattle <- data.frame()}else{

  breeding <- almsinfo%>%
              filter(Category == "breeding")%>%
              group_by(Date, Category)%>%
              summarise(breedingcattlenum = n(), breedingusenum = sum(as.numeric(Count)))%>%
              mutate(breedingpercentuse = round(breedingusenum/breedingcattlenum*100, 0))%>%
              select(-Category)

  growing <- almsinfo%>%
             filter(Category == "growing")%>%
             group_by(Date, Category)%>%
             summarise(growingcattlenum = n(), growingusenum = sum(as.numeric(Count)))%>%
             mutate(growingpercentuse = round(growingusenum/growingcattlenum*100, 0))%>%
             select(-Category)

  allcattle <- full_join(breeding, growing, by = "Date")%>%
               replace(is.na(.), 0)%>%
               mutate(novisitnum = (breedingcattlenum - breedingusenum) + (growingcattlenum - growingusenum),
               novisitpercent = ifelse(breedingcattlenum == 0 | growingcattlenum == 0, 100 - (breedingpercentuse + growingpercentuse), 200 - (breedingpercentuse + growingpercentuse)))%>%
               arrange(Date)

  allcattle <- allcattle%>%
               ungroup()%>%
               mutate(Date = as.character(as.Date(Date, tz = "Australia/Brisbane"), format = "%b %d"))
  }}

  return(allcattle)

}
