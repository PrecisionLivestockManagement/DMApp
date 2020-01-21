#' Retrieve cattle information via the DataMuster website app
#'
#' This function provides a flexible search tool to find cattle based on a flexible number of search terms. It also allows the user to define what fields should be returned via a list of fields.
#' @name get_cattle
#' @param property property to search for
#' @param sex male or female
#' @param zoom whole property or paddock zoomchoice
#' @param paddock paddock on property
#' @param category the class of cattle either breeding or growing
#' @param fields a list of MongoDB cattle collection headers that you want returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle with the list of fields defined in the inputs and searched using the search terms.
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_cattle <- function(property, sex, category, zoom, paddock, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  property <- sprintf('"stationname":"%s",', property)
  if(sex == "all"){sex <- NULL} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  if(category == "all"){category <- NULL} else {category <- sprintf('"properties.category":"%s",', category)}
  if(is.null(paddock)||zoom == 1){paddock <- NULL}else{paddock <- sprintf('"properties.Paddock":"%s",', paddock)}

  if(is.null(fields)){fields <- c("RFID", "properties.Management", "properties.sex", "properties.category", "properties.Paddock")}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

# Set up find query

cattlesearch <-paste0("{", property, sex, paddock, category,"}")
if(nchar(cattlesearch)==2){}else{
cattlesearch <- substr(cattlesearch, 1 , nchar(cattlesearch)-2)
cattlesearch <- paste0(cattlesearch, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

cattledata <- cattle$find(query = cattlesearch, fields = snappy)

if(nrow(cattledata) != 0){
cattledataf <- cbind(cattledata[-1], cattledata$properties)%>%
               rename(Tag = Management, Sex = sex, Category = category)%>%
               select(RFID, Tag, Sex, Category, Paddock)}

if(!exists("cattledataf") | exists("cattledataf") && nrow(cattledataf) == 0){
cattledataf <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("RFID", "Tag", "Sex", "Category", "Paddock"))}

cattledataf

}

