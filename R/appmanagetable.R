#' Retrieves data from the DataMuster database for the Manage Cattle table
#'
#' This function retreives cattle data from the DataMuster database and prepares the data for table display on the DataMuster website
#' @name appmanagetable
#' @param property the name of the property to search the database
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param paddock the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle RFID numbers, management tags, category and sex information, current paddock allocations and the latest crush weight and date
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appmanagetable <- function(property, sex, category, paddock, zoom, username, password){

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
                     "properties.breed":true, "properties.colour":true, "properties.brand":true, "properties.horn":true,
                     "properties.birthDate":true, "properties.damMTag":true, "properties.sireMTag":true, "properties.rego":true,
                     "properties.weaned":true, "properties.desexed":true, "properties.AE":true, "properties.estcalvingdate":true, "properties.calvingdate":true,
                     "properties.foetalagedate":true, "properties.foetalage":true, "properties.entryDate":true, "properties.PaddockdateIN":true, "properties.PrevPaddock":true,
                     "_id":false}')

  cattleinfo <- cattle$find(query = filter, fields = lookfor)

  if(nrow(cattleinfo) == 0){
    cattleinfo <- cattle$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = lookfor)
  }

  cattleinfo <- cbind(cattleinfo[-1], cattleinfo$properties)

  collist <- colnames(cattleinfo)

    for(i in 1:length(collist)){
    if("POSIXt" %in% class(cattleinfo[,i])){
      attributes(cattleinfo[,i])$tzone <- "Australia/Brisbane"}}

  s <- Sys.time()
  attr(s,"tzone") <- "Australia/Brisbane"

  caseconvert <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

  cattleinfof <- cattleinfo%>%
                 mutate_at(vars(birthDate, entryDate, estcalvingdate, calvingdate, PaddockdateIN, foetalagedate), as.character) %>%
                 replace(.  == "xxxxxx", NA) %>%
                 mutate(sex = caseconvert(sex), category = caseconvert(category), colour = caseconvert(colour), horn = caseconvert(horn),
                        weaned = caseconvert(weaned), desexed = caseconvert(desexed)) %>%
                 mutate(AE = ifelse(AE == "0", NA, AE),
                        estcalvingdate = ifelse(estcalvingdate < paste0(substr(Sys.Date(),1,4), "-05-01"), NA, estcalvingdate)
                        ) %>%
                 #replace(estcalvingdate  < as.Date(paste0(substr(Sys.Date(),1,4), "-05-01")), NA) %>%
                 mutate_at(vars(birthDate, entryDate, estcalvingdate, calvingdate, PaddockdateIN, foetalagedate), as.Date) %>%
                 filter(RFID != "xxxxxx") %>%
                 replace(.  == "1970-01-01", NA) %>%
                 select(RFID, Management, sex, category, Paddock, breed, colour, brand, horn, birthDate, damMTag, sireMTag, rego, weaned, desexed,
                        AE, estcalvingdate, calvingdate, foetalagedate, foetalage, entryDate, PaddockdateIN, PrevPaddock) %>%
                 rename("Tag" = Management, "Sex" = sex, "Category" = category, "Breed" = breed, "Colour" = colour, "Brand" = brand, "Horn status" = horn,
                        "Date of birth" = birthDate, "Dam tag" = damMTag, "Sire tag" = sireMTag, "Registration" = rego, "Weaned" = weaned,
                        "Desexed" = desexed, "AE rating" = AE, "Est. calving date" = estcalvingdate,
                        "Previous calving date" = calvingdate, "Foetal age date" = foetalagedate, "Foetal age" = foetalage, "Farm entry date" = entryDate,
                        "Paddock entry date" = PaddockdateIN, "Previous paddock" = PrevPaddock)

  return(cattleinfof)

}
