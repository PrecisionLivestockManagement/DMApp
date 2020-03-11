#' Retrieves paddock information from the DataMuster database
#'
#' This function allows a list of paddock polygons to be retreived from the DataMuster database via the DataMuster website
#' @name appreaddata
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a spatialpolygonsdataframe with a list of the paddock names and associated polygon coordinates
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @export


appreaddata <- function(property, filepath, filetype, username, password){

  properrormessage <- "Sorry, this feature is not enabled for your property. Please contact the team at info@datamuster.net.au if you would like to be able to upload calving data into the DataMuster database."

  fileerrormessage <- "There was an error reading this file. Please ensure that the correct File Type is selected, that the file contains complete data and that there are no errors particularly in the date formatting. Please contact the team at info@datamuster.net.au if you have any further difficulties."

  if(filetype == "DataMuster WoW (.txt)"){

    df <- try(read.csv(filepath, header=FALSE, strip.white=TRUE, stringsAsFactors = F), silent = TRUE)

    if(nrow(df) == 0 || ncol(df) != 4){df <- fileerrormessage}else{

    df <- df%>%
          mutate(V1 = as.character(V1),
                 V1 = ifelse(is.na(V1), "", V1))%>%
          rename(RFID = "V1", Weight = "V2", Datetime = "V3", ALMS = "V4")}
    }

  if(filetype == "DataMuster EID (.txt)"){

    df <- try(read.csv(filepath, header=FALSE, strip.white=TRUE, stringsAsFactors = F), silent = TRUE)

    if(nrow(df) == 0 || ncol(df) != 3){df <- fileerrormessage}else{

    df <- df%>%
          mutate(V1 = as.character(V1),
                 V1 = ifelse(is.na(V1), "", V1))%>%
          rename(RFID = "V1", Datetime = "V2", ALMS = "V3")}
    }

  if(filetype == "Gallagher TSi Session Data (.csv)"){

    df <- try(read.csv(filepath, header=TRUE, stringsAsFactors = F, check.names=FALSE), silent = TRUE)

    if(nrow(df) == 0 || ncol(df) >= 8){df <- fileerrormessage}else{

    datepattern <- ifelse(substr(df$`Last Seen Date`[1],3,3) == "/", "%d/%m/%Y", "%Y/%m/%d")

    df <- df %>%
          mutate(`Live Weight (kg)` = as.character(`Live Weight (kg)`),
                 `Last Seen Date` = format(as.Date(`Last Seen Date`, datepattern), "%d/%m/%Y"))%>%
          replace(., is.na(.), "")}
  }

  if(filetype == "Calving Data (.csv)"){

    if(property != "Belmont"){df <- properrormessage}else{

    df <- try(read.csv(filepath, header=TRUE, stringsAsFactors = F, check.names=FALSE), silent = TRUE)

        if(nrow(df) == 0 || ncol(df) != 11){df <- fileerrormessage}else{

          df <- df%>%
          replace(., is.na(.), "")

    names(df) <- gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", names(df), perl = TRUE)} # Converts df column names to uppercase first letter
  }}

  return(df)

}
