#' Read a datafile from the DataMuster database
#'
#' This function reads cattle data from a file and prepares the data for table display on the DataMuster website
#' @name appreaddata
#' @param property the name of the property of the registered user
#' @param filepath the path of the file to be read, determined by navigation by the user to the file after selecting the 'Browse' button
#' @param filetype the type of file to be read, "DataMuster WoW (.txt)", "DataMuster EID (.txt)", "Gallagher TSi Session Data (.csv)" or "Calving Data (.csv)"
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe containing the data from the file
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import jsonlite
#' @export


appreaddata <- function(property, filepath, filetype, username, password){

  properrormessage <- "Sorry, this feature is not enabled for your property. Please contact the team at info@datamuster.net.au if you would like to be able to upload calving data into the DataMuster database."

  fileerrormessage <- "There was an error reading this file. Please ensure that the correct File Type is selected, that the file contains complete data and that there are no errors particularly in the date formatting. Please contact the team at info@datamuster.net.au if you have any further difficulties."

  if(filetype == "DataMuster WoW (.txt)"){

    df <- try(read.csv(filepath, sep = ";", header = F, stringsAsFactors = F, quote = ""), silent = TRUE)

    df <- lapply(df$V1, function(x) fromJSON(x))

    df <- bind_rows(df)

    if(nrow(df) == 0 || ncol(df) != 4){df <- fileerrormessage}else{

    df <- df%>%
          mutate(RFID = as.character(RFID),
                 RFID = ifelse(is.na(RFID), "", RFID))%>%
          rename(Weight = "Wt", Datetime = "datetime", ALMS = "Location")}
    }

  # if(filetype == "DataMuster EID (.txt)"){
  #
  #   df <- try(read.csv(filepath, header=FALSE, strip.white=TRUE, stringsAsFactors = F), silent = TRUE)
  #
  #   if(nrow(df) == 0 || ncol(df) != 3){df <- fileerrormessage}else{
  #
  #   df <- df%>%
  #         mutate(V1 = as.character(V1),
  #                V1 = ifelse(is.na(V1), "", V1))%>%
  #         rename(RFID = "V1", Datetime = "V2", ALMS = "V3")}
  #   }

  if(filetype == "Gallagher TSi Session Data (.csv)"){

    df <- try(read.csv(filepath, header=TRUE, stringsAsFactors = F, check.names=FALSE), silent = TRUE)

    if(nrow(df) == 0 || !("Last Seen Date" %in% colnames(df))){df <- fileerrormessage}else{

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
