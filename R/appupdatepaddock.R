#' Update cattle allocations to paddocks
#'
#' This function allows individuals or groups of cattle to be allocated to paddocks in the DataMuster database via the DataMuster website
#' @name appupdatepaddock
#' @param RFID a list of cattle RFID numbers
#' @param MTag a list of cattle management tag numbers
#' @param property the name of the property that the cattle belong
#' @param paddock the name of the paddock that the cattle belong
#' @param date the date that the cattle were moved in date format, the default is today's date
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


appupdatepaddock <- function(RFID, MTag, property, paddock, date, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
  infs <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.POSIXct(date)}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}
  if (length(paddock) == 1){paddock <- rep(paddock, length = length(RFID))}


  # Check that the paddocks exist in the database
#
   checkpads <- paste(unlist(paddock), collapse = '", "' )

   filterpaddock <- sprintf('{"stationname":"%s", "paddname":{"$in":["%s"]}}', property, checkpads)

   pad <- paddocks$find(query = filterpaddock, fields = '{"_id":true, "geometry":true, "paddname":true, "properties.hectares":true}')

  # Check for WoW infrastructure in new paddocks

  filterinfs <- sprintf('{"stationname":"%s", "paddock":{"$in":["%s"]}, "properties.type":"%s"}', property, checkpads, "Walk-over-Weighing Unit")

  inf <- infs$find(query = filterinfs, fields = '{"_id":true, "paddock":true, "properties.asset_id":true, "properties.datarecording":true}')

  for (i in 1:length(RFID)){

    if (RFID[i] != "xxx xxxxxxxxxxxx"){

    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{

    RFIDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

          banger <- cattle$find(query= RFIDS, fields='{"properties.Paddock":true,"properties.ALMS":true, "properties.ALMSID":true,"properties.ALMSasset_id":true,"pdkhist.dateIN":true, "pdkhist.dateOUT":true, "almshist.dateON":true, "almshist.dateOFF":true, "_id":false}')
          arrpos <- length(banger$pdkhist$dateIN[[1]])
          arrpos1 <- length(banger$pdkhist$dateOUT[[1]])

          prevpaddock <- banger$properties$Paddock

      # If current paddock is different to new padock, continue

          if (banger$properties$Paddock != paddock[i]){

        # Update paddock

          temppad <- pad[which(pad$paddname == paddock[i]),]

          RFIDIlast <- sprintf('{"$set":{"properties.PaddockdateIN":{"$date":"%s"}, "properties.Paddock":"%s", "properties.PaddockID":"%s", "properties.PrevPaddock":"%s"}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"), paddock[i], temppad$`_id`, prevpaddock)
          RFIDI <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}, "pdkhist.dateIN.%s":{"$date":"%s"}, "pdkhist.name.%s":"%s", "pdkhist.ID.%s":"%s"}}', arrpos1, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, paddock[i], arrpos, temppad$`_id`)

      cattle$update(RFIDS, RFIDI)
      cattle$update(RFIDS, RFIDIlast)

        # Does the new paddock have an ALMS? TRUE or FALSE

          ALMS <- paddock[i] %in% inf$paddock[[1]]

          arrpos2 <- length(banger$almshist$dateON[[1]])
          arrpos3 <- length(banger$almshist$dateOFF[[1]])

          # If the new paddock does not have an ALMS or if the ALMS is not active and the animal is currently allocated to an ALMS unit, remove the animal from that unit..

          if (ALMS == "FALSE" ||
              ALMS == "TRUE" & inf$properties$datarecording == "FALSE"){

             if (banger$properties$ALMS == "TRUE"){

              IDI <- sprintf('{"$set":{"almshist.dateOFF.%s":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                             arrpos3, paste0(substr(date[i],1,10),"T","00:00:00","+1000"),"FALSE", "xxxxxx", "xxxxxx")

              cattle$update(RFIDS, IDI)
              }}

          # If the new paddock does have an active ALMS unit..

          if (ALMS == "TRUE" && inf$properties$datarecording == "TRUE"){

            WOW <- inf[which(paddock[i] %in% inf$paddock[[1]]),]

            IDIlast <- sprintf('{"$set":{"properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                             "TRUE", WOW$`_id`, WOW$properties$asset_id)

            # If the animal is currently allocated to a different ALMS unit...

            if (banger$properties$ALMS == "TRUE" && WOW$properties$asset_id != banger$properties$ALMSasset_id){

              IDI <- sprintf('{"$set":{"almshist.dateON.%s":{"$date":"%s"}, "almshist.dateOFF.%s":{"$date":"%s"}, "almshist.ID.%s":"%s", "almshist.asset_id.%s":"%s"}}',
                             arrpos2, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos3, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos2, WOW$`_id`, arrpos2, WOW$properties$asset_id)

              cattle$update(RFIDS, IDIlast)
              cattle$update(RFIDS, IDI)}

            # If the animal is not currently allocated to an ALMS unit...

            if (banger$properties$ALMS == "FALSE"){

              IDI <- sprintf('{"$set":{"almshist.dateON.%s":{"$date":"%s"}, "almshist.ID.%s":"%s", "almshist.asset_id.%s":"%s"}}',
                             arrpos2, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos2, WOW$`_id`, arrpos2, WOW$properties$asset_id)

          cattle$update(RFIDS, IDIlast)
          cattle$update(RFIDS, IDI)}
          }
          }

          }

appmovecattle(property = property, paddock = unique(paddock), username = username, password = password)

}



