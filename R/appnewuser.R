#' Add a new user to the DataMuster database
#'
#' This function allows new users to be added to the DataMuster database via the DataMuster website
#' @name appnewuser
#' @param email the email address of the user
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the data has been successfully added
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


appnewuser <- function(email, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    users <- mongo(collection = "NewUsers", db = "DataMuster", url = pass, verbose = T)

    template <- users$find(query = '{"loginemail":"info@datamuster.net.au"}', fields = '{"_id":false}')

    template$loginemail <- email
    template$createdAt <- Sys.time()

    rownames(template)<-c()

    users$insert(template)

}
