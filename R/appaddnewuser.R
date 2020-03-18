#' Adds a new user to the DataMuster database
#'
#' This function adds a new user to the DataMuster database via the DataMuster website
#' @name appaddnewuser
#' @param email the email address of the user
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a message that indicates whether or not the user has been added successfully
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


appaddnewuser <- function(email, username, password){

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    newusers <- mongo(collection = "NewUsers", db = "DataMuster", url = pass, verbose = T)

    template <- newusers$find(query = '{"loginemail":"info@datamuster.net.au"}', fields = '{"_id":false}')

    template$loginemail <- email
    template$createdAt <- Sys.time()

    rownames(template)<-c()

    newusers$insert(template)

}
