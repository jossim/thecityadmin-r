#' The City users
#' 
#' @param object an instance of TheCity class
#' @param value a data frame of users
#' @return a data frame of users
#' @export
setGeneric("users", function(object) standardGeneric("users"))
setMethod("users", "TheCity", function(object) object@env$users)

#' @rdname users
setGeneric("users<-", function(object, value) standardGeneric("users<-"))
setMethod("users<-", "TheCity",
          function(object, value) {
              object@env$users <- value
              if(validObject(object))
                  return(object)
          }
)

#' Gets users via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of users to request. Default: all
#' @param sleep if TRUE a sleep time will be inserted between page requests. 
#' Default: TRUE
#' @export
setGeneric("fetch.users", 
           function(object, start = 1, total = "all", sleep = TRUE) {
               standardGeneric("fetch.users")
           }
)
setMethod("fetch.users", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = users(object)
              new.df = request.iterator(object, "users", start = start, 
                                        total = total, df = df, sleep = sleep)
              users(object) = new.df
          }
)

#' Saves users to an RDS file
#' 
#' @param object an instance of TheCity class
#' @param file the file path to save the data at
#' @export
setGeneric("save.users", function(object, file) standardGeneric("save.users"))
setMethod("save.users", "TheCity", 
          function(object, file) saveRDS(users(object), file)
)