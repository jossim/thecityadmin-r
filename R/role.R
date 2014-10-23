#' User roles
#' 
#' @param object an instance of TheCity class
#' @param value a data frame of roles
#' @return a data frame of roles
#' @export
setGeneric("roles", function(object) standardGeneric("roles"))
setMethod("roles", "TheCity", function(object) object@env$roles)

setGeneric("roles<-", function(object, value) standardGeneric("roles<-"))
setMethod("roles<-", "TheCity",
          function(object, value) {
              object@env$roles <- value
              if(validObject(object))
                  return(object)
          }
)

#' Gets user roles via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of roles to request. Default: all
#' @param sleep if TRUE a sleep time will be inserted between page requests. 
#' Default: TRUE
#' @export
setGeneric("fetch.roles", 
           function(object, start = 1, total = "all", sleep = TRUE) {
               standardGeneric("fetch.roles")
           }
)
setMethod("fetch.roles", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = roles(object)
              new.df = request.iterator(object, "roles", start = start, 
                                        total = total, df = df, sleep = sleep)
              roles(object) = new.df
          }
)
