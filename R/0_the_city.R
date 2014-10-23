#' An S4 class that defines an API connection to The City & stores data from The
#' City
#' 
#' @slot key the Secret Key
#' @slot token the User Token
#' @slot max.sleep.time the maximum sleep time between API request in minutes. Default 1.25 minutes.
#' @slot env the environment where data from The City will be stored.
#' @export TheCity
#' @exportClass TheCity
TheCity <- setClass("TheCity",
                    slots = list(key = "character", 
                                 token = "character",
                                 max.sleep.time = "numeric",
                                 env = "environment"
                                 )
                    )

#' @export
setMethod("initialize", "TheCity",
          function(.Object, key, token, max.sleep.time, env = new.env()) {
              .Object@key = key
              .Object@token = token
              .Object@max.sleep.time = 1.25
              .Object@env$last.request.time = Sys.time()
              .Object@env$request = list(path = "", query = "", host = "", 
                                         verb = "")
              .Object@env$rate.limit = 10000
              .Object@env$rate.limit.remaining = 10000
              .Object@env$users = data.frame()
              .Object@env$groups = data.frame()
              .Object@env$addresses = data.frame()
              .Object@env$campuses = data.frame()
              .Object@env$checkins = data.frame()
              .Object@env$roles = data.frame()
              .Object@env$skills = data.frame()
              return(.Object)
          }
)

#' The Secret Key, used during an API request
#' 
#' @param object an instance of TheCity class
#' @return the secret key
#' @param value the new value of the secret key
#' @export
setGeneric("key", function(object) standardGeneric("key"))
setMethod("key", "TheCity", function(object) object@key)

#' @rdname key
setGeneric("key<-", function(object, value) standardGeneric("key<-"))
setMethod("key<-", "TheCity",
          function(object, value) {
              object@key <- value
              if(validObject(object))
                  return(object)
          }
)

#' The User Token, used during an API request
#' 
#' @param object an instance of TheCity class
#' @param value the new value of the user token
#' @return the user token
#' @export
setGeneric("token", function(object) standardGeneric("token"))
setMethod("token", "TheCity", function(object) object@token)

#' @rdname token
setGeneric("token<-", function(object, value) standardGeneric("token<-"))
setMethod("token<-", "TheCity", 
          function(object, value) {
              object@token <- value
              if(validObject(object))
                  return(object)
          }
)

#' Maximum sleep time between API requests
#' 
#' @param object an instance of TheCity class
#' @param value the new value of the sleep time in minutes
#' @return the maximum sleep time, in minutes
#' @export
setGeneric("max.sleep.time", function(object) standardGeneric("max.sleep.time"))
setMethod("max.sleep.time", "TheCity", function(object) object@max.sleep.time)

#' @rdname max.sleep.time
setGeneric("max.sleep.time<-", 
           function(object, value) standardGeneric("max.sleep.time<-"))
setMethod("max.sleep.time<-", "TheCity",
          function(object, value) {
              object@max.sleep.time <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("last.request.time", function(object) standardGeneric("last.request.time"))
setMethod("last.request.time", "TheCity", function(object) object@env$last.request.time)

setGeneric("last.request.time<-", 
           function(object, value) standardGeneric("last.request.time<-"))
setMethod("last.request.time<-", "TheCity",
          function(object, value) {
              object@env$last.request.time <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("rate.limit", function(object) standardGeneric("rate.limit"))
setMethod("rate.limit", "TheCity", function(object) object@env$rate.limit)

setGeneric("rate.limit<-", 
           function(object, value) standardGeneric("rate.limit<-"))
setMethod("rate.limit<-", "TheCity",
          function(object, value) {
              object@env$rate.limit <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("rate.limit.remaining", 
           function(object) standardGeneric("rate.limit.remaining"))
setMethod("rate.limit.remaining", "TheCity", 
          function(object) object@env$rate.limit.remaining)

setGeneric("rate.limit.remaining<-", 
           function(object, value) standardGeneric("rate.limit.remaining<-"))
setMethod("rate.limit.remaining<-", "TheCity",
          function(object, value) {
              object@env$rate.limit.remaining <- value
              if(validObject(object))
                  return(object)
          }
)

setValidity("TheCity", function(object) {
    msg = NULL
    valid = TRUE
    
    if (!nchar(key(object)) > 0) {
        valid = FALSE
        msg = c(msg, "Must have a secret key")
    }

    if (!nchar(token(object)) > 0) {
        valid = FALSE
        msg = c(msg, "Must have a user token")
    }
})