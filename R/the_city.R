TheCity <- setClass("TheCity",
                    slots = list(key = "character", 
                                 token = "character",
                                 rate_limit = "numeric",
                                 rate_limit_remaining = "numeric",
                                 max_sleep_time = "numeric",
                                 users = "data.frame",
                                 groups = "data.frame",
                                 campuses = "data.frame")
                    )

setGeneric("token", function(object) standardGeneric("token"))
setMethod("token", "TheCity", function(object) object@token)

setGeneric("token<-", function(object, value) standardGeneric("token<-"))
setMethod("token<-", "TheCity", 
          function(object, value) {
              object@token <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("key", function(object) standardGeneric("key"))
setMethod("key", "TheCity", function(object) object@key)

setGeneric("key<-", function(object, value, ...) standardGeneric("key<-"))
setMethod("key<-", "TheCity",
          function(object, value) {
              object@key <- key
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