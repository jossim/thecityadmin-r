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

setGeneric("fetch.roles", function(object,...) standardGeneric("fetch.roles"))
setMethod("fetch.roles", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = roles(object)
              new.df = request.iterator(object, "roles", start = start, 
                                        total = total, df = df, sleep = sleep)
              roles(object) = new.df
          }
)
