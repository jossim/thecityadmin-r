setGeneric("groups", function(object) standardGeneric("groups"))
setMethod("groups", "TheCity", function(object) object@env$groups)

setGeneric("groups<-", function(object, value) standardGeneric("groups<-"))
setMethod("groups<-", "TheCity",
          function(object, value) {
              object@env$groups <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("fetch.groups", function(object,...) standardGeneric("fetch.groups"))
setMethod("fetch.groups", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = groups(object)
              new.df = request.iterator(object, "groups", start = start, 
                                        total = total, df = df, sleep = sleep)
              groups(object) = new.df
          }
)
