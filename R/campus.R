setGeneric("campuses", function(object) standardGeneric("campuses"))
setMethod("campuses", "TheCity", function(object) object@env$campuses)

setGeneric("campuses<-", function(object, value) standardGeneric("campuses<-"))
setMethod("campuses<-", "TheCity",
          function(object, value) {
              object@env$campuses <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("fetch.campuses", function(object,...) standardGeneric("fetch.campuses"))
setMethod("fetch.campuses", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = campuses(object)
              new.df = request.iterator(object, "campuses", start = start, 
                                        total = total, df = df, sleep = sleep)
              campuses(object) = new.df
          }
)
