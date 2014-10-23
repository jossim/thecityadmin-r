setGeneric("checkins", function(object) standardGeneric("checkins"))
setMethod("checkins", "TheCity", function(object) object@env$checkins)

setGeneric("checkins<-", function(object, value) standardGeneric("checkins<-"))
setMethod("checkins<-", "TheCity",
          function(object, value) {
              object@env$checkins <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("fetch.checkins", function(object,...) standardGeneric("fetch.checkins"))
setMethod("fetch.checkins", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = checkins(object)
              new.df = request.iterator(object, "checkins", start = start, 
                                        total = total, df = df, sleep = sleep)
              checkins(object) = new.df
          }
)

setGeneric("save.checkins", function(object, file) standardGeneric("save.checkins"))
setMethod("save.checkins", "TheCity", 
          function(object, file) saveRDS(checkins(object), file)
)
