setGeneric("users", function(object) standardGeneric("users"))
setMethod("users", "TheCity", function(object) object@env$users)

setGeneric("users<-", function(object, value) standardGeneric("users<-"))
setMethod("users<-", "TheCity",
          function(object, value) {
              object@env$users <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("fetch.users", function(object,...) standardGeneric("fetch.users"))
setMethod("fetch.users", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = users(object)
              new.df = request.iterator(object, "users", start = start, 
                                        total = total, df = df, sleep = sleep)
              users(object) = new.df
          }
)

setGeneric("save.users", function(object, file) standardGeneric("save.users"))
setMethod("save.users", "TheCity", 
          function(object, file) saveRDS(users(object), file)
)