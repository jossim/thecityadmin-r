setGeneric("skills", function(object) standardGeneric("skills"))
setMethod("skills", "TheCity", function(object) object@env$skills)

setGeneric("skills<-", function(object, value) standardGeneric("skills<-"))
setMethod("skills<-", "TheCity",
          function(object, value) {
              object@env$skills <- value
              if(validObject(object))
                  return(object)
          }
)

setGeneric("fetch.skills", function(object,...) standardGeneric("fetch.skills"))
setMethod("fetch.skills", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = skills(object)
              new.df = request.iterator(object, "skills", start = start, 
                                        total = total, df = df, sleep = sleep)
              skills(object) = new.df
          }
)
