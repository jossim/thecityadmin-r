#' Church campuses
#' 
#' @param object an instance of TheCity class
#' @param value a data frame of campuses
#' @return a data frame of campuses
#' @export
setGeneric("campuses", function(object) standardGeneric("campuses"))
setMethod("campuses", "TheCity", function(object) object@env$campuses)

#' @rdname campuses
setGeneric("campuses<-", function(object, value) standardGeneric("campuses<-"))
setMethod("campuses<-", "TheCity",
          function(object, value) {
              object@env$campuses <- value
              if(validObject(object))
                  return(object)
          }
)

#' Gets church campuses via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of campuses to request. Default: all
#' @param sleep if TRUE a sleep time will be inserted between page requests. 
#' Default: TRUE
#' @export
setGeneric("fetch.campuses", 
           function(object, start = 1, total = "all", sleep = TRUE) 
               standardGeneric("fetch.campuses"))
setMethod("fetch.campuses", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = campuses(object)
              new.df = request.iterator(object, "campuses", start = start, 
                                        total = total, df = df, sleep = sleep)
              campuses(object) = new.df
          }
)
