#' Event checkins
#' 
#' @param object an instance of TheCity class
#' @param value a data frame of event checkins
#' @return a data frame of event checkins
#' @export
setGeneric("checkins", function(object) standardGeneric("checkins"))
setMethod("checkins", "TheCity", function(object) object@env$checkins)

#' @rdname checkins
setGeneric("checkins<-", function(object, value) standardGeneric("checkins<-"))
setMethod("checkins<-", "TheCity",
          function(object, value) {
              object@env$checkins <- value
              if(validObject(object))
                  return(object)
          }
)

#' Gets event checkins via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of event checkins to request. Default: all
#' @param sleep if TRUE a sleep time will be inserted between page requests. 
#' Default: TRUE
#' @param param a named vector of query parameters.
#' @export
setGeneric("fetch.checkins", 
           function(object, start = 1, total = "all", sleep = TRUE, 
                    params = c()) { 
               standardGeneric("fetch.checkins")
           }
)
setMethod("fetch.checkins", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE, 
                   params = c()) {
              df = checkins(object)
              new.df = request.iterator(object, "checkins", start = start, 
                                        total = total, df = df, params = params,
                                        sleep = sleep)
              checkins(object) = new.df
          }
)

#' Saves event checkins to an RDS file
#' 
#' @param object an instance of TheCity class
#' @param file the file path to save the data at
#' @export
setGeneric("save.checkins", 
           function(object, file) standardGeneric("save.checkins"))
setMethod("save.checkins", "TheCity", 
          function(object, file) saveRDS(checkins(object), file)
)
