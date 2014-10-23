#' User skills
#' 
#' @param object an instance of TheCity object
#' @param value a data frame of skills
#' @return a data frame of skills
#' @export
setGeneric("skills", function(object) standardGeneric("skills"))
setMethod("skills", "TheCity", function(object) object@env$skills)

#' @rdname skills
setGeneric("skills<-", function(object, value) standardGeneric("skills<-"))
setMethod("skills<-", "TheCity",
          function(object, value) {
              object@env$skills <- value
              if(validObject(object))
                  return(object)
          }
)

#' Gets user skills via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of skills to request. Default: all
#' @param sleep if TRUE a sleep time will be inserted between page requests. 
#' Default: TRUE
#' @export
setGeneric("fetch.skills", function(object,...) standardGeneric("fetch.skills"))
setMethod("fetch.skills", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = skills(object)
              new.df = request.iterator(object, "skills", start = start, 
                                        total = total, df = df, sleep = sleep)
              skills(object) = new.df
          }
)
