#' Groups
#' 
#' @param object an instance of TheCity class
#' @param value a data frame of groups
#' @return a data frame of groups
#' @export
setGeneric("groups", function(object) standardGeneric("groups"))
setMethod("groups", "TheCity", function(object) object@env$groups)

#' @rdname groups
setGeneric("groups<-", function(object, value) standardGeneric("groups<-"))
setMethod("groups<-", "TheCity",
          function(object, value) {
              object@env$groups <- value
              if(validObject(object))
                  return(object)
          }
)

#' Gets groups via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of groups to request. Default: all
#' @param sleep if TRUE a sleep time will be inserted between page requests. 
#' Default: TRUE
#' @export
setGeneric("fetch.groups", 
           function(object, start = 1, total = "all", sleep = TRUE) {
               standardGeneric("fetch.groups")
           }
)
setMethod("fetch.groups", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = groups(object)
              new.df = request.iterator(object, "groups", start = start, 
                                        total = total, df = df, sleep = sleep)
              groups(object) = new.df
          }
)


setGeneric("group", function(object, grpid, name) standardGeneric("group"))
setMethod("group", "TheCity",
          function(object, grpid = NULL, name = NULL) {
              grp = data.frame()
              grps = groups(city)
              
              if(nrow(grps) > 0)
                  grp = subset(grps, grps$id == grpid)
              
              #browser()
              if(nrow(grp) > 0)
                  return(grp)
              else {
                  grp = request(object, path = paste("groups", grpid, sep = "/"))
              }
              return(grp)
          }
)