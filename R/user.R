#' The City users
#' 
#' @param object an instance of TheCity class
#' @param value a data frame of users
#' @return a data frame of users
#' @export
setGeneric("users", function(object) standardGeneric("users"))
setMethod("users", "TheCity", function(object) object@env$users)

#' @rdname users
setGeneric("users<-", function(object, value) standardGeneric("users<-"))
setMethod("users<-", "TheCity",
          function(object, value) {
              object@env$users <- value
              if(validObject(object))
                  return(object)
          }
)

#' A user on The City
#' 
#' Looks in TheCity object for the user. If not found, looks to the API & adds
#' the user to the stored users in TheCity object.
#' 
#' @param object an instance of TheCity class
#' @param usid the user id of the given user
#' @return a data frame of the user
#' @importFrom plyr rbind.fill
#' @export
setGeneric("user", function(object, usid) standardGeneric("user"))
setMethod("user", "TheCity",
          function(object, usid) {
              user = data.frame()
              users = users(object)
              
              if(nrow(users) > 0)
                  user = subset(users, users$id == usid)
              
              if(nrow(user) > 0)
                  return(user)
              else {
                  path = paste("users", usid, sep = "/")
                  req = request.error.handler(object, 
                                               request(object, path = path))
                  user = content(req)
                  user = lapply(user, function(ele) replace.empty(ele))
                  user = as.data.frame(user)
                  users(object) = rbind.fill(users, user)
                  return(user)
              }
          }
)

#' Gets users via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of users to request. Default: all
#' @param sleep if TRUE a sleep time will be inserted between page requests. 
#' Default: TRUE
#' @export
setGeneric("fetch.users", 
           function(object, start = 1, total = "all", sleep = TRUE) {
               standardGeneric("fetch.users")
           }
)
setMethod("fetch.users", "TheCity",
          function(object, start = 1, total = "all", sleep = TRUE) {
              df = users(object)
              new.df = request.iterator(object, "users", start = start, 
                                        total = total, df = df, sleep = sleep)
              users(object) = new.df
          }
)

#' Saves users to an RDS file
#' 
#' @param object an instance of TheCity class
#' @param file the file path to save the data at
#' @export
setGeneric("save.users", function(object, file) standardGeneric("save.users"))
setMethod("save.users", "TheCity", 
          function(object, file) saveRDS(users(object), file)
)