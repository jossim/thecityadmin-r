#' User & group addresses
#' 
#' @param object an instance of TheCity class
#' @param value a data frame of addresses
#' @return a data frame of addresses
#' @export
setGeneric("addresses", function(object) standardGeneric("addresses"))
setMethod("addresses", "TheCity", function(object) object@env$addresses)

#' @rdname addresses
setGeneric("addresses<-", function(object, value) standardGeneric("addresses<-"))
setMethod("addresses<-", "TheCity",
          function(object, value) {
              object@env$addresses <- value
              if(validObject(object))
                  return(object)
          }
)

#' Gets addresses via The City API
#' 
#' @param object an instance of TheCity class
#' @param start the page to start the request on. Default: 1.
#' @param total the number of pages of addresses to request. Default: all
#' @export
setGeneric("fetch.addresses", 
           function(object, start, total) { 
               standardGeneric("fetch.addresses")
           }
)
setMethod("fetch.addresses", "TheCity",
          function(object, start = 1, total = "all") {
              df = addresses(object)
              new.df = request.iterator(object, "addresses", start = start, 
                                        total = total, df = df)
              addresses(object) = new.df
          }
)

#' Saves addresses to an RDS file
#' 
#' @param object an instance of TheCity class
#' @param file the file path to save the data at.
#' @export
setGeneric("save.addresses", 
           function(object, file) {
               standardGeneric("save.addresses")
           }
)
setMethod("save.addresses", "TheCity", 
          function(object, file) saveRDS(addresses(object), file)
)