setGeneric("addresses", function(object) standardGeneric("addresses"))
setMethod("addresses", "TheCity", function(object) object@env$addresses)

setGeneric("addresses<-", function(object, value) standardGeneric("addresses<-"))
setMethod("addresses<-", "TheCity",
          function(object, value) {
              object@env$addresses <- value
              if(validObject(object))
                  return(object)
          }
)

# Group & user addresses are mixed & they have different attributes, which
# breakes the data frame building.
setGeneric("fetch.addresses", function(object,...) standardGeneric("fetch.addresses"))
setMethod("fetch.addresses", "TheCity",
          function(object, start = 1, total = "all") {
              df = addresses(object)
              new.df = request.iterator(object, "addresses", start = start, 
                                        total = total, df = df)
              addresses(object) = new.df
          }
)

setGeneric("save.addresses", function(object, file) standardGeneric("save.addresses"))
setMethod("save.addresses", "TheCity", 
          function(object, file) saveRDS(addresses(object), file)
)