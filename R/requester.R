library(digest)
library(RCurl)
library(httr)

setGeneric("hmac_signature", function(object, ...) standardGeneric("hmac_signature"))
setMethod("hmac_signature", "TheCity",
          function(object,
                   path = "",
                   query = "",
                   verb = "GET",
                   host = "https://api.onthecity.org",
                   time = as.numeric(Sys.time()), ...) {
              
              ctime = as.character(time)
              ltime = strsplit(ctime, split = ".", fixed = TRUE)[[1]]
              ctime = ltime[1]
                
              string_to_sign = paste(ctime, verb, host, "/", path, query, sep = "")

              hm = hmac(key = key(object), object = string_to_sign, 
                        algo = "sha256", raw = T)
                
              curlEscape(base64(hm)[1])
          }
)

setGeneric("request", function(object, ...) standardGeneric("request"))
setMethod("request", "TheCity", 
          function(object,
                   path,
                   query = "", 
                   host = "https://api.onthecity.org", 
                   time = as.numeric(Sys.time()), verb = 'GET') {
              
              sig = hmac_signature(object, path = path, time = time, 
                                   query = query, host = host, verb = verb)
    
              ctime = as.character(time)
              ltime = strsplit(ctime, split = ".", fixed = TRUE)[[1]]
              ctime = ltime[1]
              
              # If a body is being sent, the Content-Length & Content-Type 
              # header must be set.
              headers <- c("X-City-Sig" = sig, 
                           "X-City-User-Token" = token(object), 
                           "X-City-Time" = ctime,
                           "Accept" = "application/vnd.thecity.admin.v1+json",
                           "Content-Type" = "application/json"
                           )
              
              #get = getURL(paste(host, path, sep = "/"), httpheader = headers, verbose = T)
              GET(url = host, path = path, add_headers(headers))
          }
)

setGeneric("request_iterator", function(object, ...) standardGeneric("request_iterator"))
setMethod("request_iterator", "TheCity",
          function(object, resource, total = "all", start = 0) {
              
          }
)