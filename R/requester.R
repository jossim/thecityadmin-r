library(digest)
library(RCurl)
library(httr)

setGeneric("sleep.time", function(object) standardGeneric("sleep.time"))
setMethod("sleep.time", "TheCity",
          function(object) {
              max.sec = max.sleep.time(object) * 60
              last.sec = as.numeric(last.request.time(object))
              time.now = as.numeric(Sys.time())
              #browser()
              if (last.sec + max.sec < time.now) {
                  time = 0 
              } else {
                  time = 60 * max.sleep.time(object) * 
                        (1 - rate.limit.remaining(object) / rate.limit(object))
              }
              
              msg = paste("Sleeping for", time, "sec.", 
                          rate.limit.remaining(object), "requests remaining out of",
                          rate.limit(object), "total. Max sleep time is",
                          max.sleep.time(object), "minutes")
              print(msg)
              Sys.sleep(time)
          }
)

setGeneric("hmac.signature", function(object, ...) standardGeneric("hmac.signature"))
setMethod("hmac.signature", "TheCity",
          function(object,
                   path = "",
                   query = "",
                   verb = "GET",
                   host = "https://api.onthecity.org",
                   time = as.numeric(Sys.time())) {
              
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
              
              sig = hmac.signature(object, path = path, time = time, 
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
              r = GET(url = host, path = path, add_headers(headers))
              
              rate.limit(object) = 
                  as.numeric(r$headers['x-city-ratelimit-limit-by-account'])
              rate.limit.remaining(object) = 
                  as.numeric(r$headers['x-city-ratelimit-remaining-by-account'])
              last.request.time(object) = Sys.time()
              
              return(r)
          }
)

setGeneric("request_iterator", function(object, ...) standardGeneric("request_iterator"))
setMethod("request_iterator", "TheCity",
          function(object, resource, params = c(), total = "all", start = 0) {
              
          }
)