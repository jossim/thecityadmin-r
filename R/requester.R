library(digest)
library(RCurl)
library(httr)

setGeneric("sleep.time", function(object) standardGeneric("sleep.time"))
setMethod("sleep.time", "TheCity",
          function(object) {
              max.sec = max.sleep.time(object) * 60
              last.sec = as.numeric(last.request.time(object))
              time.now = as.numeric(Sys.time())
              
              if (last.sec + max.sec < time.now) {
                  time = 0 
              } else {
                  time = 60 * max.sleep.time(object) * 
                        (1 - rate.limit.remaining(object) / rate.limit(object))
              }
              
              msg = paste(Sys.time(), "Sleeping:", time, "sec.", 
                          rate.limit.remaining(object), "remain out of",
                          rate.limit(object), "total. Max sleep:",
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
              
              if(nchar(query) > 0)
                  string_to_sign = paste(ctime, verb, host, "/", path, "?", 
                                         query, sep = "")
              else
                  string_to_sign = paste(ctime, verb, host, "/", path, sep = "")
              
              hm = hmac(key = key(object), object = string_to_sign, 
                        algo = "sha256", raw = T)
                
              curlEscape(base64(hm)[1])
          }
)

setGeneric("request", function(object, ...) standardGeneric("request"))
setMethod("request", "TheCity", 
          function(object,
                   path = "",
                   query = "", 
                   host = "https://api.onthecity.org", 
                   time = Sys.time(), verb = 'GET') {
              
              time = Sys.time()
              time = as.numeric(time)
              
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
              if(nchar(query) > 0)
                  r = GET(url = host, path = path, query = query, add_headers(headers))
              else
                  r = GET(url = host, path = path, add_headers(headers))
              
              last.request.time(object) = Sys.time()
              
              object@env$request = list(path = path, query = query, host = host, 
                                        verb = verb)
              
              return(r)
          }
)

setGeneric("request.error.handler", function(object, ...) 
    standardGeneric("request.error.handler"))
setMethod("request.error.handler", "TheCity",
          function(object, request, request.tries = 3) {
              sc = request$status_code
              status = request$status
              if (sc == 200 || sc == 304) {
                  rate.limit(object) = 
                      as.numeric(request$headers['x-city-ratelimit-limit-by-account'])
                  rate.limit.remaining(object) = 
                      as.numeric(request$headers['x-city-ratelimit-remaining-by-account'])
                  return(request)
              }
              else if (sc == 404) {
                  stop("Request returned 404 error.")
              }
              else if(request.tries <= 0) {
                  msg = paste("Unable to proccess request. Reason:", status)
                  stop(msg)
              }
              else {
                  request.tries = request.tries - 1
                  
                  msg = paste("Error: ", status, " trying ", 
                              request.tries, " more times.", sep = "")
                  warning(msg)
                  print(msg)
                  
                  sleep.time(object)
                  req.data = object@env$request
                  
                  req = request(object, path = req.data$path, 
                                query = req.data$query, host = req.data$host,
                                verb = req.data$verb)
                  
                  request.error.handler(object, req, request.tries)
              }
          }
)

setGeneric("request.iterator", function(object, ...) standardGeneric("request.iterator"))
setMethod("request.iterator", "TheCity",
          function(object, resource, df, params = c(), total = "all", start = 1,
                   sleep = TRUE) {
              page = start
              pages.left = TRUE
             
              if(total != "all") page.stop = start + total - 1
              
              while(pages.left) {
                  if(sleep) sleep.time(object) # sleep before sending the request
                  
                  query = paste("page=", page, sep = "")
                  req = request.error.handler(object,
                                request(object, path = resource, query = query))
                  cont = content(req)
                  
                  items = cont[[resource]]
                  # remove NULLs & replace them with NAs
                  
                  items = lapply(items, function(x) flatten.list(x))

                  # If the data frame is empty, make a new one with the correct
                  # names and add data to it. If it's not, then just add data.
                  if(ncol(df) == 0) {
                      df = data.frame(matrix(
                                unlist(items[[1]]),
                                nrow = 1,
                                ncol = length(unlist(items[[1]])))
                            )
                      
                      colnames(df) = names(items[[1]])
                      df = build.frame(df, items, index = 2)
                  }
                  else {
                      df = build.frame(df, items, index = 1)
                  }
                                    
                  if(total == "all") {
                      msg = paste("Page: ", page, ". Stopping at: ", 
                                  cont$total_pages, sep = "")
                      print(msg)
                      pages.left = cont$total_pages > page
                  }
                  else {
                      msg = paste("Page: ", page, ". Stopping at: ",
                                  page.stop, sep = "")
                      print(msg)
                      pages.left = page.stop > page
                  }
                  page = page + 1
              }
              return(df)
          }
)