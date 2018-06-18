articles_history <- function(collection, from, until, limit=1000, offset=0){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/article/history/"
  param1 <- paste0("?collection=", collection)
  param2 <- paste0("&from=", from)
  param3 <- paste0("&until=", until)
  param4 <- paste0("&limit=", limit)
  param5 <- paste0("&offset=", offset)
  
  url <- paste0(api_url, use, param1, param2, param3, param4, param5)
  result <- jsonlite::fromJSON(url)
  
  return(result)
}