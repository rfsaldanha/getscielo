journals <- function(collection, limit = 1000, offset = 0){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/journal/identifiers/"
  param1  <- paste0("?collection=", collection)
  param2 <- paste0("&limit=", limit)
  param3 <- paste0("&offset=", offset)
  url <- paste0(api_url, use, param1, param2, param3)

  result <- jsonlite::fromJSON(url)

  return(result$objects)
}
