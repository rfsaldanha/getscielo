articles <- function(issn, collection, yearFrom, yearUntil, limit = 1000, offset = 0){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/article/identifiers/"
  param1 <- paste0("?issn=", issn)
  param2 <- paste0("&collection=", collection)
  param3 <- paste0("&from=", yearFrom, "-01-01")
  param4 <- paste0("&until=", yearUntil, "-12-31")
  param5 <- paste0("&limit=", limit)
  param6 <- paste0("&offset=", offset)
  url <- paste0(api_url, use, param1, param2, param3, param4, param5, param6)

  result <- jsonlite::fromJSON(url)

  return(result)
}
