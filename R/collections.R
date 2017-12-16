collections <- function(){
  devtools::use_package("jsonlite")

  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/collection/identifiers/"
  url <- paste0(api_url, use)

  result <- jsonlite::fromJSON(url)

  return(result)
}
