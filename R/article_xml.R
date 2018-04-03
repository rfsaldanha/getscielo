article_xml <- function(code){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/article/"
  format <- "xmlwos"
  param1 <- paste0("?code=", code)
  param2 <- paste0("&format=", format)
  url <- paste0(api_url, use, param1, param2)

  download.file(url, paste0(code, ".xml"), mode = "wb")
}
