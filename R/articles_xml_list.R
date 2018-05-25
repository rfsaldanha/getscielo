articles_xml_list <- function(code){
  for(i in code){
    Sys.sleep(runif(1,1,2))
    print(i)
    article_xml(i)
  }
}
