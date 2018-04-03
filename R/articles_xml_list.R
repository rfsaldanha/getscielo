articles_xml_list <- function(code){
  for(i in code){
    Sys.sleep(runif(1,0,5))
    print(i)
    article_xml(i)
  }
}
