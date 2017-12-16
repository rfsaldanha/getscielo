articles_meta_list <- function(code){
  results <- data.frame()
  for(i in code){
    Sys.sleep(1)
    print(i)
    result <- article_meta(i)
    results <- dplyr::bind_rows(results, result)
  }

  return(results)
}
