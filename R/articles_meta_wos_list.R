articles_meta_wos_list <- function(code, save_xml){
  results_articles <- data.frame()
  results_citation <- data.frame()
  for(i in code){
    Sys.sleep(runif(1,0,5))
    print(i)
    if(save_xml == TRUE){
      result <- article_meta_wos(i, TRUE)
    } else {
      result <- article_meta_wos(i)
    }
    results_articles <- suppressWarnings(dplyr::bind_rows(results_articles, result$df_articles))
    results_citation <- suppressWarnings(dplyr::bind_rows(results_citation, result$df_citation))
  }

  list_df <- list(results_articles = results_articles, results_citation = results_citation)
  return(list_df)
}
