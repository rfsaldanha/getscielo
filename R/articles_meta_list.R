articles_meta_list <- function(code){
  results_articles <- data.frame()
  results_citation <- data.frame()
  for(i in code){
    Sys.sleep(1)
    print(i)
    result <- article_meta(i)
    results_articles <- dplyr::bind_rows(results_articles, result$df_articles)
    results_citation <- dplyr::bind_rows(results_citation, result$df_citation)
  }

  list_df <- list(results_articles = results_articles, results_citation = results_citation)
  return(list_df)
}
