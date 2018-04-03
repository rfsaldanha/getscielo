articles_meta_wos_list <- function(code, pre = NULL){
  results_articles <- data.frame()
  results_citation <- data.frame()
  for(i in code){
    print(i)
    if(!is.null(pre)){
      result <- article_meta_wos(paste0(pre,i))
    } else {
      result <- article_meta_wos(i)
    }

    results_articles <- suppressWarnings(dplyr::bind_rows(results_articles, result$df_articles))
    results_citation <- suppressWarnings(dplyr::bind_rows(results_citation, result$df_citation))
  }

  list_df <- list(results_articles = results_articles, results_citation = results_citation)
  return(list_df)
}
