articles_history_year <- function(collection, from, until){
  # Get total of articles
  total <- articles_history(collection = collection, from = from, until = until, limit = 1)
  total <- total$meta$total
  print(total)
  
  # Number of loops necessary
  loops <- ceiling(total/1000)
  
  # DF to store results
  results <- data.frame()
  
  # Get articles
  for(i in 1:loops){
    print(i)
    offset <- (i-1)*1000
    
    result <- articles_history(collection = collection, from = from, until = until, offset = offset)
    result <- result$objects
    results <- dplyr::bind_rows(results, result)
  }
  
  return(results)
}