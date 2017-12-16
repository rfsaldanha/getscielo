articles_year <- function(collection, year){
  # Get total of articles
  total <- articles(collection = collection, yearFrom = year, yearUntil = year, limit = 1)
  total <- total$meta$total

  # Number of loops necessary
  loops <- ceiling(total/1000)

  # DF to store results
  results <- data.frame()

  # Get articles
  for(i in 1:loops){
    offset <- (i-1)*1000

    result <- articles(collection = collection, yearFrom = year, yearUntil = year, offset = offset)
    result <- result$objects
    results <- dplyr::bind_rows(results, result)
  }

  return(results)
}
