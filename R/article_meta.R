article_meta <- function(code, format = "xmlcrossref"){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/article/"
  param1 <- paste0("?code=", code)
  param2 <- paste0("&format=", format)
  url <- paste0(api_url, use, param1, param2)

  result <- XML::xmlParse(url)
  result <- XML::xmlToList(result)

  timestamp <- result$head$timestamp
  journal_full_title <- result$body$journal$journal_metadata$full_title
  journal_issn <- result$body$journal$journal_metadata$issn$text
  journal_publication_date_year <- result$body$journal$journal_issue$publication_date$year
  journal_publication_date_month <- result$body$journal$journal_issue$publication_date$month
  journal_publication_date_day <- result$body$journal$journal_issue$publication_date$day
  journal_volume <- result$body$journal$journal_issue$journal_volume$volume
  journal_issue <- result$body$journal$journal_issue$issue
  article_title <- result$body$journal$journal_article$titles$title

  df <- data.frame(
    timestamp = ifelse(!is.null(timestamp), timestamp, NA),
    journal_full_title = ifelse(!is.null(journal_full_title), journal_full_title, NA),
    journal_issn = ifelse(!is.null(journal_issn), journal_issn, NA),
    journal_publication_date_year = ifelse(!is.null(journal_publication_date_year), journal_publication_date_year, NA),
    journal_publication_date_month = ifelse(!is.null(journal_publication_date_month), journal_publication_date_month, NA),
    journal_publication_date_day = ifelse(!is.null(journal_publication_date_day), journal_publication_date_day, NA),
    journal_volume = ifelse(!is.null(journal_volume), journal_volume, NA),
    journal_issue = ifelse(!is.null(journal_issue), journal_issue, NA),
    article_title = ifelse(!is.null(article_title), article_title, NA)
  )

  return(df)
}
