article_meta <- function(code){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/article/"
  format <- "xmlcrossref"
  param1 <- paste0("?code=", code)
  param2 <- paste0("&format=", format)
  url <- paste0(api_url, use, param1, param2)

  result <- XML::xmlParse(url)
  result <- XML::xmlToList(result)

  ### Article data

  # Simple fields
  timestamp <- result$head$timestamp
  journal_full_title <- result$body$journal$journal_metadata$full_title
  journal_issn <- result$body$journal$journal_metadata$issn$text
  journal_volume <- result$body$journal$journal_issue$journal_volume$volume
  journal_issue <- result$body$journal$journal_issue$issue
  article_identifier <- result$body$journal$journal_article$publisher_item$identifier$text
  article_doi <- result$body$journal$journal_article$doi_data$doi
  article_publication_date_year <- result$body$journal$journal_article$publication_date$year
  article_publication_date_month <- result$body$journal$journal_article$publication_date$month
  article_publication_date_day <- result$body$journal$journal_article$publication_date$day
  article_title <- result$body$journal$journal_article$titles$title
  article_abstract <- result$body$journal$journal_article$abstract$p

  # List fields
  article_contributors <- result$body$journal$journal_article$contributors
  article_contributors_names <- NULL
  article_contributors_affiliation <- NULL
  if(!is.null(article_contributors)){
    article_contributors <- article_contributors[names(article_contributors) == "person_name"]
    for(i in article_contributors){
      article_contributors_names <- c(article_contributors_names, paste(i$given_name, i$surname))
    }
    article_contributors_names <- paste(article_contributors_names, collapse = "; ")
    for(i in article_contributors){
      article_contributors_affiliation <- c(article_contributors_affiliation, i$affiliation)
    }
    article_contributors_affiliation <- paste(article_contributors_affiliation, collapse = "; ")
  }

  df_articles <- data.frame(
    timestamp = ifelse(!is.null(timestamp), timestamp, NA),
    journal_full_title = ifelse(!is.null(journal_full_title), journal_full_title, NA),
    journal_issn = ifelse(!is.null(journal_issn), journal_issn, NA),
    journal_volume = ifelse(!is.null(journal_volume), journal_volume, NA),
    journal_issue = ifelse(!is.null(journal_issue), journal_issue, NA),
    article_identifier = ifelse(!is.null(article_identifier), article_identifier, NA),
    article_doi = ifelse(!is.null(article_doi), article_doi, NA),
    article_publication_date_year = ifelse(!is.null(article_publication_date_year), article_publication_date_year, NA),
    article_publication_date_month = ifelse(!is.null(article_publication_date_month), article_publication_date_month, NA),
    article_publication_date_day = ifelse(!is.null(article_publication_date_day), article_publication_date_day, NA),
    article_title = ifelse(!is.null(article_title), article_title, NA),
    article_abstract = ifelse(!is.null(article_abstract), article_abstract, NA),
    article_contributors_names = ifelse(!is.null(article_contributors_names), article_contributors_names, NA),
    article_contributors_affiliation = ifelse(!is.null(article_contributors_affiliation), article_contributors_affiliation, NA)
  )

  ### Citation data
  citation_list <- result$body$journal$journal_article$citation_list
  df_citation <- data.frame()
  if(!is.null(citation_list)){
    citation_list <- citation_list[names(citation_list) == "citation"]
    for(i in citation_list){
      i <- as.data.frame(i)
      i$source <- article_identifier
      suppressWarnings(df_citation <- dplyr::bind_rows(df_citation, i))
    }
  }

  ### Results
  list_df <- list(df_articles = df_articles, df_citation = df_citation)

  return(list_df)
}
