article_meta <- function(code){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/article/"
  format <- "xmlwos"
  param1 <- paste0("?code=", code)
  param2 <- paste0("&format=", format)
  url <- paste0(api_url, use, param1, param2)

  result <- XML::xmlParse(url)
  result <- XML::xmlToList(result)

  ### Journal data
  journal_id <- result$article$front$`journal-meta`$`journal-id`$text
  journal_title <- result$article$front$`journal-meta`$`journal-title-group`$`journal-title`
  journal_abbrev_title <- result$article$front$`journal-meta`$`journal-title-group`$`abbrev-journal-title`
  journal_issn <- result$article$front$`journal-meta`$issn
  journal_collection <- result$article$front$`journal-meta`$collection
  journal_publisher <- result$article$front$`journal-meta`$publisher$`publisher-name`

  ### Article data

  # Simple fields
  article_type <- result$article$.attrs[2]
  article_unique_id <- result$article$front$`article-meta`$`unique-article-id`$text
  article_publisher_id <- result$article$front$`article-meta`$`article-id`$text
  article_subject <- result$article$front$`article-meta`$`article-categories`$`subj-group`$subject
  article_title <- result$article$front$`article-meta`$`title-group`$`article-title`$text
  article_title_lang <- result$article$front$`article-meta`$`title-group`$`article-title`$.attrs[1]
  article_translated_title <- result$article$front$`article-meta`$`title-group`$`trans-title-group`$`trans-title`
  article_translated_title_lang <- result$article$front$`article-meta`$`title-group`$`trans-title-group`$.attrs[1]
  article_pub_date_month <- result$article$front$`article-meta`$`pub-date`$month
  article_pub_date_year <- result$article$front$`article-meta`$`pub-date`$year
  article_volume <- result$article$front$`article-meta`$volume
  article_issue <- result$article$front$`article-meta`$issue
  article_first_page <- result$article$front$`article-meta`$fpage
  article_last_page <- result$article$front$`article-meta`$lpage
  article_license_type <- result$article$front$`article-meta`$permissions$license$.attrs[1]
  article_license_text <- result$article$front$`article-meta`$permissions$license$`license-p`
  article_abstract <- result$article$front$`article-meta`$abstract$p
  article_abstract_lang <- result$article$front$`article-meta`$abstract$.attrs[1]
  article_trans_abstract <- result$article$front$`article-meta`$`trans-abstract`$p
  article_trans_abstract_lang <- result$article$front$`article-meta`$`trans-abstract`$.attrs[1]

  # List fields
  article_contributors <- result$article$front$`article-meta`$`contrib-group`
  article_contributors_names <- NULL
  article_contributors_roles <- NULL
  article_contributors_aff <- result$article$front$`article-meta`
  article_contributors_aff <- article_contributors_aff[names(article_contributors_aff) == "aff"]
  article_contributors_affiliation_institution <- NULL
  article_contributors_affiliation_country <- NULL
  if(!is.null(article_contributors)){
    # Names
    for(i in article_contributors){
      surname <- i$name$surname
      given_names <- i$name$`given-names`
      article_contributors_names <- c(article_contributors_names, paste(given_names, surname))
    }
    article_contributors_names <- paste(article_contributors_names, collapse = " # ")
    # Roles
    for(i in article_contributors){
      role <- i$role
      article_contributors_roles <- c(article_contributors_roles, role)
    }
    article_contributors_roles <- paste(article_contributors_roles, collapse = " # ")
    # Affiliation
    for(i in article_contributors_aff){
      article_contributors_affiliation_institution <- c(article_contributors_affiliation_institution, i$institution)
      article_contributors_affiliation_country <- c(article_contributors_affiliation_country, i$country)
    }
    article_contributors_affiliation_institution <- paste(article_contributors_affiliation_institution, collapse = " # ")
    article_contributors_affiliation_country <- paste(article_contributors_affiliation_country, collapse = " # ")
  }

  df_articles <- data.frame(
    article_type = ifelse(!is.null(article_type), article_type, NA),
    article_unique_id = ifelse(!is.null(article_unique_id), article_unique_id, NA),
    article_publisher_id = ifelse(!is.null(article_publisher_id), article_publisher_id, NA),
    article_subject = ifelse(!is.null(article_subject), article_subject, NA),
    article_title = ifelse(!is.null(article_title), article_title, NA),
    article_title_lang = ifelse(!is.null(article_title_lang), article_title_lang, NA),
    article_translated_title = ifelse(!is.null(article_translated_title), article_translated_title, NA),
    article_translated_title_lang = ifelse(!is.null(article_translated_title_lang), article_translated_title_lang, NA),
    article_pub_date_month = ifelse(!is.null(article_pub_date_month), article_pub_date_month, NA),
    article_pub_date_year = ifelse(!is.null(article_pub_date_year), article_pub_date_year, NA),
    article_volume = ifelse(!is.null(article_volume), article_volume, NA),
    article_issue = ifelse(!is.null(article_issue), article_issue, NA),
    article_first_page = ifelse(!is.null(article_first_page), article_first_page, NA),
    article_last_page = ifelse(!is.null(article_last_page), article_last_page, NA),
    article_license_type = ifelse(!is.null(article_license_type), article_license_type, NA),
    article_license_text = ifelse(!is.null(article_license_text), article_license_text, NA),
    article_abstract = ifelse(!is.null(article_abstract), article_abstract, NA),
    article_abstract_lang = ifelse(!is.null(article_abstract_lang), article_abstract_lang, NA),
    article_trans_abstract = ifelse(!is.null(article_trans_abstract), article_trans_abstract, NA),
    article_trans_abstract_lang = ifelse(!is.null(article_trans_abstract_lang), article_trans_abstract_lang, NA),
    article_contributors_names = ifelse(!is.null(article_contributors_names), article_contributors_names, NA),
    article_contributors_roles = ifelse(!is.null(article_contributors_roles), article_contributors_roles, NA),
    article_contributors_affiliation_institution = ifelse(!is.null(article_contributors_affiliation_institution), article_contributors_affiliation_institution, NA),
    article_contributors_affiliation_country = ifelse(!is.null(article_contributors_affiliation_country), article_contributors_affiliation_country, NA)
  )

  ### Citation data
  # citation_list <- result$body$journal$journal_article$citation_list
  # df_citation <- data.frame()
  # if(!is.null(citation_list)){
  #   citation_list <- citation_list[names(citation_list) == "citation"]
  #   for(i in citation_list){
  #     i <- as.data.frame(i)
  #     i$source <- article_identifier
  #     suppressWarnings(df_citation <- dplyr::bind_rows(df_citation, i))
  #   }
  # }

  ### Results
  #list_df <- list(df_articles = df_articles, df_citation = df_citation)
  list_df <- list(df_articles = df_articles, df_citation = NA)

  return(list_df)
}
