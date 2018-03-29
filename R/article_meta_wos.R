article_meta_wos <- function(code, save_xml){
  api_url <- "http://articlemeta.scielo.org"
  use <- "/api/v1/article/"
  format <- "xmlwos"
  param1 <- paste0("?code=", code)
  param2 <- paste0("&format=", format)
  url <- paste0(api_url, use, param1, param2)

  if(save_xml == TRUE){
    download.file(url, paste0(code, ".xml"), mode = "wb")
    result <- XML::xmlParse(paste0(code, ".xml"))
  } else {
    result <- XML::xmlParse(url)
  }

  result <- XML::xmlToList(result)

  ### Journal data
  journal_id <- try(result$article$front$`journal-meta`$`journal-id`$text, silent = TRUE)
  journal_id <- ifelse(class(journal_id) %in% c("try-error", "NULL"), NA, journal_id)

  journal_id_type <- try(result$article$front$`journal-meta`$`journal-id`$.attrs[1], silent = TRUE)
  journal_id_type <- ifelse(class(journal_id_type) %in% c("try-error", "NULL"), NA, journal_id_type)

  journal_title <- try(result$article$front$`journal-meta`$`journal-title-group`$`journal-title`, silent = TRUE)
  journal_title <- ifelse(class(journal_title) %in% c("try-error", "NULL"), NA, journal_title)

  journal_abbrev_title <- try(result$article$front$`journal-meta`$`journal-title-group`$`abbrev-journal-title`, silent = TRUE)
  journal_abbrev_title <- ifelse(class(journal_abbrev_title) %in% c("try-error", "NULL"), NA, journal_abbrev_title)

  journal_issn <- try(result$article$front$`journal-meta`$issn, silent = TRUE)
  journal_issn <- ifelse(class(journal_issn) %in% c("try-error", "NULL"), NA, journal_issn)

  journal_collection <- try(result$article$front$`journal-meta`$collection, silent = TRUE)
  journal_collection <- ifelse(class(journal_collection) %in% c("try-error", "NULL"), NA, journal_collection)

  journal_publisher_name <- try(result$article$front$`journal-meta`$publisher$`publisher-name`, silent = TRUE)
  journal_publisher_name <- ifelse(class(journal_publisher_name) %in% c("try-error", "NULL"), NA, journal_publisher_name)

  ### Article data

  # Simple fields
  article_language <- try(result$article$.attrs[1], silent = TRUE)
  article_language <- ifelse(class(article_language) %in% c("try-error", "NULL"), NA, article_language)

  article_type <- try(result$article$.attrs[2], silent = TRUE)
  article_type  <- ifelse(class(article_type) %in% c("try-error", "NULL"), NA, article_type)

  article_unique_id <- try(result$article$front$`article-meta`$`unique-article-id`$text, silent = TRUE)
  article_unique_id <- ifelse(class(article_unique_id) %in% c("try-error", "NULL"), NA, article_unique_id)

  article_publisher_id <- try(result$article$front$`article-meta`$`article-id`$text, silent = TRUE)
  article_publisher_id <- ifelse(class(article_publisher_id) %in% c("try-error", "NULL"), NA, article_publisher_id)

  article_subject <- try(result$article$front$`article-meta`$`article-categories`$`subj-group`$subject, silent = TRUE)
  article_subject <- ifelse(class(article_subject) %in% c("try-error", "NULL"), NA, article_subject)

  article_title <- try(result$article$front$`article-meta`$`title-group`$`article-title`$text, silent = TRUE)
  article_title <- ifelse(class(article_title) %in% c("try-error", "NULL"), NA, article_title)

  article_title_lang <- try(result$article$front$`article-meta`$`title-group`$`article-title`$.attrs[1], silent = TRUE)
  article_title_lang <- ifelse(class(article_title_lang) %in% c("try-error", "NULL"), NA, article_title_lang)

  article_translated_title <- try(result$article$front$`article-meta`$`title-group`$`trans-title-group`$`trans-title`, silent = TRUE)
  article_translated_title <- ifelse(class(article_translated_title) %in% c("try-error", "NULL"), NA, article_translated_title)

  article_translated_title_lang <- try(result$article$front$`article-meta`$`title-group`$`trans-title-group`$.attrs[1], silent = TRUE)
  article_translated_title_lang <- ifelse(class(article_translated_title_lang) %in% c("try-error", "NULL"), NA, article_translated_title_lang)

  article_pub_date_day <- try(result$article$front$`article-meta`$`pub-date`$day, silent = TRUE)
  article_pub_date_day <- ifelse(class(article_pub_date_day) %in% c("try-error", "NULL"), NA, article_pub_date_day)

  article_pub_date_month <- try(result$article$front$`article-meta`$`pub-date`$month, silent = TRUE)
  article_pub_date_month <- ifelse(class(article_pub_date_month) %in% c("try-error", "NULL"), NA, article_pub_date_month)

  article_pub_date_year <- try(result$article$front$`article-meta`$`pub-date`$year, silent = TRUE)
  article_pub_date_year <- ifelse(class(article_pub_date_year) %in% c("try-error", "NULL"), NA, article_pub_date_year)

  article_volume <- try(result$article$front$`article-meta`$volume, silent = TRUE)
  article_volume <- ifelse(class(article_volume) %in% c("try-error", "NULL"), NA, article_volume)

  article_issue <- try(result$article$front$`article-meta`$issue, silent = TRUE)
  article_issue <- ifelse(class(article_issue) %in% c("try-error", "NULL"), NA, article_issue)

  article_first_page <- try(result$article$front$`article-meta`$fpage, silent = TRUE)
  article_first_page <- ifelse(class(article_first_page) %in% c("try-error", "NULL"), NA, article_first_page)

  article_last_page <- try(result$article$front$`article-meta`$lpage, silent = TRUE)
  article_last_page <- ifelse(class(article_last_page) %in% c("try-error", "NULL"), NA, article_last_page)

  article_license_type <- try(result$article$front$`article-meta`$permissions$license$.attrs[1], silent = TRUE)
  article_license_type <- ifelse(class(article_license_type) %in% c("try-error", "NULL"), NA, article_license_type)

  article_license_text <- try(result$article$front$`article-meta`$permissions$license$`license-p`, silent = TRUE)
  article_license_text <- ifelse(class(article_license_text) %in% c("try-error", "NULL"), NA, article_license_text)

  article_abstract <- try(result$article$front$`article-meta`$abstract$p, silent = TRUE)
  article_abstract <- ifelse(class(article_abstract) %in% c("try-error", "NULL"), NA, article_abstract)

  article_abstract_lang <- try(result$article$front$`article-meta`$abstract$.attrs[1], silent = TRUE)
  article_abstract_lang <- ifelse(class(article_abstract_lang) %in% c("try-error", "NULL"), NA, article_abstract_lang)

  article_trans_abstract <- try(result$article$front$`article-meta`$`trans-abstract`$p, silent = TRUE)
  article_trans_abstract <- ifelse(class(article_trans_abstract) %in% c("try-error", "NULL"), NA, article_trans_abstract)

  article_trans_abstract_lang <- try(result$article$front$`article-meta`$`trans-abstract`$.attrs[1], silent = TRUE)
  article_trans_abstract_lang <- ifelse(class(article_trans_abstract_lang) %in% c("try-error", "NULL"), NA, article_trans_abstract_lang)

  # List fields
  article_contributors <- result$article$front$`article-meta`$`contrib-group`
  article_contributors_count <- length(article_contributors)
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
    article_language = ifelse(!is.null(article_language), as.character(article_language), NA),
    journal_id = ifelse(!is.null(journal_id), journal_id, NA),
    journal_id_type = ifelse(!is.null(journal_id_type), journal_id_type, NA),
    journal_title = ifelse(!is.null(journal_title), journal_title, NA),
    journal_abbrev_title = ifelse(!is.null(journal_abbrev_title), journal_abbrev_title, NA),
    journal_issn = ifelse(!is.null(journal_issn), journal_issn, NA),
    journal_collection = ifelse(!is.null(journal_collection), journal_collection, NA),
    journal_publisher_name = ifelse(!is.null(journal_publisher_name), journal_publisher_name, NA),
    article_type = ifelse(!is.null(article_type), article_type, NA),
    article_unique_id = ifelse(!is.null(article_unique_id), article_unique_id, NA),
    article_publisher_id = ifelse(!is.null(article_publisher_id), article_publisher_id, NA),
    article_subject = ifelse(!is.null(article_subject), article_subject, NA),
    article_title = ifelse(!is.null(article_title), article_title, NA),
    article_title_lang = ifelse(!is.null(article_title_lang), article_title_lang, NA),
    article_translated_title = ifelse(!is.null(article_translated_title), article_translated_title, NA),
    article_translated_title_lang = ifelse(!is.null(article_translated_title_lang), article_translated_title_lang, NA),
    article_pub_date_day = ifelse(!is.null(article_pub_date_day), article_pub_date_day, NA),
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
    article_contributors_count = ifelse(!is.null(article_contributors_count), article_contributors_count, NA),
    article_contributors_names = ifelse(!is.null(article_contributors_names), article_contributors_names, NA),
    article_contributors_roles = ifelse(!is.null(article_contributors_roles), article_contributors_roles, NA),
    article_contributors_affiliation_institution = ifelse(!is.null(article_contributors_affiliation_institution), article_contributors_affiliation_institution, NA),
    article_contributors_affiliation_country = ifelse(!is.null(article_contributors_affiliation_country), article_contributors_affiliation_country, NA)
  )

  # Factor variables to character
  df_articles %>% dplyr::mutate_if(is.factor, as.character) %>% as.data.frame -> df_articles



  ### Citation data
  citation_list <- result$article$back$`ref-list`
  df_citation <- data.frame()
  if(!is.null(citation_list)){
    citation_list <- citation_list[names(citation_list) == "ref"]
    for(i in citation_list){
      i <- as.data.frame(i)
      i$source <- article_unique_id
      i$count <- length(citation_list)
      df_citation <- suppressWarnings(dplyr::bind_rows(df_citation, i))
    }
  }

  ### Results
  list_df <- list(df_articles = df_articles, df_citation = df_citation)

  return(list_df)
}
