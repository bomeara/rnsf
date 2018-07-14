#library(jsonlite)
#library(plyr)

#' Retrieve table of grant information
#'
#' @param keyword Optional keyword to search on
#' @param zipcode Optional zip code to limit search to (note that you should try both 5 and 9 digit)
#' @param agency Agency to search for (NSF or NASA)
#' @param verbose If TRUE, outputs progress
#' @param print_fields Names of elements to return (see NSF API documentation)
#' @param save_file File to save results to while running
#' @return A data frame with returned info
#' @export
nsf_return <- function(keyword=NULL, zipcode=NULL, agency=NULL, verbose=TRUE, print_fields=print_fields_get(), save_file=NULL) {
  base_url <- paste0('https://api.nsf.gov/services/v1/awards.json?printFields=', paste0(print_fields, collapse=","), "&")
  url_parameters <- c()
  if (!is.null(keyword)) {
    url_parameters <- paste0(url_parameters, "keyword=",URLencode(keyword), collapse='&')
  }
  if (!is.null(zipcode)) {
    url_parameters <- paste0(url_parameters, "awardeeZipCode=",URLencode(zipcode), collapse='&')
  }
  if (!is.null(agency)) {
    url_parameters <- paste0(url_parameters, "agency=",URLencode(agency), collapse='&')
  }
  grants <- data.frame(jsonlite::fromJSON(paste0(base_url, url_parameters)))
  if(verbose) {
    print("Finished first batch")
  }
  offset <- 1
  local.grants <- grants
  if(!is.null(save_file)) {
    save(grants, file=save_file)
  }
  while(nrow(local.grants)==25) {
    offset <- offset+25
    local.grants <- data.frame(jsonlite::fromJSON(paste0(base_url, url_parameters, '&offset=', offset)))
    if(nrow(local.grants)>0 & ncol(local.grants)>1) {
      Sys.sleep(3)
      grants <- plyr::rbind.fill(grants, local.grants)
      if(verbose) {
        print(paste0("Finished next batch; now ", nrow(grants), " records"))
        if(!is.null(save_file)) {
          save(grants, file=save_file)
        }
      }
    }
  }
  colnames(grants) <- gsub('response.award.', '', colnames(grants))
  if(!is.null(save_file)) {
    save(grants, file=save_file)
  }
  return(grants)
}

print_fields_get <- function() {
  print_fields <- c("id", "agency", "awardeeCity", "awardeeCountryCode",
                    "awardeeCounty", "awardeeDistrictCode", "awardeeName", "awardeeStateCode",
                    "awardeeZipCode", "cfdaNumber", "coPDPI", "date", "startDate",
                    "expDate", "estimatedTotalAmt", "fundsObligatedAmt", "dunsNumber",
                    "fundProgramName", "parentDunsNumber", "pdPIName", "perfCity",
                    "perfCountryCode", "perfCounty", "perfDistrictCode", "perfLocation",
                    "perfStateCode", "perfZipCode", "poName", "primaryProgram", "transType",
                    "title", "awardee", "awardeeAddress", "perfAddress",
                    "publicationResearch", "publicationConference", "fundAgencyCode",
                    "awardAgencyCode", "projectOutComesReport", "abstractText", "piFirstName",
                    "piMiddeInitial", "piLastName")
  return(print_fields)
}


#' Retrieve all NSF grant information, saving to a file
#'
#' @param save_file File to save results to while running
#' @return A data frame with returned info
#' @export
nsf_get_all <- function(save_file="NSFAllGrants.rda") {
  grants <- nsf_return(agency="NSF", save_file=save_file)
  return(grants)
}

#' Grant information
#'
#' A dataset of NSF awards from its start until the package was last updated
#' @format A data frame with one row per award and columns with award information
"grants"

#' Abstract wordcloud
#'
#' Make a word cloud of all the words in the abstracts
#' @param grants A data frame with grant information
#' @description
#' This follows the advice from http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know on making a word cloud
#' @export
nsf_abstract_wordcloud <- function(grants=nsf_get_all(), ...) {
  abstract_corpus <- tm::Corpus(tm::VectorSource(grants$abstractText))
  toSpace <- tm::content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  abstract_corpus <- tm::tm_map(abstract_corpus, toSpace, "/")
  abstract_corpus <- tm::tm_map(abstract_corpus, toSpace, "@")
  abstract_corpus <- tm::tm_map(abstract_corpus, toSpace, "\\|")
  # Convert the text to lower case
  abstract_corpus <- tm::tm_map(abstract_corpus, content_transformer(tolower))
  # Remove numbers
  abstract_corpus <- tm::tm_map(abstract_corpus, removeNumbers)
  # Remove english common stopwords
  abstract_corpus <- tm::tm_map(abstract_corpus, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  abstract_corpus <- tm::tm_map(abstract_corpus, removeWords, c("blabla1", "blabla2"))
  # Remove punctuations
  abstract_corpus <- tm::tm_map(abstract_corpus, removePunctuation)
  # Eliminate extra white spaces
  abstract_corpus <- tm::tm_map(abstract_corpus, stripWhitespace)
  dtm <- TermDocumentMatrix(abstract_corpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  wordcloud(words = d$word, freq = d$freq, ...)
}
