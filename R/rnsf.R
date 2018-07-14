#library(jsonlite)
#library(plyr)

#' Retrieve table of grant information
#'
#' @param keyword Optional keyword to search on
#' @param zipcode Optional zip code to limit search to (note that you should try both 5 and 9 digit). Note that there are often grants that lack zip code info
#' @param agency Agency to search for (NSF or NASA)
#' @param verbose If TRUE, outputs progress
#' @param print_fields Names of elements to return (see NSF API documentation)
#' @param save_file File to save results to while running
#' @return A data frame with returned info
#' @examples
#' ants <- nsf_return(keyword="Formicidae")
#' nsf_wordcloud(ants$abstractText)
#'
#' sanderson <- nsf_return(keyword="Sanderson")
#' mjs.PI <- sanderson[grepl("Michael J Sanderson",sanderson$pdPIName),] #note the lack of period for middle initial in the search string
#' mjs.CoPI <- sanderson[grepl("Michael J Sanderson",sanderson$coPDPI),]
#' mjs.grants <- rbind(mjs.PI, mjs.CoPI)
#' plot(x=lubridate::mdy(mjs.grants$startDate), y=mjs.grants$fundsObligatedAmt, pch=20, log="y", bty="n", xlab="Start date", ylab="Funding amount in US dollars")
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

#' Attempt to get all NSF grants for a person's name
#'
#' @description
#' This will *attempt* to get all information for a person. There are many potential problems with this. Multiple people could have the same name. People could also modify their names (with change in marital status, change in gender, whether they use a middle initial, and so forth) so do not use this blindly to evaluate someone. That's what the h-index is for (I joke).
#' @param first_name Just the first name
#' @param middle_initial Only one letter, no periods
#' @param last_name Only the last name
#' @return A data frame with grant info
#' @examples
#' # Let's check with my grants (mainly to show how to deal with weird characters, like apostrophes)
#' bco <- nsf_get_person("Brian", "C", "O'Meara")
#' nsf_wordcloud(bco$abstractText)
#' plot(x=range(c(lubridate::mdy(bco$startDate), lubridate::mdy(bco$expDate))), y=range(bco$fundsObligatedAmt), type="n", log="y", bty="n", xlab="Date", ylab="Funding amount in US dollars")
#' for (grant.index in sequence(nrow(bco))) {
#'   lines(x=c(lubridate::mdy(bco$startDate)[grant.index], lubridate::mdy(bco$expDate)[grant.index]), y=rep(bco$fundsObligatedAmt[grant.index],2))
#'   text(x=mean(c(lubridate::mdy(bco$startDate)[grant.index], lubridate::mdy(bco$expDate)[grant.index])), y=as.numeric(bco$fundsObligatedAmt[grant.index]), labels=bco$title[grant.index], pos=3, cex=0.5)
#' }
#' @export
nsf_get_person <- function(first_name, middle_initial, last_name) {
  if(nchar(middle_initial)>1) {
    stop("middle initial should be a single character only (and no periods)")
  }
  last_only <- nsf_return(keyword=last_name)
  print("Note that these are all records with this last name as a keyword")
  PI <- last_only[grepl(paste(first_name, middle_initial, last_name),last_only$pdPIName, ignore.case=TRUE),]
  CoPI <- last_only[grepl(paste(first_name, middle_initial, last_name),last_only$coPDPI, ignore.case=TRUE),]
  person.grants <- rbind(PI, CoPI)
  return(person.grants)
}

#' Retrieve all NSF grant information, saving to a file
#'
#' @param save_file File to save results to while running
#' @return A data frame with grant info
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

#' Text wordcloud
#'
#' Make a word cloud of all the interesting words
#' @param text A vector of text (for example, from grants$abstractText)
#' @param prune_words Other words you want to prune before plotting
#' @param max_words How many words at most to plot (it picks the most frequent ones)
#' @param ... Arguments to the wordcloud function
#' @description
#' Create a wordcloud of text. This excludes common English words ("the", "and") but you can add your own to exclude as well. This uses the wordcloud package for plotting, and you can pass other arguments to that to make the plot prettier (see ?wordcloud::wordcloud)
#' This follows the advice from http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know on making a word cloud
#' @examples
#' data(grants)
#' nsf_wordcloud(grants$abstractText[1:10])
#' @export
nsf_wordcloud <- function(text=nsf_get_all()$abstractText, prune_words=c("will", "nfs"), max_words=500, ...) {
  text_corpus <- suppressWarnings(tm::Corpus(tm::VectorSource(text)))
  toSpace <- suppressWarnings(tm::content_transformer(function (x , pattern ) gsub(pattern, " ", x)))
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, toSpace, "/"))
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, toSpace, "@"))
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, toSpace, "\\|"))
  # Convert the text to lower case
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::content_transformer(tolower)))
  # Remove numbers
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::removeNumbers))
  # Remove english common stopwords
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::removeWords, tm::stopwords("english")))
  if(length(prune_words)>0) {
    text_corpus <- suppressWarnings(tm_map(text_corpus, tm::removeWords, prune_words))
  }
  # Remove punctuations
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::removePunctuation))
  # Eliminate extra white spaces
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::stripWhitespace))
  dtm <- TermDocumentMatrix(text_corpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)[1:min(max_words, length(v))]
  d <- data.frame(word = names(v),freq=v)
  wordcloud(words = d$word, freq = d$freq, random.order=FALSE, ...)
}
