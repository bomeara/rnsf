library(jsonlite)
library(plyr)

#' Retrieve table of grant information
#' 
#' @param keyword Optional keyword to search on
#' @param zipcode Optional zip code to limit search to
#' @param print_fields Names of elements to return (see NSF API documentation)
#' @param verbose If TRUE, outputs progress
#' @return A data frame with returned info
#' @export
nsf_return <- function(keyword=NULL, zipcode=NULL, verbose=TRUE, print_fields=print_fields_get()) {
  base_url <- paste0('https://api.nsf.gov/services/v1/awards.json?printFields=', paste0(print_fields, collapse=","), "&")
  url_parameters <- c()
  if (!is.null(keyword)) {
    url_parameters <- paste0(url_parameters, "keyword=",URLencode(keyword), collapse='&')
  }
  if (!is.null(zipcode)) {
    url_parameters <- paste0(url_parameters, "awardeeZipCode=",URLencode(zipcode), collapse='&')
  }
  result <- data.frame(fromJSON(paste0(base_url, url_parameters)))
  if(verbose) {
    print("Finished first batch") 
  }
  offset <- 1
  local.result <- result
  while(nrow(local.result)==25) {
    offset <- offset+25
    local.result <- data.frame(fromJSON(paste0(base_url, url_parameters, '&offset=', offset)))
    if(nrow(local.result)>0 & ncol(local.result)>1) {
      result <- plyr::rbind.fill(result, local.result)
      if(verbose) {
        print(paste0("Finished next batch; now ", nrow(result), " records"))
      }
    }
  }
  colnames(result) <- gsub('response.', '', colnames(result))
  return(result)
}

print_fields_get <- function() {
  print_fields <- c("rpp", "offset", "id", "agency", "awardeeCity", "awardeeCountryCode", 
                    "awardeeCounty", "awardeeDistrictCode", "awardeeName", "awardeeStateCode", 
                    "awardeeZipCode", "cfdaNumber", "coPDPI", "date", "startDate", 
                    "expDate", "estimatedTotalAmt", "fundsObligatedAmt", "dunsNumber", 
                    "fundProgramName", "parentDunsNumber", "pdPIName", "perfCity", 
                    "perfCountryCode", "perfCounty", "perfDistrictCode", "perfLocation", 
                    "perfStateCode", "perfZipCode", "poName", "primaryProgram", "transType", 
                    "title", "awardee", "poPhone", "poEmail", "awardeeAddress", "perfAddress", 
                    "publicationResearch", "publicationConference", "fundAgencyCode", 
                    "awardAgencyCode", "projectOutComesReport", "abstractText", "piFirstName", 
                    "piMiddeInitial", "piLastName", "piPhone", "piEmail")
  return(print_fields)
}