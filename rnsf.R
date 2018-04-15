library(jsonlite)
library(plyr)
library(zipcode)

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
      Sys.sleep(3)
      result <- plyr::rbind.fill(result, local.result)
      if(verbose) {
        print(paste0("Finished next batch; now ", nrow(result), " records"))
      }
    }
  }
  colnames(result) <- gsub('response.award.', '', colnames(result))
  return(result)
}

print_fields_get <- function() {
  print_fields <- c("id", "agency", "awardeeCity", "awardeeCountryCode", 
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

nsf_get_all <- function() {
  data(zipcode)
  all_grants <- data.frame()
  for (zipcode.index in sequence(nrow(zipcode))) {
    Sys.sleep(3)
   all_grants <- plyr::rbind.fill(all_grants, nsf_return(zipcode = zipcode$zip[zipcode.index])) 
   print(paste0("Finished zipcode ", zipcode$zip[zipcode.index], " meaning ", round(100*zipcode.index/nrow(zipcode),2), '% done. ', nrow(all_grants), " rows done"))
   save(all_grants, file="~/Downloads/NSFGrants.rda")
  }
  return(all_grants)
}