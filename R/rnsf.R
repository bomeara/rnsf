#library(jsonlite)
#library(plyr)

#' Retrieve table of grant information
#' 
#' Note that it can only return a maximum of 9976 grants per call.
#'
#' @param keyword Optional keyword to search on
#' @param zipcode Optional zip code to limit search to (note that you should try both 5 and 9 digit). Note that there are often grants that lack zip code info
#' @param agency Agency to search for (NSF or NASA)
#' @param statecode Two letter state code for the awardee location
#' @param startdate Start date for award date to search. Accepted date format is mm/dd/yyyy (ex.12/31/2012)
#' @param enddate End date for award date to search. Accepted date format is mm/dd/yyyy (ex.12/31/2012)
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
nsf_return <- function(
	keyword = NULL,
	zipcode = NULL,
	agency = "NSF",
	statecode = NULL,
	startdate = NULL,
	enddate = NULL,
	verbose = TRUE,
	print_fields = print_fields_get(),
	save_file = NULL
) {
	base_url <- paste0(
		'https://api.nsf.gov/services/v1/awards.json?printFields=',
		paste0(print_fields, collapse = ","),
		"&"
	)
	url_parameters <- c()
	years <- c()

	if (!is.null(keyword)) {
		url_parameters <- paste0(
			c(url_parameters, paste0("keyword=", URLencode(keyword))),
			collapse = '&'
		)
	}
	if (!is.null(zipcode)) {
		url_parameters <- paste0(
			c(url_parameters, paste0("awardeeZipCode=", URLencode(zipcode))),
			collapse = '&'
		)
	}
	if (!is.null(agency)) {
		url_parameters <- paste0(
			c(url_parameters, paste0("agency=", URLencode(agency))),
			collapse = '&'
		)
	}
	if (!is.null(statecode)) {
		url_parameters <- paste0(
			c(
				url_parameters,
				paste0("awardeeStateCode=", URLencode(statecode))
			),
			collapse = '&'
		)
	}
	if (!is.null(startdate)) {
		url_parameters <- paste0(
			c(
				url_parameters,
				paste0("dateStart=", URLencode(startdate))
			),
			collapse = '&'
		)		
	}
	if (!is.null(enddate)) {
		url_parameters <- paste0(
			c(
				url_parameters,
				paste0("dateEnd=", URLencode(enddate))
			),
			collapse = '&'
		)
	}
	grants <- data.frame()
	try({
		grants <- data.frame(jsonlite::fromJSON(paste0(base_url, url_parameters)))
	}, silent=TRUE) # so it is quiet if this fails
	if (verbose) {
		print("Finished first batch")
	}
	offset <- 1
	local.grants <- grants
	if (!is.null(save_file)) {
		save(grants, file = save_file)
	}
	keep_running = TRUE
	while (nrow(local.grants) == 25 & keep_running) {
		keep_running <- FALSE
		try({
			offset <- offset + 25
			local.grants <- data.frame(jsonlite::fromJSON(paste0(
				base_url,
				url_parameters,
				'&offset=',
				offset
			)))
			if (nrow(local.grants) > 0 & ncol(local.grants) > 1) {
				keep_running <- TRUE
				years <- c(
					years,
					as.numeric(format(
						as.Date(
							local.grants$response.award.date,
							format = "%m/%d/%Y"
						),
						"%Y"
					))
				)

				#Sys.sleep(3)
				grants <- plyr::rbind.fill(grants, local.grants)
				if (verbose) {
					cat(paste0(
						"\rFinished next batch of ",
						nrow(local.grants),
						" records; now ",
						nrow(grants),
						" records from ",
						min(years, na.rm = TRUE),
						" to ",
						max(years, na.rm = TRUE)
					))
					if (!is.null(save_file)) {
						save(grants, file = save_file)
					}
				}
			}
		}, silent=TRUE)
	}
	colnames(grants) <- gsub('response.award.', '', colnames(grants))
	if (!is.null(save_file)) {
		save(grants, file = save_file)
	}
	if(nrow(grants) == 9976) {
		warning("This call to rnsf::nsf_return recovered the maximum number of grants (9976) it could recover from NSF's API -- this may mean that older grants are not included")	
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
#' This will *attempt* to get all information for a person. There are many potential problems with this. Multiple people could have the same name. People could also modify their names (with change in marital status, change in gender, whether they use a middle initial, and so forth) so do not use this naively to evaluate someone. That's what the h-index is for (I joke).
#' @param first_name Just the first name. For wildcards, use ".*"
#' @param middle_initial Only one letter, no periods. For wildcards, use ".*"
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
nsf_get_person <- function(first_name=".*", middle_initial=".*", last_name) {
  if(nchar(middle_initial)>1 & middle_initial!=".*") {
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
#' Since the returns are limited to 3,000 entries total, we will search by state, then aggregate. Progress as you go will be saved to save_file. You can also specify a startdate; this is mostly used internally for updating the cache.
#' 
#' There are over 500,000 NSF grants. This will take hours to run and result in a file almost a gigabyte in size.
#' 
#' @param save_file File to save results to while running
#' @param startdate Start date for award date to search. Accepted date format is mm/dd/yyyy (ex.12/31/2012)
#' @return A data frame with grant info
#' @export
nsf_get_all <- function(save_file="NSFAllGrants.rda", startdate=NULL) {
  grants <- data.frame()
  data(state_codes, package="USAboundaries")
  for(state_index in sequence(nrow(state_codes))) {
	try({
		if(nchar(state_codes$state_abbr[state_index])==2) {
			print(paste0("Now getting data for ", state_codes$state_name[state_index]))
			start_time <- Sys.time()
			if (!is.null(startdate)) {
				# we're just updating
				new_grants <- nsf_return(
					agency = "NSF",
					statecode = state_codes$state_abbr[state_index],
					startdate = startdate,
					save_file = NULL,
					verbose = TRUE
				)
			} else {
				# we're doing a full pull, but some states (CA, AZ) have too many grants to pull at once (CA has over 9976 for just 2017-2026), so we have to split it up
				new_grants <- data.frame()
				print(" ")
				for (year in rev(1960:as.numeric(format(Sys.Date(), "%Y")))) {
					try({
						year_grants <- nsf_return(
							agency = "NSF",
							statecode = state_codes$state_abbr[state_index],
							startdate = paste0("01/01/", year),
							enddate = paste0("12/31/", year),
							save_file = NULL,
							verbose = TRUE
						)
						if (nrow(year_grants) > 0) {
							new_grants <- plyr::rbind.fill(
								new_grants,
								year_grants
							)
						}
						cat(
							"\rYear: ",
							year,
							" number of grants: ",
							nrow(year_grants),
							" total: ",
							nrow(new_grants)
						)
					})
				}
			}
			if (nrow(new_grants) > 0) {
				grants <- plyr::rbind.fill(
					grants,
					new_grants
				)
			}
			end_time <- Sys.time()
			print(paste0("\nFound ", nrow(new_grants), " grants, making for ", nrow(grants), " grants in total"))
			print(end_time - start_time)
			save(grants, file = save_file)
		}
	}, silent=TRUE)
  }
  return(grants)
}

#' Update cached information
#'
#' Adds grants after the last cached ones. Once it is updated
#'
#' @examples
#' # not running as it's slow
#' # grants <- nsf_update_cached()
#' # usethis::use_data(grants, overwrite=TRUE) # if you are updating the package as well
#'
#' @return A data.frame with the original grants data and appended new data
#' @export
nsf_update_cached <- function() {
	data(grants)
	dates <- as.Date(grants$date, format = "%m/%d/%Y")
	most_recent <- max(dates, na.rm=TRUE)
	next_day <- most_recent+1
	grants_new <- nsf_get_all(startdate=format(next_day, format="%m/%d/%y"))
	if (nrow(grants_new) > 0) {
		grants <- plyr::rbind.fill(grants, grants_new)
	}
	print(paste0("\n\nIn total, ", nrow(grants_new), " were added, there are now ", nrow(grants), " grants total"))
	print("Remember, if you are using this to update the data in the package, assuming the results are output into an object called grants, use usethis::use_data(grants, overwrite=TRUE) to update the package")
	return(grants)
}

#' Grant information
#'
#' A dataset of NSF awards from its start until the package was last updated
#' @format A data frame with one row per award and columns with award information
"grants"


#' GRFP information
#'
#' A dataset of NSF GRFP awards and honorable mentions from its start until the package was last updated
#' @format A data frame with one row per award and columns with award information
"grfp"

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
    text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::removeWords, prune_words))
  }
  # Remove punctuations
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::removePunctuation))
  # Eliminate extra white spaces
  text_corpus <- suppressWarnings(tm::tm_map(text_corpus, tm::stripWhitespace))
  dtm <- tm::TermDocumentMatrix(text_corpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)[1:min(max_words, nrow(m))]
  d <- data.frame(word = names(v),freq=v)
  wordcloud::wordcloud(words = d$word, freq = d$freq, random.order = FALSE, ...)
}

#' Convert to academic year
#' 
#' Convert a date like 12/15/2024 to academic year (assumed to start on July 1)
#' 
#' @param date A date or vector of dates
#' @return a character string or vector with the academic year (i.e. "AY2024-2025")
#' @export 
#' @examples
#' dates <- as.Date(c("12/20/2024", "06/01/2021", "09/01/2021"), format="%m/%d/%Y")
#' print(cbind(format(dates, "%m/%d/%Y"), date_to_academic_year(dates)))
date_to_academic_year <- function(dates) {
	years <- as.numeric(format(dates, "%Y"))
	months <- as.numeric(format(dates, "%m"))
	years[which(months < 7)] <- years[which(months < 7)]-1
	results <- paste0("AY", years, "-", years+1)
	return(results)
}

#' Convert to academic semester
#' 
#' Convert a date like 12/15/2024 to academic semester (2024 Fall). Note that to make it sort correctly, "Spring" is preceded by two spaces so that "2025  Spring" is alphabetically before "2025 Fall".
#' 
#' @param date A date or vector of dates
#' @return a character string or vector with the academic year (i.e. "2024 Fall")
#' @export 
#' @examples
#' dates <- as.Date(c("12/20/2024", "06/01/2021", "09/01/2021"), format="%m/%d/%Y")
#' print(cbind(format(dates, "%m/%d/%Y"), date_to_academic_semester(dates)))
date_to_academic_semester <- function(dates) {
	years <- as.numeric(format(dates, "%Y"))
	months <- as.numeric(format(dates, "%m"))
	month_season <- rep(" Fall", length(months))
	month_season[which(months<=6)] <- "  Spring"
	results <- paste0(years, month_season)
	return(results)
}

#' Convert two-letter symbol to state or territory name
#'
#' Goes from "AZ" to "Arizona"
#'
#' @param x Vector of abbreviations
#' @return Vector of state or territory names
#' @export
abbreviation_to_state <- function(x) {
	data(state_codes, package = "USAboundaries")
	results <- rep(NA, length(x))
	for (i in sequence(length(results))) {
		results[i] <- state_codes$state_name[which(state_codes$state_abbr == x[i])]
	}
	return(results)
}

#' Collapse list columns
#' 
#' Some grants columns are lists as they may have multiple entries. This will collapse them with semicolons between each entry. Note that this will be SLOW on a large grants data.frame.
#' 
#' @param grants A data.frame of grant info
#' @return A data.frame of grant info where each column is a "normal" vector: character, numeric, etc.
#' @export 
flatten_list_columns <- function(grants) {
	grants2 <- data.frame(matrix(nrow=nrow(grants), ncol=ncol(grants)))
	for (i in sequence(ncol(grants))) {
		cat("\r Converting column ", i)
		grants2[, i] <- sapply(grants[, i], paste0, collapse = "; ")	
	}	
	return(grants2)
}

#' Save CSVs of grants by decade
#' 
#' This will save a CSV of all grants by decade (to make it easier to get the relevant files)
#' 
#' @param grants A data.frame of grant info
#' @param path Where to save the files
#' @return Nothing, but the files will appear in the correct directory
#' @export 
save_grant_csvs <- function(grants, path) {
	grants_flat <- flatten_list_columns(grants)
	grants_flat$year <- as.numeric(format(as.Date(grants_flat$date, format="%m/%d/%Y"), format="%Y"))
	grants_flat$academic_semester <- date_to_academic_semester(
		as.Date(grants_flat$date,
		format = "%m/%d/%Y")
	)
	year_range <- range(grants_flat$year, na.rm=TRUE)
	start_year <- 10*floor(year_range[1]/10)
	min_years <- seq(from=start_year, to = year_range[2], by=10)
	grants_flat <- subset(grants_flat, !is.na(grants_flat$year))
	for (min_year in sequence(min_years)) {
		max_year <- min_year+9
		grants_flat_local <- subset(grants_flat, year>=min_year & year<=max_year)
		write.csv(grants_flat_local, file.path(path, paste0("NSF_grants_", min_year, "_to_", max_year, ".csv")))
	}
	return(NULL)
}

#' Compile GRFP data from the external data in the package
#' 
#' NSF releases GRFP information separately from other grant information, in two excel sheets per year: one for honorable mention, one for awards. These are downloaded as "xls" files but are actually "tsv" files. I have manually downloaded them for every year and stored them in `inst/extdata`. This function will compile them into one document. Package developers can then cache it in the package. For most users, just `data(grfp)` will be all you need to do to load in the data.
#' @return A data.frame of all GRFP data
#' @export
#' @examples 
#' #Not run
#' #grfp <- compile_grfp()
#' #usethis::use_data(grfp, overwrite=TRUE) # if you are updating the package as well
compile_grfp <- function() {
	all_files <- list.files(
		system.file("extdata", package = "rnsf"),
		,
		full.names = TRUE,
		pattern = "Honorable.*|Awardee.*"
	)
	grfp <- data.frame()
	for (file_index in sequence(length(all_files))) {
		file_name <- all_files[file_index]
		award_level <- stringr::str_extract(file_name, "Awardee|HonorableMention")
		year <- stringr::str_match(file_name, "(\\d\\d\\d\\d)\\.tsv")[2]
		if(!is.na(award_level) & !is.na(year)) {
			entry <- read.delim(file_name)
			entry$year <- year
			entry$award_level <- award_level
		}
		grfp <- plyr::rbind.fill(grfp, entry)
	}
	return(grfp)
}