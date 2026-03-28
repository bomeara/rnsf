# rnsf

Unofficial package to interface with NSF API

It also has abstracts, dates, and more information for all grants up to March 28, 2026. 

* Webpage with package information: <https://bomeara.github.io/rnsf/>
* Github page: <https://github.com/bomeara/rnsf/>

To install the package:

```
install.packages("remotes")
remotes::install_github("bomeara/rnsf")
```

To get all grant data:

```
library(rnsf)
data(grants)
```

This will give you a data.frame with all data: `head(grants)`

It has multiple date fields which are in "%m/%d/%Y" format (but everything is stored as a raw list, leaving to the user to process it). 

You can also do a new search. For example, to get all info on ants:

```
ants <- rnsf::nsf_return(keyword="Formicidae")
```

And there is a function for doing a wordcloud:

```
nsf_wordcloud(ants$abstractText)
```

We can plot funding over time, inspired by Jeremy Berg's graphs on federal funding (see <https://jeremymberg.github.io/jeremyberg.github.io/>)

```
library(rnsf)
library(ggplot2)
library(timeDate)
library(dplyr)

data(grants) # or use grants <- rnsf::nsf_get_all() to get the most current list
grants$date <- as.Date(grants$date, format="%m/%d/%Y")
grants$year <- format(grants$date, "%Y")
grants$dayofyear <- timeDate::dayOfYear(timeDate::timeDate(grants$date))
grants$estimatedTotalAmt <- as.numeric(grants$estimatedTotalAmt)
grants <- grants |> arrange(year, dayofyear) |> group_by(year) |> mutate(estimatedTotalAmt_by_year = cumsum(estimatedTotalAmt)) |> ungroup()
grants <- grants[!is.na(grants$year),]
g <- ggplot(grants, aes(x=dayofyear, y=estimatedTotalAmt_by_year/1e9, colour=year)) + geom_line() + theme_minimal() + xlab("Day of year") + ylab("Cumulative billions of dollars awarded")
print(g)
```