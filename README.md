# rnsf

Unofficial package to interface with NSF API

It also has abstracts, dates, and more information for all grants up to March 28, 2026. 

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