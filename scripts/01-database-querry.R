if (!require("pacman")) install.packages("pacman")
pacman::p_load(SPARQL, XML, RCurl)


####example from the page, trial

#STEP 1 - Set up URL and define query

#define the "endpoint
endpoint <- "https://www.govdata.de/sparql"

#create query statement
query <- "

PREFIX dct: <http://purl.org/dc/terms/>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
SELECT * WHERE {
  ?dataset dct:title ?title . #dataset title
  ?dataset dcat:keyword ?keyword . #dataset keywords
OPTIONAL {
?dataset dct:issued ?issued . #release date
?dataset dcat:theme ?theme . #theme according to eurostat
?dataset dct:description ?description . #dataset description
}
}
LIMIT 5000

"


###STEP 2 - Use SPARQL to submit query and save results

qd <- SPARQL(endpoint, query)
df <- gd$results