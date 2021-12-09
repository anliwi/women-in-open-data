if (!require("pacman")) install.packages("pacman")
pacman::p_load(jsonlite, tidyjson, httr)

#this will be deleted later, if API works out fine
if (!require("pacman")) install.packages("pacman")
pacman::p_load(SPARQL, XML, RCurl)

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

################USING API

#getting titles of datasets

endp <- "https://www.govdata.de/ckan/api/action/package_list"
resp <- resp <- GET(endp) # make the call
titles <- fromJSON(content(resp, as = "text")) #all dataset names as a list 

titles_df <- dplyr::as_tibble(titles[[3]])


#trial with all datasets resources

endp <- "https://www.govdata.de/ckan/api/action/current_package_list_with_resources"
q <- list()
resp <- resp <- GET(endp) # make the call
json <- fromJSON(content(resp, as = "text")) #all dataset names as a list 


#geting data out of json object
json$result$title
as.data.frame(json$result$groups)["name"]
json$result$tags

d <- as.data.frame(json$result$tags[[7]])
d <- unlist(as.list(d["name"])$name)
d

d1 <- as.vector(json$result$tags[[2]])
d1

d1_vector <- unlist(d1$name)
d1_vector

as.list(json$result$tags)

#######doesn't work

tags <- function(x){
  d <- as.data.frame()
}



#first unlist
tags_to_list <- function(x){
  as.list(x$result$tags)
}

l <- tags_to_list(json)
l$name

names_to_char <- function(x){
  unlist(x$name)
}

l1 <- names_to_char(l)


library(plyr)
ldply(json$result$tags)


# now we make a function that can get these two things for any element of res
get_results <- function(x){
  list(title = x$result$title, 
       name = x$result$name) 
} 

get_results_2 <- function(x){
  list(groups = unlist(as.list(x$result$groups)$name), 
       tags = unlist(as.list(x$result$tags)$name))
} 

# check it runs on that single element 
get_results(json)

# Now apply this new function to *all* the elements of res and return them as another list
proc <- lapply(json, get_results_2)
proc

# Finally, collapse this thing into a data frame
df <- dplyr::bind_rows(get_results(json))
# check we got what we expected
df






