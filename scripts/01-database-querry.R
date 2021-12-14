if (!require("pacman")) install.packages("pacman")
pacman::p_load(jsonlite, tidyjson, httr, doParallel, foreach)

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
df <- qd$results

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



################# Dinahs Tries #############



endp <- "https://www.govdata.de/ckan/api/action/current_package_list_with_resources"
q <- list()
resp <- resp <- GET(endp) # make the call
json <- fromJSON(content(resp, as = "text")) #all dataset names as a list 




##### GET THE TAGS #####
# reduce the json to the list of tags
json_tags <- json$result$tags

# function to get the names of the tags
get_tags <- function(x){
  list(my_tags = x$name)
} 

# apply the function to the reduced json
tags_list <- lapply(json_tags, get_tags)
tags_list

# create a dataframe of the tag names with the id of the element (needed for matching with other dataframes at the end)
df_tags <- dplyr::bind_rows(tags_list,.id = "id")
df_tags

##### GET THE TITLES #######

# get the titles in a dataframe
df_titles <- as.data.frame(json$result$title)
# change column name
colnames(df_titles)[1] <- "titles"

# add an id column
df_titles <- tibble::rowid_to_column(df_titles, "id")


##### GET THE GROUPS #####

# reduce the json to the list of groups
json_groups <- json$result$groups

# function to get the names of the groups
get_groups <- function(x){
  list(my_groups = x$display_name)
} 

# apply the function to the reduced json
groups_list <- lapply(json_groups, get_groups)

# transform empty values, if there are any, into NA to keep them 
groups_list <- lapply(groups_list, lapply, function(x)ifelse(is.null(x), NA, x))


# create a dataframe of the groups names with the id of the element (needed for matching with other dataframes at the end)
df_groups <- rbind.fill(lapply(groups_list, as.data.frame))
df_groups <- tibble::rowid_to_column(df_groups, "id")



##### GET THE DESCRIPTION #####

# get the descriptions in a dataframe
df_description <- as.data.frame(json$result$notes)
# change column name
colnames(df_description)[1] <- "description"

# add an id column
df_description <- tibble::rowid_to_column(df_description, "id")

###### TRY WITH BIG ELEMENT #####

# TRY WITH 1000'er LISTS

## set up parallel core use for getting the raw data
n_cores <- detectCores()-1
cl <- makeCluster(
  n_cores,
  type = 'PSOCK'
)

## write function to deregister the cores after use
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

## getting the meta data of all 55000 datasets
# Register cluster
registerDoParallel(cl)

# create empty list for json objects
x <- list()

# for loop to go over the urls and extend the limit each time and store the object in the json_list
# tryout with 1:5 objects - final version has to be with 1:55
json_list <- foreach(i=1:5,.packages = c('httr','jsonlite', 'tidyjson')) %dopar% {
  
  endp_one <- "https://www.govdata.de/ckan/api/action/current_package_list_with_resources?limit=1000&offset="
  endp_two <- as.character(1001+1000*(i-1))
  endp <- paste0(endp_one,endp_two)
  resp <- GET(endp)
  x <- fromJSON(content(resp, as = "text"))

}

# De-register cluster
registerDoSEQ()
unregister_dopar()


######### GETTING THE RELEVANT DATA OUT OF THE JSON OBJECTS #########

## specify all the functions needed
# get the tags
get_tags <- function(x){
  list(my_tags = x$name)
} 

# get the groups
get_groups <- function(x){
  list(my_groups = x$display_name)
} 

## create empty dataframe with specified columns


## building loops to get out the needed attributes and combine them in a list

test_df <- for (i in json_list) {
  json_tags <- [[i]]$result$tags
  
}



