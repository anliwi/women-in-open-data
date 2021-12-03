#############################################
# Final Project for Intro to Data Science ###
# December 19, 2021 #########################
# created by dinah r, anna-lisa w, helena b #
#############################################

# Content 

# 1. Basic Housekeeping
# Load all relevant libraries and install packages



# 2. Functions
# define function to retrieve relevant information from RDF files


###########################
## 1. BASIC HOUSEKEEPING ##
###########################

# set working directory!
setwd("/Users/dinah/Dropbox/Schule und Uni/Hertie/03 Semester 3/Intro to Data Science/assignments/data-project-invisible_women/")
getwd()

# Load all relevant libraries and install packages hosted on GitHub

library(RWDataPlyr)
library(rdflib)
library(tidyverse)
library(XML)
library(xml2)


# try to import it as RDF --> either didnt work or resulted in a weird format

read_rdf("bundestagswahl-2021-dormagen-wahlbezirk.rdf", rdf = TRUE)

example_file <- rdf_parse("bundestagswahl-2021-dormagen-wahlbezirk.rdf")

print(example_file)

doc <- system.file("bundestagswahl-2021-dormagen-wahlbezirk.rdf", package="redland")
rdf <- rdf_parse("bundestagswahl-2021-dormagen-wahlbezirk.rdf", format = "rdfxml") 


### Then i tried treating it like a XML file
# that worked
data <- xmlParse(file = "bundestagswahl-2021-dormagen-wahlbezirk.rdf")
xml_data <- xmlToList(data)

# trying to manually extract information
title <- as.list(xml_data[["Dataset"]][["title"]])
titles <- unlist(title[[1]])

keyword <- as.list(xml_data[["Dataset"]][["keyword"]])
keywords <- unlist(keyword[[1]])

# try finding all
x <- read_xml("bundestagswahl-2021-dormagen-wahlbezirk.rdf")
keys <- xml_find_all(x,".keyword")


last_mod <- as.list(xml_data[["Dataset"]][["modified"]][["text"]])
last_modified <- unlist(last_mod[[1]])

# try to modify the document into a usable one

data_2 <- rdf_parse("bevolkerung-in-deutschland-nach-alter-und-geschlechtea228.rdf", format = "rdfxml")
xml_2 <- xmlToList(data_2)

dat_2 <- as.list(data_2[["doc"]])

subset <- as.list(xml_2[["Dataset"]])
keys <- unlist(subset[["keyword"]])

