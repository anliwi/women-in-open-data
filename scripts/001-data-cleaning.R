##### data cleaning ######

# Set working directory to source file location
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# UTF-8 lettering, enabling e.g. potential German 'Umlaute'
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# install necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,naniar)

# load dataset
raw_data <- read_csv("raw_data.csv")

# replace "character(0)" in tags with NA
raw_data <- raw_data %>%
  replace_with_na(replace = list(tags = "character(0)"))

##### clean the tags column #####

#remove "c(" from start of string
raw_data$tags <- sub('^c[(]', '', raw_data$tags)

#remove ")" from end of string
raw_data$tags <- gsub("[)]", "", raw_data$tags)

# create a new id column
clean_df <- raw_data[,2:6]
clean_df <- tibble::rowid_to_column(clean_df, "id")

write.csv(clean_df,paste0(getwd(),"/clean_data.csv"), row.names = FALSE)

