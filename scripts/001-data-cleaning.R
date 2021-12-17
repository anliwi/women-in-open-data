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
raw_data <- read_csv("data/raw_data.csv")

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

#removes punctuation, lowercase
#could use a tweak -- do we remove numbers? do we remove all punctuation apart from hyphen between words?

clean_df <- clean_df %>%
  mutate(across(
    .cols = title:tags, .fns = ~ str_replace_all(
      ., "[[:punct:]](?!\\w)", " ") #removes all punctuation that is not followed by a character
  )) %>%
  mutate(across(
    .cols = title:tags, .fns = ~ str_replace_all(
      ., '\\"|\\_|\\(', " ") #remove remaining " , _ and (
  )) %>%
  mutate(across(
    .cols = title:tags, .fns = ~ str_squish(.) #removes double whitespace
  )) %>%
  mutate(across(
    .cols = title:tags, .fns = ~ str_to_lower(.)
  ))

#writes data as csv
write.csv(clean_df,paste0(getwd(),"/data/clean_data.csv"), row.names = FALSE)

