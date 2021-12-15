data <- read.csv(choose.files())

head(data)

library(dplyr)

data_cl <- data %>% 
  select(-issued) %>% 
  select(-dataset) %>% 
  select(-theme)
#select unique titles

library(topicmodels)
data_LDA <- LDA(data_cl, k=2, control = list(seed=1234))


#titles
titles <- data_cl %>% 
  select(title) %>% 
  sample_n(size=500)
library(stringr)
library(stopwords)
library(tidyverse)

titles <- str_replace_all(titles, "[[:punct:]]", "")
titles <- tolower(titles)
titles <- titles[!duplicated(titles)]

stopwords_regex = paste(stopwords('German'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
titles_sort <- titles %>% 
  stringr::str_replace_all( stopwords_regex, '') %>% 
  str_squish()

#Splitting and displaying 5 most frequent words

titles_sort %>% 
  strsplit(" ") %>%
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(5)

#keywords
keywords <- data_cl %>%
  select(keyword) %>%
  sample_n(size=500)


keywords <-  str_replace_all(keywords, "[[:punct:]]", "")
keywords <-  tolower(keywords)

keywords %>%
  strsplit(" ") %>%
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(5)

#descriptions

descriptions <- data_cl %>% 
  select(description) %>% 
  sample_n(size=500)

descriptions <- str_replace_all(descriptions, "[[:punct:]]", "")
descriptions <- tolower(descriptions)
descriptions <- descriptions[!duplicated(descriptions)]

stopwords_regex = paste(stopwords('German'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
descriptions_sort <- descriptions %>% 
  stringr::str_replace_all( stopwords_regex, '') %>% 
  str_squish()

#Splitting and displaying 5 most frequent words

descriptions_sort %>% 
  strsplit(" ") %>%
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(5)
#seems like we got English descriptions as it is giving me English stopwords which I did not filter for
