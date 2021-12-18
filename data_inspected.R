data <- read.csv(choose.files())

#head(data)

library(dplyr)

#data_cl <- data %>% 
#  select(-date) %>% 
#  select(-id) %>% 
#  select(-tags)
#select unique titles

library(stringr)
library(stopwords)
library(tidyverse)
library(tidyr)
library(tm)

#data_m <- as.matrix(data_cl)

###cleaning data

#class(data$groups)

#data_v2 <- data %>% 
#  mutate(data$title = replace(data$title, str_detect(data$title, "Ã¤|Ã„"), "ae"))
  
  
 #sample_n(500) %>% 
 #str_replace_all(pattern = "Ã¼r", replacement = "ue") %>% 
 #as.data.frame()  
  

#mutate(food = replace(food, str_detect(food, "fruit"), "fruit"))



#data_join_later <- data$tags %>% 
#  unlist()
 
###


#will_d <- data_cl %>% 
 # summarise(group_by(row_number()))



library(topicmodels)
#data_LDA <- LDA(data_m, k=2, control = list(seed=1234))
#this is still not working and reproducing the same error"Error in !all.equal(x$v, as.integer(x$v)) : invalid argument type
#In addition: Warning message:
 # In mode(current) : NAs introduced by coercion

#titles
#titles <- data %>% 
#  select(title) # %>% 
# # sample_n(size=500)


#titles <- str_replace_all(titles, "[[:punct:]]", "")
#titles <- tolower(titles)
#titles <- titles[!duplicated(titles)]
#
stopwords_regex = paste(stopwords('German'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
#titles_sort <- titles %>% 
#  stringr::str_replace_all( stopwords_regex, '') %>% 
#  str_squish()

#Splitting and displaying 5 most frequent words

#titles_sort %>% 
#  strsplit(" ") %>%
#  unlist() %>% 
#  table() %>% 
#  sort(decreasing = TRUE) %>% 
#  head(10)

#keywords
keywords <- data %>%
  select(tags)


keywords <-  str_replace_all(keywords, "[[:punct:]]", "")
keywords <-  tolower(keywords)

keyword_freq <- keywords %>%
  strsplit(" ") %>%
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(100)

keyword_freq <- as.data.frame(keyword_freq)  
keyword_freq$index <- 1:nrow(keyword_freq)

names(keyword_freq) <-  c("term", "frequency")
keyword_freq <-  keyword_freq[keyword_freq$term %in% c('bebauungsplan','strandbelegung', 'scharbeutz', 'futtermittel','pkw','lkw','bundesstrasse','geschlecht','weiblich', 'covid19'), ]

library(ggplot2)

bar <- ggplot(data = keyword_freq) + 
  geom_bar(
    mapping = aes(x = frequency, fill = term), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_polar()
#descriptions

groups_df <- data %>%
  mutate(wt = str_detect(groups,"wissenschaft und technologie")) %>%
  mutate(gb = str_detect(groups,"bevoelkerung und gesellschaft")) %>%
  mutate(bks = str_detect(groups,"bildung kultur und sport")) %>%
  mutate(en = str_detect(groups,"energie")) %>%
  mutate(ge = str_detect(groups,"gesundheit")) %>%
  mutate(it = str_detect(groups,"internationale themen")) %>%
  mutate(lfn = str_detect(groups,"landwirtschaft fischerei forstwirtschaft und nahrungsmittel")) %>%
  mutate(ros = str_detect(groups,"regierung und oeffentlicher sektor")) %>%
  mutate(rs = str_detect(groups,"regionen und staedte")) %>%
  mutate(um = str_detect(groups,"umwelt")) %>%
  mutate(ve = str_detect(groups,"verkehr")) %>%
  mutate(wf = str_detect(groups,"wirtschaft und finanzen")) %>%
  mutate(jros = str_detect(groups,"justiz rechtssystem und oeffentliche sicherheit"))

# transform logical to numeric
groups_df[,7:19] <- lapply(groups_df[,7:19], as.numeric)

data$description[data$description==''] <- NA

descriptions <- groups_df %>% 
  filter(str_detect(description, "geschlecht|weiblich|maennlich|frau")) %>% 
  filter(ge==1) %>% 
  select(description)

#as.data.frame(description)
 # filter_all("geschlecht|weiblich|maennlich|frau")

descriptions <- str_replace_all(descriptions, "[[:punct:]]", "")
descriptions <- tolower(descriptions)
#descriptions <- descriptions[!duplicated(descriptions)]

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
  head(100)
#seems like we got English descriptions as it is giving me English stopwords which I did not filter for
