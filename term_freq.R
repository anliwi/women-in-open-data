data <- read.csv(choose.files())

library(stringr)
library(stopwords)
library(tidyverse)
library(tidyr)
library(dplyr)

stopwords_regex = paste(stopwords('German'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

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
library(ggthemes)


 
tf_plot <-ggplot(keyword_freq, 
       aes(x=frequency, 
           y=reorder(term, frequency))) +
  geom_point(color="#800000", 
             size = 3) +
  geom_segment(aes(x = 40, 
                   xend = frequency, 
                   y = reorder(term, frequency), 
                   yend = reorder(term, frequency)),
               color = "grey") +
  labs (x = "Term Frequency",
        y = "",
        title = "Term Frequency on govdata.de",
        subtitle = "Gotta set priorities",
        caption = "out of over 54000 datasets") +
  theme_gray(base_size=8)+ 
  scale_color_manual(values = c("#800000", "#808080")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave("tf_plot.png", width = 5, height = 4, dpi = 300, tf_plot)

