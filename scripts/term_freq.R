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

keyword_freq2 <-  keyword_freq[keyword_freq$term %in% c('bebauungsplan', 'tourismus'  ,'bodennutzung', 'futtermittel', 'strandbelegung','bundesstrasse','geschlecht', 'weiblich'), ]



library(ggplot2)
library(ggthemes)




ggplot(keyword_freq, 
       aes(x=frequency, 
           y=reorder(term, frequency))) +
  geom_point(color="red", 
             size = 2) +
  geom_segment(aes(x = 40, 
                   xend = frequency, 
                   y = reorder(term, frequency), 
                   yend = reorder(term, frequency)),
               color = "lightgrey") +
  labs (x = "term frequency",
        y = "",
        title = "term frequency in open data",
        subtitle = "Gotta set priorities") +
  theme_wsj(base_size=8)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#Other possiblity

library(ggrepel)



d <- as.tibble(keyword_freq2[0:2])
d <- d %>% mutate(x = 1,
         term = as.character(term),
         color = ifelse(str_detect(term, "geschlecht|weiblich"), "#800000", "#808080"))
  
eng <- c("development plan", "tourism", "land use", "feed", "beach occupacy", "federal road", "gender", "female")

dd <- cbind(d, eng)

fig3a <- ggplot(dd, aes(x = 1.001, y = frequency)) +
  scale_size(range = c(2, 12)) +
  geom_text(aes(size = frequency, label = eng, color = color),
                   hjust = 0) +
  xlim(1, 1.1) +
  scale_color_manual(values = c("#800000", "#808080")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_text(colour="white"),
        axis.text.x=element_text(colour="white"),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.y = element_text(margin = margin(r = 0)),
        ) +
  geom_segment(aes(x = 1.06, y = 1000, xend = 1.02, yend = 1000, color = "#800000"),
               arrow = arrow(length = unit(2, "mm"))) +
  geom_vline(xintercept = 1) +
  labs(title = "Gotta set priorities",
       y = "Term number in the portal")

ggsave("outputs/datasets-yearly.png", dpi = 400, fig3a)
