# UTF-8 lettering, enabling e.g. potential German 'Umlaute'
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, stopwords)

df <- read.csv("data/clean_data.csv")

df <- df %>% mutate(date = lubridate::ymd(date))

#data sets in total -- with and without gender

total <- df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  group_by(gendered) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100) %>%
  round(2)

total

####data sets per year -- with and without gender

#table - gendered vs non-gendered

year <- df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  group_by(gendered, year = lubridate::year(date)) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100,
         cum_perc = cumsum(perc)) %>%
  round(2)


year %>%
  ggplot(aes(x = year, y = cum_perc, colour = gendered)) +
  geom_point(aes(color = factor(gendered)), shape = "o", size = 2) +
  scale_x_continuous(breaks = seq(2013, 2021, 1)) +
  geom_line(aes(color = factor(gendered))) +
  labs(title = "Cummulative percentage of gendered and non-gendered data sets per year",
       y = "cummulative percentage") +
  theme_minimal()

#gendered and total

gendered_year <- df %>%
  filter_at(.vars = vars(title, description, tags), 
            .vars_predicate = any_vars(str_detect(., "frauen|weiblich|geschlecht"))) %>%
  group_by(year = lubridate::year(date)) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100,
         cum_perc = cumsum(perc),
         cum_n = cumsum(n)) %>%
  round(2)

total_year <- df %>%
  group_by(year = lubridate::year(date)) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100,
         cum_perc = cumsum(perc),
         cum_n = cumsum(n)) %>%
  round(2)

fig <- full_join(gendered_year, total_year, by = "year", suffix = c(".gendered", ".total"))
fig[is.na(fig)] <- 0 #replaces all NA with 0 (only for gendered data sets)

fig %>% #plots total n
  ggplot(aes(x = year)) +
  scale_x_continuous(breaks = seq(2013, 2021, 1)) +
  geom_point(aes(y = n.total), color = "black", fill = "black", shape = "o") +
  geom_line(aes(y= n.total), color = "black") +
  geom_point(aes(y = n.gendered), color = "red", fill = "red", shape = "o") +
  geom_line(aes(y = n.gendered), color = "red") +
  labs(title = "Number of gendered and total data sets per year",
       y = "number of data sets") +
  theme_minimal()

fig %>% #plots cummulative n
  ggplot(aes(x = year)) +
  scale_x_continuous(breaks = seq(2013, 2021, 1)) +
  geom_point(aes(y = cum_n.total), color = "black", fill = "black", shape = "o") +
  geom_line(aes(y= cum_n.total), color = "black") +
  geom_point(aes(y = cum_n.gendered), color = "red", fill = "red", shape = "o") +
  geom_line(aes(y = cum_n.gendered), color = "red") +
  labs(title = "Cumulative number of gendered and total data sets per year",
       y = "number of data sets") +
  theme_minimal()

##datasets per topic

# add columns per topic to check whether topic is present in dataset
groups_df <- df %>%
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


topic <- df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  mutate(groups = as.factor(groups),
         gendered = as.factor(gendered)) %>%
  group_by(groups, gendered) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n),
         perc = freq * 100) %>%
  drop_na() %>%
  mutate(across(.cols = freq:perc, .fns = ~ round(., 2)))

topic


ggplot(topic, aes(fill=gendered, order=gendered, y=perc, x=groups)) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()

##words that separate topics

#which topic are most common

topic %>%
  filter(gendered == 1 & n >= 300) 

#importing german stop words
stop_german <- data.frame(word = stopwords::stopwords("de"), stringsAsFactors = FALSE)


#tidy text data frame
tidy_description <-
  df %>%
  filter(groups == "Bevölkerung und Gesellschaft") %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>%
  mutate(gendered = as.factor(gendered)) %>%
  ungroup() %>%
  #select(id, groups, gendered, description) %>%
  unnest_tokens(word, description) %>%
  count(gendered, word, sort = TRUE)

#removing stop words
tidy_description_stop <- tidy_description %>%
  anti_join(stop_german)

#getting tf_idf scores
description_tf_idf <- tidy_description_stop %>%
  bind_tf_idf(word, gendered, n)

#plot -- need to take out gendered words from gendered data-sets, and probably numbers from others???
library(forcats)

description_tf_idf %>%
  group_by(gendered) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = gendered)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gendered, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)





