#packages

library(tidyverse)
library(lubridate)


#data
df <- read.csv("data/queryResults-limit-700000.csv")

head(df, 50)

########data cleaning

#dates <- df$issued
#dates1 <- str_extract(dates, "^\\d{4}")


#extracting year without the pipe operator becausue it is not working (see below). I think the problem is with vector/column 
df$year <- str_extract(df$issued, "^\\d{4}")


###df %>%
###  mutate(year = str_extract(issued, "^\\d{4}")) %>%
###  head(df)


#number of datasets per year -- not sure if year in df refers to the year added, because there are some ancient dates

df %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 1999) %>%
  group_by(year) %>% 
  summarise(n = n_distinct(title)) %>%
  print(n = 30) 
  ggplot(aes(x = year, y = n)) +
  geom_line()
  


#total and gendered datasets per year - no gendered data sets anywhere but in 2018. 
#but we don't have all datasets in the df, we need to figure it out better
#we need to look into filtering geder out of multiple columns (title, description)

df %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 1999) %>%
  mutate(gendered = if_else(
    str_detect(keyword, "weiblich|geschlecht|Geschlecht"), "gendered", "total")) %>%
  group_by(year, gendered) %>% 
  summarise(n = n_distinct(title)) %>%
  spread(gendered, n) %>%
  mutate(gendered = replace_na(gendered, 0)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = gendered), color = "red") +
  geom_line(aes(y = total), color = "black")

######the same figure as above (total and gendered datasets per year), but with looking for "gender" in several columns.
#I didn't manage to figure it out if_else, so I filter it in one dataframe and use the other for total.

year <- df %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 1999) %>%
  group_by(year) %>% 
  summarise(n = n_distinct(title))  

year_gender <- df %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 1999) %>%
  filter_at(.vars = vars(title, description, keyword),
            .vars_predicate = any_vars(str_detect(., "weiblich|geschlecht|Geschlecht"))) %>%
  group_by(year) %>%
  summarise(n = n_distinct(title))

fig1 <- full_join(year, year_gender, by = "year", suffix = c(".total", ".gendered"))
fig1$n.gendered <- replace_na(fig1$n.gendered, 0)
  
fig1 %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y= n.total), color = "black") +
  geom_line(aes(y = n.gendered), color = "red")
  

