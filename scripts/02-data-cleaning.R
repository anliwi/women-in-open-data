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
  summarise(n = n())

