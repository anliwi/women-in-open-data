# UTF-8 lettering, enabling e.g. potential German 'Umlaute'
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, doParallel)

df <- read.csv("data/clean_data.csv")

##speeding up calculations -- set up of parallel core use

n_cores <- detectCores()-1
cl <- makeCluster(
  n_cores,
  type = 'PSOCK'
)

#write function to deregister the cores after use
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

#Register cluster
registerDoParallel(cl)

###tables and visualisations

#data sets in total -- with and without gender

total <- df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "weiblich|geschlecht|Geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  group_by(gendered) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100) %>%
  round(2)

total

##data sets per year -- with and without gender

#gendered and non-gendered

year <- df %>%
  mutate(date = lubridate::ymd(date)) %>% #add this in cleaning the data?
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "weiblich|geschlecht|Geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
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
  filter_at(.vars = vars(title, description, tags), .vars_predicate = any_vars(str_detect(., "weiblich|geschlecht|Geschlecht"))) %>%
  group_by(year = lubridate::year(date)) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100,
         cum_perc = cumsum(perc)) %>%
  round(2)

total_year <- df %>%
  group_by(year = lubridate::year(date)) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100,
         cum_perc = cumsum(perc)) %>%
  round(2)

fig <- full_join(gendered_year, total_year, by = "year", suffix = c(".gendered", ".total"))
fig[is.na(fig)] <- 0 #replaces all NA with 0 (only for gendered data sets)

fig %>%
  ggplot(aes(x = year)) +
  scale_x_continuous(breaks = seq(2013, 2021, 1)) +
  geom_point(aes(y = n.total), color = "black", fill = "black", shape = "o") +
  geom_line(aes(y= n.total), color = "black") +
  geom_point(aes(y = n.gendered), color = "red", fill = "red", shape = "o") +
  geom_line(aes(y = n.gendered), color = "red") +
  labs(title = "Number of gendered and total data sets per year",
       y = "number of data sets") +
  theme_minimal()

##datasets per topic
#weird error, fix later
  
df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "weiblich|geschlecht|Geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  group_by(gendered, groups) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100,
         cum_perc = cumsum(perc)) %>%
  round(2)















