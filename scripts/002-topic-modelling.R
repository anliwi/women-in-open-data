##### data analysis ######

# Set working directory to source file location
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# UTF-8 lettering, enabling e.g. potential German 'Umlaute'
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# install necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,topicmodels,quanteda)

# load dataset
clean_df <- read_csv("data/clean_data.csv")

groups_df <- clean_df %>%
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

##### prepare data for topic analysis #####

## 1 topic model compare all gender non-containing datasets with all gender containing datasets

full_df <- groups_df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE))


# divide datasets

full_gender <- subset(full_df, gendered == 1, select = c(1:6))
full_gender <- full_gender[,2:6]
full_gender <- tibble::rowid_to_column(full_gender, "id")

full_nongender <- subset(full_df, gendered == 0, select = c(1:6))
full_nongender <- full_nongender[,2:6]
full_nongender <- tibble::rowid_to_column(full_nongender, "id")


# reduce datasets to id and description
gender_tpm1_df <- full_gender[,c("id","title")]
nongender_tpm1_df <- full_nongender[,c("id","title")]


# transform both  into quanteda corpora
gender_tpm1_corp <- corpus(gender_tpm1_df,docid_field = "id",text_field = "title")
nongender_tpm1_corp <- corpus(nongender_tpm1_df,docid_field = "id",text_field = "title")


# tokenize the corpus
gender_tpm1_tokens <- tokens(gender_tpm1_corp)
nongender_tpm1_tokens <- tokens(nongender_tpm1_corp)


# transform tokens object into document feature matrix
gender_tpm1_dfm <- dfm(gender_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))
nongender_tpm1_dfm <- dfm(nongender_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))


# set the number of themes
number.themes <- 10


# transform dfm into form that is suitable for topicmodel package
gender_tpm1 <- convert(gender_tpm1_dfm, to = "topicmodels")
nongender_tpm1 <- convert(nongender_tpm1_dfm, to = "topicmodels")


# perform the LDA
gender_lda.model <- LDA(gender_tpm1, number.themes)
nongender_lda.model <- LDA(nongender_tpm1, number.themes)

# show 10 most related words per topic in a dataframe
results_gender_lda <- as.data.frame(terms(gender_lda.model, 5))
results_nongender_lda <- as.data.frame(terms(nongender_lda.model, 5))
