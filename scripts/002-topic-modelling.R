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
clean_df <- read_csv("clean_data.csv")

# reduce dataframe to id and column that should be analyzed
test_df <- clean_df[,c("id","tags")]

# transform it into a quanteda corpus
clean_corp <- corpus(test_df,docid_field = "id",text_field = "tags")

# tokenize the corpus
clean_tokens <- tokens(clean_corp)

# transform tokens object into document feature matrix
clean_dfm <- dfm(clean_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))

# set the number of themes
anzahl.themen <- 10
# transform dfm into form that is suitable for topicmodel package
dfm2topicmodels <- convert(clean_dfm, to = "topicmodels")
# perform the LDA
lda.modell <- LDA(dfm2topicmodels, anzahl.themen)
# show 10 most related words per topic in a dataframe
ergebnis_lda <- as.data.frame(terms(lda.modell, 10))
ergebnis_lda
