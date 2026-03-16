library(quanteda)
library(dplyr)
library(tibble)
library(tidyverse)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(stopwords)

documents_trump <- list.files("dataset/raw/trump", pattern="\\.txt$", full.names = TRUE)

reden <- tibble(
  docs = basename(documents_trump),
  text = purrr::map_chr(documents_trump, readr::read_file)
)

reden <- reden %>%
  mutate(text = tolower(text)) %>%
  mutate(text = str_remove_all(text, "\\s*\\([^)]*\\)")) %>%
  mutate(text = str_replace_all(text, "[^a-z]", " "))

tokens_trump <- reden %>%
  unnest_tokens(word, text)

custom_stopwords <- c("mr", "mrs", "thank", "you", "today",
                      "laugther", "president", "people", "know", "going", "want",
                      "just", "like", "think", "much", "well", "right", "good",
                      "laughter", "said", "years", "really", "even", "back", "baier",
                      "time", "done", "make", "secretary", "many", "general", "spoke")

tokens_clean <- tokens_trump %>%
  filter(!word %in% stopwords::stopwords("en")) %>%
  filter(!word %in% custom_stopwords) %>%
  filter(nchar(word) > 3)

word_counts <- tokens_clean %>%
  count(docs, word, sort = TRUE)
