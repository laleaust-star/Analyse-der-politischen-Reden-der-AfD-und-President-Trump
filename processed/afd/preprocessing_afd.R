library(quanteda)
library(dplyr)
library(tibble)
library(tidyverse)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(stopwords)

documents_afd <- list.files("dataset/raw/afd", pattern="\\.txt$", full.names = TRUE)

sitzungen <- tibble(
  docs = basename(documents_afd),
  text = purrr::map_chr(documents_afd, readr::read_file)
)

sitzungen <- sitzungen %>%
  mutate(text = tolower(text)) %>%
  mutate(text = str_remove_all(text, "\\s*\\([^)]*\\)")) %>%
  mutate(text = str_replace_all(text, "[^a-zäöüß\\.\\!\\? ]", " "))

tokens_afd <- sitzungen %>%
  unnest_tokens(word, text)

custom_stopwords <- c("damen", "herren", "vielen", "dank", "heute",
                      "beifall", "afd", "cdu", "csu", "zuruf", "die", "linke",
                      "spd", "minister", "ministerin", "abgeordnete","präsident",
                      "präsidentin", "seit", "merz", "jahr", "deutschland",
                      "dass", "mensch", "seit", "müssen", "jahren", "gibt",
                      "prozent", "geht", "herr", "dafür", "mehr", "immer",
                      "davon", "endlich", "schon", "kommen", "lassen", "bundestag",
                      "statt", "nein", "darf", "kollegen", "bereits", "grünen",
                      "artikel", "bleibt", "dobrindt", "berlin", "dabei", "fünf",
                      "wurden", "zehn")

tokens_clean <- tokens_afd %>%
  filter(!word %in% stopwords::stopwords("de")) %>%
  filter(!word %in% custom_stopwords) %>%
  filter(nchar(word) > 3)

word_counts <- tokens_clean %>%
  count(docs, word, sort = TRUE)

