library(quanteda)
library(dplyr)
library(tibble)
library(tidyverse)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(stopwords)

migranten <- c(
  "ausländer", "migrant", "geflüchtete", "asylant", "afghanen", "afghanistan",
  "syrer", "syrien", "ukrainer", "ukraine", "flüchtlinge", "asylbeantrager",
  "asylmigranten", "mexico", "visum", "illegal", "illegale", "fremdarbeiter"
)

feindbild <- c(
  "täter", "straftat", "messer", "problem", "gefahr", "friedlich",
  "chancengleichheit", "verantwortung", "aufnahme", "illegal", "rechtsmässig",
  "gefährdung", "waffen", "waffe", "kartel", "banden", "drogen"
)

politik <- c(
  "einbürgerung", "grenze", "grenzen", "zurückweisung", "vorsatz",
  "asyl", "asylpolitik", "lockern", "offen", "öffnen", "aufnehmen", "aufnahme",
  "begrenzung", "begrenzen", "streng", "strenger", "schützen", "trump", "industrie",
  "wirtschaft", "steuern", "budget", "finanzen", "import", "export", "handel"
)

migranten_reden <- sitzungen %>%
  filter(str_detect(text, str_c(migranten, collapse = "|")))

migranten_reden %>% select(docs)

tokens_migranten <- migranten_reden %>%
  unnest_tokens(word, text) %>%
  filter(nchar(word) > 3)

tokens_migranten <- tokens_migranten %>%
    mutate(category = case_when(
      word %in% migranten ~ "Migranten",
      word %in% feindbild ~ "Feindbild",
      word %in% politik ~ "Politik"
  ))

tokens_migranten %>%
  count(category, word, sort = TRUE) %>%
  group_by(category) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~category, scales = "free")

