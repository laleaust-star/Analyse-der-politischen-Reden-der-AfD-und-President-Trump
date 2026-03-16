library(quanteda)
library(dplyr)
library(tibble)
library(tidyverse)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(stopwords)

dtm_trump <- cast_dtm(word_counts,
                      document = docs,
                      term = word,
                      value = n)

lda_trump <- LDA(dtm_trump, k = 3, control = list(seed = 123))


topics_trump <- tidy(lda_trump, matrix = "beta")

themes_trump <- topics_trump %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

themes_trump %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
