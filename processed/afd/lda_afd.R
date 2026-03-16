library(quanteda)
library(dplyr)
library(tibble)
library(tidyverse)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(stopwords)


dtm_afd <- cast_dtm(word_counts,
                    document = docs,
                    term = word,
                    value = n)

lda_afd <- LDA(dtm_afd, k = 3, control = list(seed = 123))

topics_word <- tidy(lda_afd, matrix = "beta")%>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  summarise(words = list(term))

themes_afd <- tidy(lda_afd, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

ggplot(themes_afd, aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  labs(x = "Beta", y = NULL)
