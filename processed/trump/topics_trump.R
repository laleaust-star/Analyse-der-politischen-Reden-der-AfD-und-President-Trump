library(dplyr)
library(stringr)
library(ggplot2)

immigrants <- c(
  "immigrant", "immigration", "refugee", "asylum",
  "migrant", "mexico", "border", "illegal", "dreamers",
  "deport", "visa","canada", "aliens", "ukraine", "afghanistan", "syria", "afd"
)

picture <- c(
  "terror", "crime", "criminal", "illegal", "problem",
  "danger", "threat", "attack", "lawbreaker", "violence",
  "drug", "gang", "cartels", "gun", "guns", "knife"
)

politics <- c(
  "policy", "law", "government", "election", "vote",
  "congress", "cabinet", "borderwall", "strict", "open", "help", "save",
  "trade", "economy", "tax", "budget", "deal", "plan", "border"
)

immigrants_reden <- reden %>%
  filter(str_detect(text, str_c(immigrants, collapse = "|")))

immigrants_reden %>% select(docs)

tokens_immigrants <- immigrants_reden %>%
  unnest_tokens(word, text) %>%
  filter(nchar(word) > 3)

tokens_immigrants <- tokens_immigrants %>%
  mutate(category = case_when(
    word %in% immigrants ~ "immigrants",
    word %in% picture ~ "picture",
    word %in% politics ~ "politics"
  ))

tokens_immigrants %>%
  count(category, word, sort = TRUE) %>%
  group_by(category) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~category, scales = "free")

