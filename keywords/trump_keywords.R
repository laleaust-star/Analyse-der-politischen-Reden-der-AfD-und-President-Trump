library(dplyr)
library(readr)
library(stringr)
library(tidyr)

weg_trump <- "dataset/raw/trump"

dateien_trump <- list.files(weg_trump, pattern = "\\.txt$", full.names = TRUE)

reden_trump <- data.frame(
  datei = basename(dateien_trump),
  text = sapply(dateien_trump, read_file),
  stringsAsFactors = FALSE
)

sätze_trump <- reden_trump %>%
  rowwise() %>%
  mutate(saetze = str_split(text, "(?<=[.!?])\\s+")) %>%
  unnest(saetze) %>%
  mutate(saetze = str_trim(saetze)) %>%
  group_by(datei) %>%
  mutate(satz_num = row_number()) %>%
  ungroup()


suchwort_trump1 <- "ukraine"

kontext_blöcke_trump1 <- sätze_trump %>%
  filter(str_detect(saetze, regex(paste0("\\b", suchwort_trump1, "\\b"), ignore_case = TRUE))) %>%
  rowwise() %>%
  mutate(
    start = max(satz_num - 1, 1),
    end = min(satz_num + 1, max(sätze_trump$satz_num[sätze_trump$datei == datei]))
  ) %>%
  rowwise() %>%
  mutate(
    text_block = paste(
      sätze_trump$saetze[sätze_trump$datei == datei & sätze_trump$satz_num >= start & sätze_trump$satz_num <= end],
      collapse = " "
    )
  ) %>%
  select(datei, text_block) %>%
  distinct()

kontext_blöcke_trump1 <- head(kontext_blöcke_trump1, 5)

for (j in 1:nrow(kontext_blöcke_trump1)) {
  cat("Datei:", kontext_blöcke_trump1$datei[j], "\n")
  cat(kontext_blöcke_trump1$text_block[j], "\n\n")
}


suchwort_trump2 <- "open"

kontext_blöcke_trump2 <- sätze_trump %>%
  filter(str_detect(saetze, regex(paste0("\\b", suchwort_trump2, "\\b"), ignore_case = TRUE))) %>%
  rowwise() %>%
  mutate(
    start = max(satz_num - 1, 1),
    end = min(satz_num + 1, max(sätze_trump$satz_num[sätze_trump$datei == datei]))
  ) %>%
  rowwise() %>%
  mutate(
    text_block = paste(
      sätze_trump$saetze[sätze_trump$datei == datei & sätze_trump$satz_num >= start & sätze_trump$satz_num <= end],
      collapse = " "
    )
  ) %>%
  select(datei, text_block) %>%
  distinct()

kontext_blöcke_trump2 <- head(kontext_blöcke_trump2, 5)

for (j in 1:nrow(kontext_blöcke_trump2)) {
  cat("Datei:", kontext_blöcke_trump2$datei[j], "\n")
  cat(kontext_blöcke_trump2$text_block[j], "\n\n")
}

