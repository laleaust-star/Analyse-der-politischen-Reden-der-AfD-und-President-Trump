library(dplyr)
library(readr)
library(stringr)
library(tidyr)

weg <- "dataset/raw/afd"

dateien <- list.files(weg, pattern = "\\.txt$", full.names = TRUE)

reden_df <- data.frame(
  datei = basename(dateien),
  text = sapply(dateien, read_file),
  stringsAsFactors = FALSE
)

sätze_df <- reden_df %>%
  rowwise() %>%
  mutate(saetze = str_split(text, "(?<=[.!?])\\s+")) %>%
  unnest(saetze) %>%
  mutate(saetze = str_trim(saetze)) %>%
  group_by(datei) %>%
  mutate(satz_num = row_number()) %>%
  ungroup()

suchwort1 <- "ukraine"

kontext_blöcke1 <- sätze_df %>%
  filter(str_detect(saetze, regex(paste0("\\b", suchwort1, "\\b"), ignore_case = TRUE))) %>%
  rowwise() %>%
  mutate(
    start = max(satz_num - 1, 1),
    end = min(satz_num + 1, max(sätze_df$satz_num[sätze_df$datei == datei]))
  ) %>%
  rowwise() %>%
  mutate(
    text_block = paste(
      sätze_df$saetze[sätze_df$datei == datei & sätze_df$satz_num >= start & sätze_df$satz_num <= end],
      collapse = " "
    )
  ) %>%
  select(datei, text_block) %>%
  distinct()

kontext_blöcke1 <- head(kontext_blöcke1, 5)

for (j in 1:nrow(kontext_blöcke1)) {
  cat("Datei:", kontext_blöcke1$datei[j], "\n")
  cat(kontext_blöcke1$text_block[j], "\n\n")
}

suchwort2 <-"trump"

kontext_blöcke2 <- sätze_df %>%
  filter(str_detect(saetze, regex(paste0("\\b", suchwort2, "\\b"), ignore_case = TRUE))) %>%
  rowwise() %>%
  mutate(
    start = max(satz_num - 1, 1),
    end = min(satz_num + 1, max(sätze_df$satz_num[sätze_df$datei == datei]))
  ) %>%
  rowwise() %>%
  mutate(
    text_block = paste(
      sätze_df$saetze[sätze_df$datei == datei & sätze_df$satz_num >= start & sätze_df$satz_num <= end],
      collapse = " "
    )
  ) %>%
  select(datei, text_block) %>%
  distinct()

kontext_blöcke2 <- head(kontext_blöcke2, 5)

for (j in 1:nrow(kontext_blöcke2)) {
  cat("Datei:", kontext_blöcke2$datei[j], "\n")
  cat(kontext_blöcke2$text_block[j], "\n\n")
}


suchwort3 <-"offen"

kontext_blöcke3 <- sätze_df %>%
  filter(str_detect(saetze, regex(paste0("\\b", suchwort3, "\\b"), ignore_case = TRUE))) %>%
  rowwise() %>%
  mutate(
    start = max(satz_num - 1, 1),
    end = min(satz_num + 1, max(sätze_df$satz_num[sätze_df$datei == datei]))
  ) %>%
  rowwise() %>%
  mutate(
    text_block = paste(
      sätze_df$saetze[sätze_df$datei == datei & sätze_df$satz_num >= start & sätze_df$satz_num <= end],
      collapse = " "
    )
  ) %>%
  select(datei, text_block) %>%
  distinct()

kontext_blöcke3 <- head(kontext_blöcke3, 5)

for (j in 1:nrow(kontext_blöcke3)) {
  cat("Datei:", kontext_blöcke3$datei[j], "\n")
  cat(kontext_blöcke3$text_block[j], "\n\n")
}

suchwort4 <-"talahon"

kontext_blöcke4 <- sätze_df %>%
  filter(str_detect(saetze, regex(paste0("\\b", suchwort4, "\\b"), ignore_case = TRUE))) %>%
  rowwise() %>%
  mutate(
    start = max(satz_num - 1, 1),
    end = min(satz_num + 1, max(sätze_df$satz_num[sätze_df$datei == datei]))
  ) %>%
  rowwise() %>%
  mutate(
    text_block = paste(
      sätze_df$saetze[sätze_df$datei == datei & sätze_df$satz_num >= start & sätze_df$satz_num <= end],
      collapse = " "
    )
  ) %>%
  select(datei, text_block) %>%
  distinct()

kontext_blöcke4 <- head(kontext_blöcke4, 5)

for (j in 1:nrow(kontext_blöcke4)) {
  cat("Datei:", kontext_blöcke4$datei[j], "\n")
  cat(kontext_blöcke4$text_block[j], "\n\n")
}
