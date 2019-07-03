# libraries -----------------------------------------------------------------
library(here)
library(plyr)
library(tidyverse)
library(data.table)

# merge files ------------------------------------------------------------------
audio_emotion_data <- "../../data/audio_emotion_data/csv_files" %>%
  fs::dir_ls(regexp = "\\.csv$") %>%
  purrr::map_dfr(readr::read_csv, .id = "source") %>%
  dplyr::mutate(source = tools::file_path_sans_ext(base::basename(source))) %>%
  dplyr::mutate(file_ID = gsub(".mp3", "", source)) %>%
  dplyr::mutate(ppt_ID = substr(file_ID, 0, 2)) %>%
  dplyr::mutate(ppt_emo = substr(file_ID, 3, 5))

# readr::write_rds(audio_emotion_data, "audio_emotion_data.rds")
