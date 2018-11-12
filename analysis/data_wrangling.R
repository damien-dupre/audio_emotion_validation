# Load package
library(here)
library(plyr)
library(tidyverse)
library(data.table)
list_files <- list.files(here::here("./data/raw_data"), pattern = ".csv")

raw_data <- plyr::ldply(list_files, function(file_name){
  dat <- data.table::fread(here::here("./data/raw_data",file_name)) %>%
    dplyr::mutate(file_ID = gsub(".mp3.csv","",file_name))%>%
    dplyr::mutate(ppt_ID = substr(file_ID, 0, 2)) %>%
    dplyr::mutate(ppt_emo = substr(file_ID, 3, 5))
})

