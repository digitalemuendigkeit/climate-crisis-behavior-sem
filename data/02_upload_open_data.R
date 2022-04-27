# This script contains code to upload the open data generated from the raw data to the OSF repository.
# Typically, you do not need to run this script
# To get the open data run script get_open_data

library(osfr)
library(tidyverse)


open_path <- here::here("data", "open")

osf_id_open <- "qbfu8"

osf_open_node <- osf_retrieve_node(osf_id_open)

files_to_upload <- dir(open_path, pattern = "*.csv", recursive = T)

osf_open_node %>%
  osf_upload(path = paste0("data/open/", files_to_upload),
             conflicts = "overwrite",
             recurse = T, progress = T)


files_to_upload <- dir(open_path, pattern = "*.RDS", recursive = T)

osf_open_node %>%
  osf_upload(path = paste0("data/open/", files_to_upload),
             conflicts = "overwrite",
             recurse = T, progress = T)
