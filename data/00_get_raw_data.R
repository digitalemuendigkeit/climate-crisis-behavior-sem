# This script contains code to get the raw data from a private OSF repository.

library(osfr)
library(tidyverse)
osf_id <- "rvkx4" # This component will remain private!

osf_node <- osf_retrieve_node(osf_id)

osf_node %>%
  osf_ls_files() %>%
  osf_download(here::here("data", "raw"),
               recurse = TRUE,
               conflicts = "overwrite",
               progress = TRUE)

source(here::here("data", "password.R"))

zip_file <- here::here("data", "raw", "encrypted_data.zip")
target_folder <- here::here("data", "raw")

if(str_detect(sessionInfo()$platform, "darwin")) {
  # This works on the mac
  system(paste0(
    "unzip -o -P ", password, " ", zip_file, " *.csv -d ", target_folder
  ))
}


