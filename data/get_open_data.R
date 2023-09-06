# This file will download the open data from the OSF repository.
# In order to download the data, you need to authenticate with you OSF PAT using osf_auth()
#
# WARNING: DO NOT RUN IF YOU HAVE UPDATED DATA!


# Generate anonymized data from RAW Data
library(osfr)
library(tidyverse)

open_path <- raw_path <- here::here("data", "open")

osf_id <- "qbfu8" # This component will be open!

osf_node <- osf_retrieve_node(osf_id)

osf_node %>%
  osf_ls_files(n_max = Inf) %>%
  osf_download(here::here("data", "open"),
               recurse = TRUE,
               conflicts = "overwrite",
               progress = TRUE)
