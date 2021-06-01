library(targets)
source(here::here("R", "functions.R"))
source(here::here("R", "00_data-wrangling.R"))
# If you don't have hcictools installed
# Uncomment and run this line
# remotes::install_git("https://github.com/statisticsforsocialscience/hcictools")

tar_option_set(packages = c("tidyverse", "careless", "hcictools"))

list(
  tar_target(
    raw_S1_text_file,
    "data/raw/S1-data-choicetext.csv",
    format = "file"
    ),
  tar_target(
    raw_S1_file,
    "data/raw/S1-Raw-Data-20210118.csv",
    format = "file"
  ),
  tar_target(
    raw_S2_file,
    "data/raw/S2-Raw-Data-20210208.csv",
    format = "file"
    ),
  tar_target(
    survey_data_cleaned,
    sica(raw_S1_file, raw_S2_file)
  )
)

# Data import
# we should start with anonymized data
# so the analysis can be fully reproduced with targets
# data import
# data cleaning
# data upload?

# model creation
# model evaluation
