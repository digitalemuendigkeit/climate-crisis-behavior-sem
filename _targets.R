library(targets)
library(tarchetypes)
#source(here::here("R", "functions.R"))
source("R/00_data-wrangling.R")
source("R/01_sem-estimation.R")
# If you don't have hcictools installed
# Uncomment and run this line
# remotes::install_git("https://github.com/statisticsforsocialscience/hcictools")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("careless", "hcictools", "Hmisc", "psych", "seminr", "tidyverse"))

list(
  tar_target(
    raw_S1_file,
    "data/raw/S1-Raw-Data-20210118.csv",
    format = "file"
  ),
  tar_target(
    raw_S1_text_file,
    "data/raw/S1-data-choicetext.csv",
    format = "file"
  ),
  tar_target(
    raw_S2_file,
    "data/raw/S2-Raw-Data-20210208.csv",
    format = "file"
    ),
  tar_target(
    raw_incidence_20210112,
    "data/external/20210112-Fallzahlen.csv",
    format = "file"
  ),
  tar_target(
    raw_incidence_20210201,
    "data/external/20210201-Fallzahlen.csv",
    format = "file"
  ),
  tar_target(
    survey_data_cleaned,
    import_clean(raw_S1_file, raw_S2_file)
  ),
  tar_target(
    survey_data_incidence,
    include_incidence(survey_data_cleaned, raw_S1_text_file, raw_incidence_20210112, raw_incidence_20210201)
  ),
  tar_target(
    survey_data_param,
    param_clean(survey_data_cleaned)
  ),
  tar_target(
    survey_data_inc_param,
    param_clean(survey_data_incidence)
  ),
  tar_target(
    survey_data_sem,
    treat_missing(survey_data_param)
  ),
  tar_target(
    survey_data_cc_sem,
      survey_data_sem %>%
        select(starts_with("CC")) %>%
      filter(!is.na(CCSKN))
  ),
  tar_target(
    cc_model_1,
    estimate_cc_1(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_1_boot,
    bootstrap_model(cc_model_1, 5000)
  ),
  tar_target(
    cc_model_1_ra,
    redundancy_cc(cc_model_1)
  )
  # ,
  # tar_render(
  #
  # )
)

# Data import
# we should start with anonymized data
# so the analysis can be fully reproduced with targets
# data import
# data cleaning
# data upload?

# model creation
# model evaluation
