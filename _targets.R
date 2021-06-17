library(targets)
library(tarchetypes)
#source(here::here("R", "functions.R"))
source("R/00_data-wrangling.R")
source("R/01_sem-estimation.R")
#source("R/02_sem-evaluation.R")
# If you don't have hcictools installed
# Uncomment and run this line
# remotes::install_git("https://github.com/statisticsforsocialscience/hcictools")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("careless",
                            "distill",
                            "DT",
                            "hcictools",
                            "Hmisc",
                            "htmltools",
                            "knitr",
                            "psych",
                            "seminr",
                            "tidyverse"))

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
    cc_model_1_fs_boot,
    bootstrap_model(cc_model_1$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_1_ra,
    redundancy_cc(cc_model_1)
  ),
  # tar_render(
  #   cc_1_mmeval,
  #   "reports/cc-1_mmeval.RMD"
  # ),
  tar_target(
    cc_model_2_a_1,
    estimate_cc_2_a_1(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_a_1_boot,
    bootstrap_model(cc_model_2_a_1, 5000)
  ),
  tar_target(
    cc_model_2_a_1_fs_boot,
    bootstrap_model(cc_model_2_a_1$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_a_1_ra,
    redundancy_cc(cc_model_2_a_1)
  ),
  tar_target(
    cc_model_2_a_2,
    estimate_cc_2_a_2(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_a_2_boot,
    bootstrap_model(cc_model_2_a_2, 5000)
  ),
  tar_target(
    cc_model_2_a_2_fs_boot,
    bootstrap_model(cc_model_2_a_2$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_a_2_ra,
    redundancy_cc(cc_model_2_a_2)
  ),
  tar_target(
    cc_model_2_a_2_proxy,
    estimate_cc_2_a_2_proxy(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_a_2_proxy_boot,
    bootstrap_model(cc_model_2_a_2_proxy, 500)
  ),
  tar_target(
    cc_model_2_a_2_plspre,
    pls_predict(cc_model_2_a_2_proxy)
  ),
  tar_target(
    cc_model_2_a_3,
    estimate_cc_2_a_3(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_a_3_boot,
    bootstrap_model(cc_model_2_a_3, 5000)
  ),
  tar_target(
    cc_model_2_a_3_fs_boot,
    bootstrap_model(cc_model_2_a_3$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_a_3_ra,
    redundancy_cc(cc_model_2_a_3)
  ),
  tar_target(
    cc_model_2_a_3_proxy,
    estimate_cc_2_a_3_proxy(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_a_3_proxy_boot,
    bootstrap_model(cc_model_2_a_3_proxy, 500)
  ),
  tar_target(
    cc_model_2_a_3_plspre,
    pls_predict(cc_model_2_a_3_proxy)
  ),
  tar_target(
    cc_model_2_a_4,
    estimate_cc_2_a_4(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_a_4_boot,
    bootstrap_model(cc_model_2_a_4, 5000)
  ),
  tar_target(
    cc_model_2_a_4_fs_boot,
    bootstrap_model(cc_model_2_a_4$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_a_4_ra,
    redundancy_cc(cc_model_2_a_4)
  ),
  tar_target(
    cc_model_2_a_4_proxy,
    estimate_cc_2_a_4_proxy(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_a_4_proxy_boot,
    bootstrap_model(cc_model_2_a_4_proxy, 500)
  ),
  tar_target(
    cc_model_2_a_4_plspre,
    pls_predict(cc_model_2_a_4_proxy)
  ),
  tar_target(
    cc_model_2_b_1,
    estimate_cc_2_b_1(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_b_1_boot,
    bootstrap_model(cc_model_2_b_1, 5000)
  ),
  tar_target(
    cc_model_2_b_1_fs_boot,
    bootstrap_model(cc_model_2_b_1$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_b_1_ra,
    redundancy_cc(cc_model_2_b_1)
  ),
  tar_target(
    cc_model_2_b_2,
    estimate_cc_2_b_2(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_b_2_boot,
    bootstrap_model(cc_model_2_b_2, 5000)
  ),
  tar_target(
    cc_model_2_b_2_fs_boot,
    bootstrap_model(cc_model_2_b_2$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_b_2_ra,
    redundancy_cc(cc_model_2_b_2)
  ),
  tar_target(
    cc_model_2_b_2_proxy,
    estimate_cc_2_b_2_proxy(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_b_2_proxy_boot,
    bootstrap_model(cc_model_2_b_2_proxy, 500)
  ),
  tar_target(
    cc_model_2_b_2_plspre,
    pls_predict(cc_model_2_b_2_proxy)
  ),
  tar_target(
    cc_model_2_b_3,
    estimate_cc_2_b_3(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_b_3_boot,
    bootstrap_model(cc_model_2_b_3, 5000)
  ),
  tar_target(
    cc_model_2_b_3_fs_boot,
    bootstrap_model(cc_model_2_b_3$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_b_3_ra,
    redundancy_cc(cc_model_2_b_3)
  ),
  tar_target(
    cc_model_2_b_3_proxy,
    estimate_cc_2_b_3_proxy(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_b_3_proxy_boot,
    bootstrap_model(cc_model_2_b_3_proxy, 500)
  ),
  tar_target(
    cc_model_2_b_3_plspre,
    pls_predict(cc_model_2_b_3_proxy)
  ),
  tar_target(
    cc_model_2_c_1,
    estimate_cc_2_c_1(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_c_1_boot,
    bootstrap_model(cc_model_2_c_1, 5000)
  ),
  tar_target(
    cc_model_2_c_1_fs_boot,
    bootstrap_model(cc_model_2_c_1$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_c_1_ra,
    redundancy_cc(cc_model_2_c_1)
  ),
  tar_target(
    cc_model_2_c_2,
    estimate_cc_2_c_2(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_c_2_boot,
    bootstrap_model(cc_model_2_c_2, 5000)
  ),
  tar_target(
    cc_model_2_c_2_fs_boot,
    bootstrap_model(cc_model_2_c_2$first_stage_model, 5000)
  ),
  tar_target(
    cc_model_2_c_2_ra,
    redundancy_cc(cc_model_2_c_2)
  ),
  tar_target(
    cc_model_2_c_2_proxy,
    estimate_cc_2_c_2_proxy(survey_data_cc_sem)
  ),
  tar_target(
    cc_model_2_c_2_proxy_boot,
    bootstrap_model(cc_model_2_c_2_proxy, 500)
  ),
  tar_target(
    cc_model_2_c_2_plspre,
    pls_predict(cc_model_2_c_2_proxy)
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
