# Prepare redundancy analyses ----
redundancy_cc = function(model) {
  # list of possible mode B constructs and their ra indicators
  B_list = data.frame(
    Constructs = c(
      "Perceived Self-Efficacy",
      "Perceived Response Efficacy",
      "Perceived Response Costs",
      "Descriptive Norm",
      "Injunctive Norm",
      "Behavioral Intention"
  ),
  A_indicators = c(
      "CCRB10",
      "CCRB11",
      "CCRB12",
      "CCDN4",
      "CCIN4",
      "CCBI4"
  )
  )
  # Find existing mode B constructs
  B_existing = model[[4]] %>%
    as.data.frame() %>%
    filter(construct %in% B_list[,1] & type == "B")
  B_constructs = unique(B_existing[,1])
  # estimate ra models for all mode B constructs
  ra_list = lapply(B_constructs,
                   FUN = function(x)
                   estimate_pls(
                     model$data,
                     measurement_model = constructs(
                     composite(paste0(x,
                                      " mode B"),
                               item_names = (B_existing %>% filter(construct == x))[,2],
                               weights = mode_B
                               ),
                           composite(paste0(x,
                                            " mode A"),
                                     single_item(
                                       B_list[match(x, B_list[,1]), 2]
                                     )
                           )
                         ),
                        structural_model = relationships(
                          from = paste0(x,
                                        " mode B"),
                          to = paste0(x,
                                       " mode A")
                          )
                   )
  )
  return(ra_list)
}

# Estimate CC-1 ----
estimate_cc_1 = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN")))))
  mm <- constructs(
    composite("Perceived Self-Efficacy", multi_items("CCRB", 1:3), mode_B),
    composite("Perceived Response Efficacy", multi_items("CCRB", 4:6), mode_B),
    composite("Perceived Response Costs", multi_items("CCRB", 7:9), mode_B),
    higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
    composite("Benevolence", multi_items("CCDI", 1:3)),
    composite("Competence", multi_items("CCDI", 4:6)),
    composite("Integrity", multi_items("CCDI", 7:9)),
    higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
    composite("Knowledge", single_item("CCKN")),
    composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
    composite("Perceived Severity", multi_items("CCTB", 4:6)),
    higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
    composite("Personal Moral Norm", multi_items("CCPN", 1:3)),
    composite("Descriptive Norm", multi_items("CCDN", 1:2), mode_B),
    composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
    higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
    composite("Behavioral Intention", multi_items("CCBI", 1:3), mode_B)
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
    paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
  )
  model <- estimate_pls(data, mm, sm)
  return(model)
}



# library(tidyverse)
# # library(remotes)
# # remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
# library(seminr)
#
# # Load data
# datafull <- read_rds("Data/S1-data-nm.RDS")
# datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
# data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))
#
# # Describe measurement model
#
#
#
# # model <- estimate_pls(data, mm, sm)
# # saveRDS(model, "SEM Climate Crisis/Models/model-cc-1.RDS")
# model <- readRDS("SEM Climate Crisis/Models/model-cc-1.RDS")
# plot(model)
# # bootmodel <- bootstrap_model(model, nboot = 5000)
# # saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-1.RDS")
# bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-1.RDS")
# plot(bootmodel)
# fsmodel <- model$first_stage_model
# # bootfsmodel <-  bootstrap_model(fsmodel, nboot = 5000)
# # saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-1.RDS")
#
# # Models for estimation of convergent validity for formative constructs
# # Perceived Self-Efficacy
# mmpse <- constructs(
#   composite("Perceived Self-Efficacy Formative", multi_items("CCRB", 1:3), mode_B),
#   composite("Perceived Self-Efficacy Reflective", single_item("CCRB10"))
# )
# smpse <- relationships(
#   paths(from = "Perceived Self-Efficacy Formative", to = "Perceived Self-Efficacy Reflective")
# )
# rapse <- estimate_pls(data, mmpse, smpse)
# saveRDS(rapse, "SEM Climate Crisis/Models/rapse-cc-1.RDS")
#
# # Perceived Response Efficacy
# mmpre <- constructs(
#   composite("Perceived Response Efficacy Formative", multi_items("CCRB", 4:6), mode_B),
#   composite("Perceived Response Efficacy Reflective", single_item("CCRB11"))
# )
# smpre <- relationships(
#   paths(from = "Perceived Response Efficacy Formative", to = "Perceived Response Efficacy Reflective")
# )
# rapre <- estimate_pls(data, mmpre, smpre)
# saveRDS(rapre, "SEM Climate Crisis/Models/rapre-cc-1.RDS")
#
# # Perceived Response Costs
# mmprc <- constructs(
#   composite("Perceived Response Costs Formative", multi_items("CCRB", 7:9), mode_B),
#   composite("Perceived Response Costs Reflective", single_item("CCRB12"))
# )
# smprc <- relationships(
#   paths(from = "Perceived Response Costs Formative", to = "Perceived Response Costs Reflective")
# )
# raprc <- estimate_pls(data, mmprc, smprc)
# saveRDS(raprc, "SEM Climate Crisis/Models/raprc-cc-1.RDS")
#
# # Descriptive Norm
# mmdn <- constructs(
#   composite("Descriptive Norm Formative", multi_items("CCDN", 1:2), mode_B),
#   composite("Descriptive Norm Reflective", single_item("CCDN4"))
# )
# smdn <- relationships(
#   paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
# )
# radn <- estimate_pls(data, mmdn, smdn)
# saveRDS(radn, "SEM Climate Crisis/Models/radn-cc-1.RDS")
#
# # Injunctive Norm
# mmin <- constructs(
#   composite("Injunctive Norm Formative", multi_items("CCIN", 1:2), mode_B),
#   composite("Injunctive Norm Reflective", single_item("CCIN4"))
# )
# smin <- relationships(
#   paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
# )
# rain <- estimate_pls(data, mmin, smin)
# saveRDS(rain, "SEM Climate Crisis/Models/rain-cc-1.RDS")
#
# # Behavioral Intention
# mmbi <- constructs(
#   composite("Behavioral Intention Formative", multi_items("CCBI", 1:3), mode_B),
#   composite("Behavioral Intention Reflective", single_item("CCBI4"))
# )
# smbi <- relationships(
#   paths(from = "Behavioral Intention Formative", to = "Behavioral Intention Reflective")
# )
# rabi <- estimate_pls(data, mmbi, smbi)
# saveRDS(rabi, "SEM Climate Crisis/Models/rabi-cc-1.RDS")
