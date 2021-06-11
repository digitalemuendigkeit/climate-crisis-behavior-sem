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

# Estimate CC-2-a-1 ----
estimate_cc_2_a_1 = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN")))))
  mm <- constructs(
    composite("Perceived Self-Efficacy", single_item("CCRB1")),
    composite("Perceived Response Efficacy", single_item("CCRB4")),
    composite("Perceived Response Costs", single_item("CCRB7")),
    higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
    composite("Benevolence", multi_items("CCDI", 1:3)),
    composite("Competence", multi_items("CCDI", 4:6)),
    composite("Integrity", multi_items("CCDI", 7:9)),
    higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
    composite("Knowledge", single_item("CCKN")),
    composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
    composite("Perceived Severity", multi_items("CCTB", 4:6)),
    higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
    composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
    composite("Descriptive Norm", multi_items("CCDN", 1:2), mode_B),
    composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
    higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
    composite("Behavioral Intention", single_item("CCBI1"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
    paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
  )
  model <- estimate_pls(data, mm, sm)
  return(model)
}

# Estimate CC-2-a-2 ----
estimate_cc_2_a_2 = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN")))))
  mm <- constructs(
    composite("Perceived Self-Efficacy", single_item("CCRB1")),
    composite("Perceived Response Efficacy", single_item("CCRB4")),
    higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy"), weights = mode_B),
    composite("Benevolence", multi_items("CCDI", 1:3)),
    composite("Competence", multi_items("CCDI", 4:6)),
    composite("Integrity", multi_items("CCDI", 7:9)),
    higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
    composite("Knowledge", single_item("CCKN")),
    composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
    composite("Perceived Severity", multi_items("CCTB", 4:6)),
    higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
    composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
    composite("Descriptive Norm", multi_items("CCDN", 1:2), mode_B),
    composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
    higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
    composite("Behavioral Intention", single_item("CCBI1"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
    paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
  )
  model <- estimate_pls(data, mm, sm)
  return(model)
}

# Estimate CC-2-a-2 proxymodel ----
estimate_cc_2_a_2_proxy = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN"))))) %>%
    as.data.frame()
  mm <- constructs(
    composite("Response Beliefs", multi_items("CCRB", c(1,4)), weights = mode_B),
    composite("Distrusting Beliefs", multi_items("CCDI", 1:9), weights = mode_B),
    composite("Knowledge", single_item("CCKN")),
    composite("Threat Beliefs", multi_items("CCTB", 1:6)),
    composite("Subjective Norm", c(multi_items("CCDN", 1:2), multi_items("CCIN",1:2))),
    composite("Personal Moral Norm", multi_items("CCPN", c(1,3))),
    composite("Behavioral Intention", single_item("CCBI1"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
    paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
  )
  proxymodel <- estimate_pls(data, mm, sm)
  return(proxymodel)
}

# Estimate CC-2-a-3 ----
estimate_cc_2_a_3 = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN")))))
  mm <- constructs(
    composite("Perceived Self-Efficacy", single_item("CCRB1")),
    composite("Perceived Response Efficacy", single_item("CCRB4")),
    higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy"), weights = mode_B),
    composite("Benevolence", multi_items("CCDI", 1:3)),
    composite("Competence", multi_items("CCDI", 4:6)),
    composite("Integrity", multi_items("CCDI", 7:9)),
    higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
    composite("Knowledge", single_item("CCKN")),
    composite("Behavioral Intention", single_item("CCBI1"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs")),
    paths(from = c("Response Beliefs"), to = "Behavioral Intention")
  )
  model <- estimate_pls(data, mm, sm)
  return(model)
}

# Estimate CC-2-a-3 proxymodel ----
estimate_cc_2_a_3_proxy = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN"))))) %>%
    as.data.frame()
  mm <- constructs(
    composite("Response Beliefs", multi_items("CCRB", c(1,4)), weights = mode_B),
    composite("Distrusting Beliefs", multi_items("CCDI", 1:9), weights = mode_B),
    composite("Knowledge", single_item("CCKN")),
    composite("Behavioral Intention", single_item("CCBI1"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs")),
    paths(from = c("Response Beliefs"), to = "Behavioral Intention")
  )
  proxymodel <- estimate_pls(data, mm, sm)
  return(proxymodel)
}

# Estimate CC-2-a-4 ----
estimate_cc_2_a_4 = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN")))))
  mm <- constructs(
    composite("Perceived Self-Efficacy", single_item("CCRB1")),
    composite("Perceived Response Efficacy", single_item("CCRB4")),
    higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy"), weights = mode_B),
    composite("Benevolence", multi_items("CCDI", 1:3)),
    composite("Competence", multi_items("CCDI", 4:6)),
    composite("Integrity", multi_items("CCDI", 7:9)),
    higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
    composite("Behavioral Intention", single_item("CCBI1"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs"), to = c("Response Beliefs")),
    paths(from = c("Response Beliefs"), to = "Behavioral Intention")
  )
  model <- estimate_pls(data, mm, sm)
  return(model)
}


# Estimate CC-2-a-4 proxymodel ----
estimate_cc_2_a_4_proxy = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN"))))) %>%
    as.data.frame()
  mm <- constructs(
    composite("Response Beliefs", multi_items("CCRB", c(1,4)), weights = mode_B),
    composite("Distrusting Beliefs", multi_items("CCDI", 1:9), weights = mode_B),
    composite("Behavioral Intention", single_item("CCBI1"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs"), to = c("Response Beliefs")),
    paths(from = c("Response Beliefs"), to = "Behavioral Intention")
  )
  proxymodel <- estimate_pls(data, mm, sm)
  return(proxymodel)
}


# Estimate CC-2-b-1 ----
estimate_cc_2_b_1 = function(data) {
  data <- as.matrix(data %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(data %>% select(starts_with("CCKN")))))
  mm <- constructs(
    composite("Perceived Self-Efficacy", single_item("CCRB2")),
    composite("Perceived Response Efficacy", single_item("CCRB5")),
    composite("Perceived Response Costs", single_item("CCRB8")),
    higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
    composite("Benevolence", multi_items("CCDI", 1:3)),
    composite("Competence", multi_items("CCDI", 4:6)),
    composite("Integrity", multi_items("CCDI", 7:9)),
    higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
    composite("Knowledge", single_item("CCKN")),
    composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
    composite("Perceived Severity", multi_items("CCTB", 4:6)),
    higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
    composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
    composite("Descriptive Norm", multi_items("CCDN", 1:2), mode_B),
    composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
    higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
    composite("Behavioral Intention", single_item("CCBI2"))
  )
  sm <- relationships(
    paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
    paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
  )
  model <- estimate_pls(data, mm, sm)
  return(model)
}
