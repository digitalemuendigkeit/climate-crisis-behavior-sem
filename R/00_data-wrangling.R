# Import, clean and anonymize data ----
import_clean = function(rs1f, rs2f) {
  # Import data from survey 1----
  # First 2 rows are descriptors
  # Next 6 rows are tests
  NameRD <- data.frame(
    read_csv(rs1f)
  )
  RD <-
    read_csv(rs1f,
             skip = 8,
             col_names = colnames(NameRD)
    )
  # Crop Data
  # From Descriptive Cols keep only Duration, RecordedDate, ResponseId, gid
  # Crop cols c(1:5,7,10:18)
  RDCrop <- RD[,-c(1:5,7,10:18)]

  # Only completed answers
  # S2 is answered
  RDComp <- RDCrop[!is.na(RDCrop$S2),]

  # Find speeders
  # Define upper range
  URtime <-
    quantile(RDComp$Duration..in.seconds.,
             probs = c(.75),
             names = FALSE) +
    1.5 * IQR(RDComp$Duration..in.seconds.)

  #Define lower range
  LRtime <-
    quantile(RDComp$Duration..in.seconds.,
             probs = c(.25),
             names = FALSE) -
    1.5 * IQR(RDComp$Duration..in.seconds.)

  # Find outliers for percentage
  # Lower range might be below 0 and therefore invalid
  URolperc <-
    nrow(RDComp[RDComp$Duration..in.seconds. > URtime, ]) / nrow(RDComp)
  LRolperc <-
    ifelse(LRtime > 0, row(RDComp[RDComp$Duration..in.seconds. > LRtime, ]) /
             nrow(RDComp), 0)
  olperc <- URolperc + LRolperc

  # Find trim mean
  durtm <- mean(RDComp$Duration..in.seconds., trim = olperc)

  # Exclude responses that are three times as fast
  RDprel <- RDComp[RDComp$Duration..in.seconds. > durtm / 3, ]

  # Find straight liners and diagonal liners
  # Reverse reverse-coded data
  # 6:1 Psychological c(BFE1, BFC1, BFA2, BFC2, BFO2)
  # BFN3 is wrongly not reversed in data -> recode twice
  # 1:3 CC Knowledge c(CCKN1, CCKN3, CCKN5, CCKN6)
  # 6:1 CC c(CCDI2, CCDI4, CCDI9, CCTB2, CCRB7, CCRB8, CCRB9, CCRB12)
  # 1:3 CO Knowledge c(COKN4, COKN5, COKN6)
  # 6:1 CO c(CODI2, CODI4, CODI9, COTB3, CORB7, CORB8, CORB9, CORB12)
  # function to invert reversed regular items
  revcode <-  function(x) {
    dplyr::recode(
      x,
      '1' = 6,
      '2' = 5,
      '3' = 4,
      '4' = 3,
      '5' = 2,
      '6' = 1,
      .default = NaN
    )
  }
  # function to invert reversed knowledge items
  revkncode <-  function(x) {
    dplyr::recode(
      x,
      '1' = 3,
      '2' = 2,
      '3' = 1,
      .default = NaN
    )
  }
  RDComp <- RDComp %>% mutate_at("BFN3", revcode)
  RDCompR <- RDComp %>%
    mutate_at(
      c(
        "BFE1",
        "BFC1",
        "BFA2",
        "BFC2",
        "BFO2",
        "BFN3",
        "CCDI2",
        "CCDI4",
        "CCDI9",
        "CCTB2",
        "CCRB7",
        "CCRB8",
        "CCRB9",
        "CCRB12",
        "CODI2",
        "CODI4",
        "CODI9",
        "COTB3",
        "CORB7",
        "CORB8",
        "CORB9",
        "CORB12"
      ),
      list(revcode)
    ) %>%
    mutate_at(c("CCKN1", "CCKN3", "CCKN5", "CCKN6", "COKN4", "COKN5", "COKN6"),
              list(revkncode))

  # Make question blocks to examine
  # Psych - some reversed, straight lining most critical
  blockpsych <- RDCompR[, c(145, 1, 25:43)]
  cbp <-
    blockpsych %>% cbind('longstr' = longstring(blockpsych),
                         'irv' = irv(blockpsych[, -(1:2)]))
  cpbrem <- cbp %>% filter(irv == 0,
                           irv == max(cbp$irv))

  # Behavioral Intention - none reversed, pole/diagonal most critical
  blockbi <- RDCompR[, c(145, 1, 44:51)]
  cbi <-
    blockbi %>% cbind('longstr' = longstring(blockbi),
                      'irv' = irv(blockbi[, -(1:2)]))
  cbirem <- cbi %>% filter(irv == 0,
                           irv == max(cbi$irv))

  # CC Knowledge - some reversed
  blockcckn <- RDCompR[!is.na(RDCompR$CCSKN), c(145, 1, 53:58)]
  ccckn <-
    blockcckn %>% cbind('longstr' = longstring(blockcckn),
                        'irv' = irv(blockcckn[, -(1:2)]))
  cccknrem <- ccckn %>% filter(irv == 0)

  # CC Distrust - some reversed, straight-lining most critical
  blockccdi <- RDCompR[!is.na(RDCompR$CCSKN), c(145, 1, 59:67)]
  cccdi <-
    blockccdi %>% cbind('longstr' = longstring(blockccdi),
                        'irv' = irv(blockccdi[, -(1:2)]))
  cccdirem <- cccdi %>% filter(irv == 0)

  # CC Threat Beliefs - one reversed, straight-lining most critical
  blockcctb <- RDCompR[!is.na(RDCompR$CCSKN), c(145, 1, 68:73)]
  ccctb <-
    blockcctb %>% cbind('longstr' = longstring(blockcctb),
                        'irv' = irv(blockcctb[, -(1:2)]))
  ccctbrem <- ccctb %>% filter(irv == 0)

  # CC Response Beliefs - four reversed, straight-lining most critical
  blockccrb <- RDCompR[!is.na(RDCompR$CCSKN), c(145, 1, 74:85)]
  cccrb <-
    blockccrb %>% cbind('longstr' = longstring(blockccrb),
                        'irv' = irv(blockccrb[, -(1:2)]))
  cccrbrem <- cccrb %>% filter(irv == 0)

  # CC Personal Moral Norm - none reversed, diagonal-lining most critical
  blockccpn <- RDCompR[!is.na(RDCompR$CCSKN), c(145, 1, 86:88)]
  cccpn <-
    blockccpn %>% cbind('longstr' = longstring(blockccpn),
                        'irv' = irv(blockccpn[, -(1:2)]))
  cccpnrem <- cccpn %>% filter(irv == max(cccpn$irv))

  # CC Subjective Norm - none reversed, diagonal-lining more critical
  # include second largest here
  blockccsn <- RDCompR[!is.na(RDCompR$CCSKN), c(145, 1, 89:96)]
  cccsn <-
    blockccsn %>% cbind('longstr' = longstring(blockccsn),
                        'irv' = irv(blockccsn[, -(1:2)]))
  cccsnrem <- cccsn %>% filter(irv == 0,
                               irv >= max(cccsn$irv[cccsn$irv!=max(cccsn$irv)]))

  # CO Knowledge - some reversed
  blockcokn <- RDCompR[!is.na(RDCompR$COSKN), c(145, 1, 98:104)]
  ccokn <-
    blockcokn %>% cbind('longstr' = longstring(blockcokn),
                        'irv' = irv(blockcokn[, -(1:2)]))
  ccoknrem <- ccokn %>% filter(irv == 0)

  # CO Distrust - some reversed, straight-lining most critical
  blockcodi <- RDCompR[!is.na(RDCompR$COSKN), c(145, 1, 105:113)]
  ccodi <-
    blockcodi %>% cbind('longstr' = longstring(blockcodi),
                        'irv' = irv(blockcodi[, -(1:2)]))
  ccodirem <- ccodi %>% filter(irv == 0,
                               irv == max(ccodi$irv))

  # CO Threat Beliefs - one reversed, straight-lining most critical
  blockcotb <- RDCompR[!is.na(RDCompR$COSKN), c(145, 1, 114:119)]
  ccotb <-
    blockcotb %>% cbind('longstr' = longstring(blockcotb),
                        'irv' = irv(blockcotb[, -(1:2)]))
  ccotbrem <- ccotb %>% filter(irv == 0,
                               irv >= max(ccotb$irv[ccotb$irv!=max(ccotb$irv)]))

  # CO Response Beliefs - four reversed, straight-lining most critical
  blockcorb <- RDCompR[!is.na(RDCompR$COSKN), c(145, 1, 120:131)]
  ccorb <-
    blockcorb %>% cbind('longstr' = longstring(blockcorb),
                        'irv' = irv(blockcorb[, -(1:2)]))
  ccorbrem <- ccorb %>% filter(irv == 0,
                               irv >= max(ccorb$irv[ccorb$irv!=max(ccorb$irv)]))

  # CO Personal Moral Norm - none reversed, diagonal-lining most critical
  blockcopn <- RDCompR[!is.na(RDCompR$COSKN), c(145, 1, 132:134)]
  ccopn <-
    blockcopn %>% cbind('longstr' = longstring(blockcopn),
                        'irv' = irv(blockcopn[, -(1:2)]))
  ccopnrem <- ccopn %>% filter(irv == max(ccopn$irv))

  # CO Subjective Norm - none reversed, diagonal-lining more critical
  blockcosn <- RDCompR[!is.na(RDCompR$COSKN), c(145, 1, 135:142)]
  ccosn <-
    blockcosn %>% cbind('longstr' = longstring(blockcosn),
                        'irv' = irv(blockcosn[, -(1:2)]))
  ccosnrem <- ccosn %>% filter(irv == 0,
                               irv >= max(ccosn$irv[ccosn$irv!=max(ccosn$irv)]))

  # Make count table
  rempattern <-
    data.frame("gid" = cpbrem$gid, "psych.irv" = cpbrem$irv) %>%
    full_join(cbirem %>% transmute("gid" = gid, "BI.irv" = irv)) %>%
    full_join(cccknrem %>% transmute("gid" = gid, "CCKN.irv" = irv)) %>%
    full_join(cccdirem %>% transmute("gid" = gid, "CCDI.irv" = irv)) %>%
    full_join(ccctbrem %>% transmute("gid" = gid, "CCTB.irv" = irv)) %>%
    full_join(cccrbrem %>% transmute("gid" = gid, "CCRB.irv" = irv)) %>%
    full_join(cccpnrem %>% transmute("gid" = gid, "CCPN.irv" = irv)) %>%
    full_join(cccsnrem %>% transmute("gid" = gid, "CCSN.irv" = irv)) %>%
    full_join(ccoknrem %>% transmute("gid" = gid, "COKN.irv" = irv)) %>%
    full_join(ccodirem %>% transmute("gid" = gid, "CODI.irv" = irv)) %>%
    full_join(ccotbrem %>% transmute("gid" = gid, "COTB.irv" = irv)) %>%
    full_join(ccorbrem %>% transmute("gid" = gid, "CORB.irv" = irv)) %>%
    full_join(ccopnrem %>% transmute("gid" = gid, "COPN.irv" = irv)) %>%
    full_join(ccosnrem %>% transmute("gid" = gid, "COSN.irv" = irv))
  # Add count for straight-lining blocks
  rempattern$straight.blocks <-
    rowSums(rempattern[, -1] == 0, na.rm = TRUE)
  # Add count for diagonal-lining or polar blocks
  rempattern$polar.blocks <-
    rowSums(rempattern[, -1] > 0, na.rm = TRUE)
  # Add count for pattern blocks
  rempattern$pattern.blocks <-
    rowSums(rempattern %>% select(ends_with("blocks")),
            na.rm = TRUE)

  # Join to completed data dataframe
  datacareless <- RDComp %>% left_join(rempattern[,c(1,16:18)])
  # Post cleaning ----
  # Last check: Implausible responses in free text fields
  # High Age
  datacareless[datacareless$SD1 > 90,]
  # 2 respondents claim to be 100 years old, household size is also implausible > remove SD1 == 100
  # Large household size
  datacareless[datacareless$SD6 > 6,]
  # Same implausible respondents as before
  # Many known persons with COVID 19
  datacareless[datacareless$COS5 > 20,]
  #Many of those answers are implausible, exclude all starting with > remove COS5 >= 55

  # Quality cleaned data from survey 1
  s1dataqualcl <- (datacareless %>%
                     filter(Duration..in.seconds. >= durtm / 2 &
                              (
                                pattern.blocks <= 2 |
                                  is.na(pattern.blocks)
                              )))[, c(3, 145, 143, 6, 8, 4, 5, 7, 9:142)] %>%
    filter(SD1 < 100,
           COS5 < 55)

  # Import data from survey 2 2 ----
  # First 2 rows are descriptors
  # Next 4 rows are tests

  NameRD2 <- data.frame(read_csv(rs2f))
  RD2 <- read_csv(rs2f,
                  skip = 6,
                  col_names = colnames(NameRD2)
  )

  # Crop Data
  # From Descriptive Cols keep only Duration, RecordedDate, ResponseId, gid
  # Crop cols c(1:5,7,10:18)

  RD2Crop <- RD2[,-c(1:5,7,10:18)]

  # Find speeders, easier median method
  RD2CropC <- RD2Crop[RD2Crop$Duration..in.seconds. > median(RD2Crop$Duration..in.seconds.)/3,]

  # Merge data
  sdata <- s1dataqualcl %>% left_join(RD2CropC[!duplicated(RD2CropC$gid),-c(1:3)])

  # Check: Find implausible differences in age
  # As changes in gender identity in the meantime are plausible, only S2 responses of respondents with implausible changes in age are removed
  # However, in the results the gender and age reported in the first survey will be reported
  sdatacheck <- sdata[!is.na(sdata$COB1), c(1:2, 6:7, 143:144)] %>%
    mutate(AgeDiff = (SD1 != Chif2 & SD1 != Chif2 - 1))
  `%notin%` <- Negate(`%in%`)
  # Save final data without identifying GID and potentially identifying free text fields
  sdataqualcl <- sdata
  sdataqualcl[sdataqualcl$ResponseId %in% (sdatacheck %>% filter(AgeDiff == TRUE))$ResponseId,-(1:ncol(s1dataqualcl))] <- NA
  sdataqualclan <- sdataqualcl %>%
    select(!c(gid,
              Anmerkungen))
  return(sdataqualclan)
}

# Include incidence data ----
include_incidence = function(sdc, rs1tf, ri01, ri02){
datafull <- sdc
datachoicetext <- read_csv(rs1tf) %>%
  tibble() %>%
  select(c("ResponseId", "SD8"))

colnames(datachoicetext) <- c("ResponseId", "SD8.text")

datafullk <- datafull %>%
  left_join(datachoicetext)

# As the RKI data is differently named to the survey, a dict has to be created
`%notin%` <- Negate(`%in%`)
# Names in our survey
surveynames <- data.frame(SN.full = unique(datafullk$SD8.text) %>%
                            sort(),
                          SN.type = unique(datafullk$SD8.text) %>%
                            sort()%>%
                            str_split(", ") %>%
                            sapply("[[", 2),
                          SN.name = unique(datafullk$SD8.text) %>%
                            sort() %>%
                            str_split(", ") %>%
                            sapply("[[", 1))
# Load RKI data
incidence <- read_csv2(ri01,
                       skip = 4,
                       col_names = c("Landkreis", "LKNR", "Count210112", "Incidence210112")
) %>%
  tibble() %>% left_join(
    tibble(read_csv2(ri02,
                     skip = 4,
                     col_names = c("Landkreis", "LKNR", "Count210201", "Incidence210201"))
    ),
    by = c("Landkreis", "LKNR")
  )
# names in the RKI data
rkinames <- data.frame(RN.full = incidence$Landkreis,
                       RN.type = incidence$Landkreis %>%
                         str_split(" ") %>%
                         sapply("[[", 1),
                       RN.name = incidence$Landkreis %>%
                         sub(pattern = "^.*? ", replacement = "")) %>%
  mutate(RN.type.long = RN.type %>%
           dplyr::recode(
             LK = "Landkreis",
             SK = "Kreisfreie Stadt"))
# Make dict
# Match entries with matching name and type by name and type
kreisdict1 <- surveynames %>%
  inner_join(rkinames,
             by = c(
               "SN.name" = "RN.name",
               "SN.type" = "RN.type.long"),
             keep = TRUE)
# Rest: Match by name
kreisdict2 <- surveynames[surveynames$SN.full %notin% kreisdict1$SN.full,] %>%
  inner_join(rkinames[rkinames$RN.full %notin% kreisdict1$RN.full,],
             by = c("SN.name" = "RN.name"),
             keep = TRUE)
# Rest: Match manually
kreisdict3 <- surveynames[surveynames$SN.full %notin% kreisdict1$SN.full & surveynames$SN.full %notin% kreisdict2$SN.full,] %>%
  cbind(rkinames[c(6,58,50,116,142,183,196,197,212,238,239,249,268,270,285,146,326,1),])
# Merge dicts
kreisdict <- kreisdict1 %>%
  union(kreisdict2) %>%
  union(kreisdict3)
# Merge dict with incidence
kreisincidence <- kreisdict %>%
  transmute(Landkreis = RN.full,
            ADSurvey = SN.full) %>%
  left_join(incidence)

# Merge with survey data
datafullwin <- datafullk %>%
  left_join(kreisincidence,
            by = c("SD8.text" = "ADSurvey"))
return(datafullwin)
}


# Examine data for parametricity ----
param_clean = function(sdc){
data <- sdc

# Psychological questions
datapsych <- as.matrix(data %>% select(starts_with("BF")))
dataskpsych <-
  data.frame(
    Item = colnames(datapsych),
    Skewness = skew(datapsych),
    Kurtosis = kurtosi(datapsych)
  )
dataskcritpsych <-
  dataskpsych %>% filter(Skewness >= 1 |
                            Skewness <= -1 | Kurtosis >= 1 | Kurtosis <= -1)
# BFC3 exhibits skewness and kurtosis beyond the threshold of -1...1, with kurtosis > 2
# exclude none

# Climate Crisis questions
datacc <-
  as.matrix(data %>% select(starts_with("CC") &
                               !c("CCS1", "CCS2", "CCS3", "CCS4")) %>% filter(!is.na(CCSKN)))
dataskcc <-
  data.frame(
    Item = colnames(datacc),
    Skewness = skew(datacc),
    Kurtosis = kurtosi(datacc)
  )
dataskcritcc <-
  dataskcc %>% filter(Skewness >= 1 | Skewness <= -1 | Kurtosis >= 1 | Kurtosis <= -1)
# CCKN4 and CCTB4 exhibit problematical skewness
# CCBI2, CCKN1 CCKN6 and CCIN3 exhibit problematical kurtosis
# CCPN3 exhibits both problematical skewness and kurtosis
# none except CCB3 with an absolute value > 2
# exclude CCB3
dataskvcritcc <-
  dataskcc %>% filter(Skewness >= 5 |
                       Skewness <= -5 | Kurtosis >= 5 | Kurtosis <= -5)
# COVID-19 questions
dataco <-
  as.matrix(data %>% select(starts_with("CO") &
                               !paste0("COS", 1:7)) %>% filter(!is.na(COSKN)))
dataskco <-
  data.frame(
    Item = colnames(dataco),
    Skewness = skew(dataco),
    Kurtosis = kurtosi(dataco)
  )
dataskcritco <-dataskco %>%
  filter(Skewness >= 1 | Skewness <= -1 | Kurtosis >= 1 | Kurtosis <= -1)
# Most CO items exhibit skewness or kurtosis
# exclude COB1, COB3, COB4
dataskvcritco <- dataskco %>%
  filter(Skewness >= 5 | Skewness <= -5 | Kurtosis >= 5 | Kurtosis <= -5)

# return (nearly) parametric data
datanormal <-  data %>%
  select(!dataskvcritco$Item) %>%
  select(!dataskvcritcc$Item)
return(datanormal)
}

# Treat missing data ----
treat_missing = function(data) {
data <- data

# Data should be only missing (=7) for the colleagues questions, i.e., CCIN3, CCNDN3, COIN3, CODN3
# Data should only be replaced if amount < 15%
nrow(data %>% filter(CCIN3 == 7))/nrow(data %>% filter(!is.na(CCIN3)))
# 20.4 % -> drop CCIN3
nrow(data %>% filter(CCDN3 == 7))/nrow(data %>% filter(!is.na(CCIN3)))
# 19.5 % -> drop CCDN3 (strange difference)
nrow(data %>% filter(COIN3 == 7))/nrow(data %>% filter(!is.na(COIN3)))
# 30.0 % -> drop COIN3
nrow(data %>% filter(CODN3 == 7))/nrow(data %>% filter(!is.na(COIN3)))
# 30.0 % -> drop CODN3

# Is there any other data randomly missing?
# cc data
cctest <- data %>% select(starts_with("CC")) %>% filter(!is.na(CCKN1))
apply(is.na(cctest), 2, which)
# CCDN3 has one missing value, CCDN4 has two
# Isolate respondents
missingr <- data %>% filter(is.na(CCDN4) & !is.na(CCKN1))
# Demographic data: 18yo male and 40yo female
# Find males closest in age
imput1a <- data %>% filter(!is.na(CCDN4) & !is.na(CCKN1) & SD2 == 2)
x1a <- imput1a$SD1[abs(imput1a$SD1-18) %in% sort(abs(imput1a$SD1-18),partial=1:10)[1:10]]
imput1 <- mean(unlist(imput1a[imput1a$SD1 %in% x1a,"CCDN4"]))
# Find females closest in ages
imput2a <- data %>% filter(!is.na(CCDN4) & !is.na(CCKN1) & SD2 == 1)
x2a <- imput2a$SD1[abs(imput2a$SD1-40) %in% sort(abs(imput2a$SD1-40),partial=1:10)[1:10]]
imput2 <- mean(unlist(imput2a[imput2a$SD1 %in% x2a,"CCDN4"]))

#co data
cotest <- data %>% select(starts_with("CO")) %>% filter(!is.na(COKN2))

#Imputate mean replacement values
dataimp <- data
dataimp[dataimp$ResponseId == missingr$ResponseId[1],"CCDN4"] <- imput1
dataimp[dataimp$ResponseId == missingr$ResponseId[2],"CCDN4"] <- imput2

# Delete other values
datanomissing <- dataimp %>% select(-c("CCIN3", "CCDN3", "COIN3", "CODN3"))
return(datanomissing)
}


# Code data for additional analysis ----

code_data_addan = function(data, model){
  #Import Data
  # Data used is nonnormal, missing values are not treated
  datafull <- data
  # Replace 7 for CCDN3, CCIN3, CODN3 and COIN3 with NA
  datafull$CCDN3[datafull$CCDN3 == 7] <- NA
  datafull$CCIN3[datafull$CCIN3 == 7] <- NA
  datafull$CODN3[datafull$CODN3 == 7] <- NA
  datafull$COIN3[datafull$COIN3 == 7] <- NA

  #Make scales from personality data
  anper <- data.frame(Variable = c(rep("Extraversion",3),
                                   rep("Agreeableness",3),
                                   rep("Conscientiousness",3),
                                   rep("Neuroticism",3),
                                   rep("Openness to Experience",3),
                                   rep("Internal Control Conviction",2),
                                   rep("External Control Conviction",2)),
                      Item = c(paste0("BFE", 1:3),
                               paste0("BFA", 1:3),
                               paste0("BFC", 1:3),
                               paste0("BFN", 1:3),
                               paste0("BFO", 1:3),
                               paste0("IC", 1:2),
                               paste0("EC", 1:2))
  )
  anperun <- data.frame(Variable = unique(anper$Variable))
  anperunalpha <- c()
  # Analyze Cronbach's Alpha
  for (i in 1:nrow(anperun)){
    x = anperun[i,1]
    anperunalpha <- append(anperunalpha,
                           ((unlist(psych::alpha(datafull[unlist((filter(anper, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]))
    print(psych::alpha(datafull[unlist((filter(anper, Variable == {{x}}))["Item"])], check.keys = FALSE))
  }
  anperun <- anperun %>% cbind(Cs.Alpha = anperunalpha)
  anperalpha <- anper %>% left_join(anperun)
  # Cronbach's alpha is insufficient for all constructs except Big Five Neuroticism
  # BFE: No alpha >0.7 attainable through item deletion, choose item with highest item-total correlation (r.drop): BFE2
  # BFA: No alpha > 0.7 attainable through item deletion, choose item with highest item-total correlation: BFA1
  # BFC: see above,choose BFC2
  # BFO: Choose BFO3
  # IC: Choose IC2 (content)
  # EC: Choose EC1 (content)
  keepper <- c("BFE2", "BFA1", "BFC2", "BFN1", "BFN2", "BFN3", "BFO3", "IC2", "EC1")
  codeper <- anper %>% filter(Item %in% keepper)
  anperalpha2 <- anperalpha %>% filter(Item %in% keepper)
  anperalpha2[anperalpha2 < 0.7] <- NA

  # Make scales for CC data

  #Climate Change
  mmcc <- model$measurement_model
  ancc <- data.frame(Variable = (unlist(mmcc))[c(T, F, F)],
                     Item = (unlist(mmcc))[c(F, T, F)]) %>%
    filter(!(Item %in% Variable)) %>%
    filter(!(Item == "CCKN")) %>%
    rbind(data.frame(Variable = c(rep("Descriptive Norm", 2),
                                  rep("Injunctive Norm", 2),
                                  paste0(c("Perceived Self-Efficacy",
                                           "Perceived Response Efficacy",
                                           "Perceived Response Costs",
                                           "Behavioral Intention"),
                                         c(rep(" Diet", 4),
                                           rep(" Heating",4),
                                           rep(" Driving",4),
                                           rep(" General",4),
                                           rep("",4))),
                                  "Subjective Knowledge",
                                  paste0("Behavior", c(rep("",4),
                                                       " Diet",
                                                       " Heating",
                                                       " Driving",
                                                       " General"))
    ),
    Item = c(paste0("CC",
                    c(paste0("DN", 3:4),
                      paste0("IN", 3:4),
                      paste0("RB", 1:3),
                      "BI1",
                      paste0("RB", 4:6),
                      "BI2",
                      paste0("RB", 7:9),
                      "BI3",
                      rep(c(paste0("RB",10:12),
                            "BI4"),2),
                      "SKN",
                      rep(paste0("B", 1:4),2))))
    ))
  ancc[ancc == "Benevolence"] <- "Distrusting Beliefs Benevolence"
  ancc[ancc == "Competence"] <- "Distrusting Beliefs Competence"
  ancc[ancc == "Integrity"] <- "Distrusting Beliefs Integrity"
  anccun <- data.frame(Variable = unique(ancc$Variable))
  anccunalpha <- c()
  # Analyze Cronbach's Alpha
  for (i in 1:nrow(anccun)){
    x = anccun[i,1]
    anccunalpha <- append(anccunalpha,
                          ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
                                 ((unlist(psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]), NA))
    # print(ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
    #              psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE), NA))
  }
  anccun <- anccun %>% cbind(Cs.Alpha = anccunalpha)
  anccalpha <- ancc %>% left_join(anccun)
  # Cronbach's alpha is sufficient for
  # CC Perceived Self-Efficacy
  # CC Perceived Response Efficacy
  # CC Distrusting Beliefs Benevolence, Competence, Integrity
  # CC Perceived Susceptibility
  # CC Perceived Severity
  # CC Personal Moral Norm
  # CC Descriptive Norm
  # CC Injunctive Norm
  # CC Behavioral Intention
  # CC Behavior
  # Cronbach's alpha is insufficient for
  # CC Perceived Response Costs
  psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:9,12)))))
  # Without CCRB9:
  psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:8,12)))))
  # Use CCRB12
  codecc <- ancc %>% filter(!(Item %in% paste0("CCRB", 7:9))) %>%
    mutate(Variable = paste0("Climate Crisis ", Variable))

  # Make scales for CO data

  #COVID-19
  mmco <- model$measurement_model
  anco <- data.frame(Variable = (unlist(mmco))[c(T, F, F)],
                     Item = (unlist(mmco))[c(F, T, F)] %>% str_replace("CC", "CO")) %>%
    filter(!(Item %in% Variable)) %>%
    filter(!(Item == "COKN")) %>%
    rbind(data.frame(Variable = c(rep("Descriptive Norm", 2),
                                  rep("Injunctive Norm", 2),
                                  paste0(c("Perceived Self-Efficacy",
                                           "Perceived Response Efficacy",
                                           "Perceived Response Costs",
                                           "Behavioral Intention"),
                                         c(rep(" Contact", 4),
                                           rep(" App",4),
                                           rep(" Mask",4),
                                           rep(" General",4),
                                           rep("",4))),
                                  "Subjective Knowledge",
                                  paste0("Behavior", c(rep("",4),
                                                       " Contact",
                                                       " App",
                                                       " Mask",
                                                       " General"))
    ),
    Item = c(paste0("CO",
                    c(paste0("DN", 3:4),
                      paste0("IN", 3:4),
                      paste0("RB", 1:3),
                      "BI1",
                      paste0("RB", 4:6),
                      "BI2",
                      paste0("RB", 7:9),
                      "BI3",
                      rep(c(paste0("RB",10:12),
                            "BI4"),2),
                      "SKN",
                      rep(paste0("B", 1:4),2))))
    ))
  anco[anco == "Benevolence"] <- "Distrusting Beliefs Benevolence"
  anco[anco == "Competence"] <- "Distrusting Beliefs Competence"
  anco[anco == "Integrity"] <- "Distrusting Beliefs Integrity"
  ancoun <- data.frame(Variable = unique(anco$Variable))
  ancounalpha <- c()
  # Analyze Cronbach's Alpha
  for (i in 1:nrow(ancoun)){
    x = ancoun[i,1]
    ancounalpha <- append(ancounalpha,
                          ifelse(test = ncol(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])]) > 1,
                                 ((unlist(psych::alpha(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]), NA))
    # print(ifelse(test = ncol(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])]) > 1,
    #               psych::alpha(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])], check.keys = FALSE), NA))
  }
  ancoun <- ancoun %>% cbind(Cs.Alpha = ancounalpha)
  ancoalpha <- anco %>% left_join(ancoun)
  # Cronbach's alpha is sufficient for all constructs
  codeco <- anco %>% mutate(Variable = paste0("COVID-19 ", Variable))

  codefull <- codeco %>% union(codecc) %>% union(codeper)
  codefullun <- unique(codefull$Variable)



  datacoded <- data.frame(Dummy = character(length = nrow(datafull)))
  for (i in 1:length(codefullun)){
    x = codefullun[i]
    datacoded <- datacoded %>%
      cbind(Bob = rowMeans((datafull %>% select(unlist((codefull %>% filter(Variable == {{x}}))[,2]))), na.rm = TRUE))
  }
  datacoded <- datacoded[,-1]
  colnames(datacoded) <- codefullun

  # make descriptive statistics
  descstat <- data.frame(Variable = rownames(psych::describe(datacoded)), psych::describe(datacoded)) %>%
    left_join(rbind(anperalpha,
                    (anccalpha %>% mutate(Variable = paste0("Climate Crisis ", Variable))),
                    (ancoalpha %>% mutate(Variable = paste0("COVID-19 ", Variable)))) %>%
                group_by(Variable) %>%
                mutate(Items = paste0(Item, collapse = ", "),
                       Item = NULL) %>%
                distinct(Variable, .keep_all = TRUE)

    )


  # order by groups and alphabetical
  descstatalph <- descstat[c(order(descstat$Variable)[order(descstat$Variable) %in% grep("Climate Crisis", descstat$Variable)],
                             order(descstat$Variable)[order(descstat$Variable) %in% grep("COVID-19", descstat$Variable)],
                             66:72),]

  # Append Knowledge data
  datacodedkn <- datacoded %>%
    cbind(datafull %>% select((contains("KN")&!(contains("SKN")))))
  datacodedkn$'Climate Crisis Knowledge Sum Correct' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 3)
  datacodedkn$'Climate Crisis Knowledge Sum DK' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 2)
  datacodedkn$'Climate Crisis Knowledge Sum Incorrect' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 1)
  datacodedkn$'COVID-19 Knowledge Sum Correct' <- rowSums(datacodedkn %>% select(contains("COKN")) == 3)
  datacodedkn$'COVID-19 Knowledge Sum DK' <- rowSums(datacodedkn %>% select(contains("COKN")) == 2)
  datacodedkn$'COVID-19 Knowledge Sum Incorrect' <- rowSums(datacodedkn %>% select(contains("COKN")) == 1)

  # Append demographic and other vars
  datarest <- datafull %>% dplyr::transmute(Gender = car::recode(SD2,
                                                                 "1 = 'female';
                                                              2 = 'male';
                                                              3 = 'other'",
                                                                 as.factor = TRUE),
                                            Age = SD1,
                                            Education = car::recode(SD3,
                                                                    "1 = 'still a student';
                                                                 2 = 'dropped out of school';
                                                                 c(3,4) = 'secondary school leaving certificate';
                                                                 5 = 'university entrance qualification';
                                                                 6 = 'university degree';
                                                                 7 = 'doctorate';
                                                                 8 =  'a different level of education'",
                                                                    as.factor = TRUE,
                                                                    levels = c('still a student',
                                                                               'dropped out of school',
                                                                               'secondary school leaving certificate',
                                                                               'university entrance qualification',
                                                                               'university degree',
                                                                               'doctorate',
                                                                               'a different level of education')),
                                            Occupation = car::recode(SD4,
                                                                     "1 = 'employed full-time';
                                                                  2 = 'employed part-time';
                                                                  3 = 'in vocational training';
                                                                  4 = 'student (university)';
                                                                  5 = 'student (school)';
                                                                  6 = 'not in paid employment'",
                                                                     as.factor = TRUE,
                                                                     levels = c('employed full-time',
                                                                                'employed part-time',
                                                                                'in vocational training',
                                                                                'student (university)',
                                                                                'student (school)',
                                                                                'not in paid employment')),
                                            Income = car::recode(SD5,
                                                                 "1 = 'up to 450 EUR';
                                                                2 = 'between 451 and 1000 EUR';
                                                                3 = 'between 1001 and 1500 EUR';
                                                                4 = 'between 1501 and 2000 EUR';
                                                                5 = 'between 2001 and 2500 EUR';
                                                                6 = 'between 2501 and 3000 EUR';
                                                                7 = 'between 3001 and 3500 EUR';
                                                                8 = 'between 3501 and 4000 EUR';
                                                                9 = 'between 4001 and 4500 EUR';
                                                                10 = 'between 4501 and 5000 EUR';
                                                                11 = 'more than 5000 EUR';
                                                                12 = 'not specified';
                                                                else = 'not specified'",
                                                                 as.factor = TRUE,
                                                                 levels = c('up to 450 EUR',
                                                                            'between 451 and 1000 EUR',
                                                                            'between 1001 and 1500 EUR',
                                                                            'between 1501 and 2000 EUR',
                                                                            'between 2001 and 2500 EUR',
                                                                            'between 2501 and 3000 EUR',
                                                                            'between 3001 and 3500 EUR',
                                                                            'between 3501 and 4000 EUR',
                                                                            'between 4001 and 4500 EUR',
                                                                            'between 4501 and 5000 EUR',
                                                                            'more than 5000 EUR',
                                                                            'not specified')),
                                            Income2 = car::recode(SD5,
                                                                  "c(1,2) = 'up to 1000 EUR';
                                                                c(3,4) = 'between 1001 and 2000 EUR';
                                                                c(5,6) = 'between 2001 and 3000 EUR';
                                                                c(7,8) = 'between 3001 and 4000 EUR';
                                                                c(9,10) = 'between 4001 and 5000 EUR';
                                                                11 = 'more than 5000 EUR';
                                                                12 = 'not specified';
                                                                else = 'not specified'",
                                                                  as.factor = TRUE,
                                                                  levels = c('up to 1000 EUR',
                                                                             'between 1001 and 2000 EUR',
                                                                             'between 2001 and 3000 EUR',
                                                                             'between 3001 and 4000 EUR',
                                                                             'between 4001 and 5000 EUR',
                                                                             'more than 5000 EUR',
                                                                             'not specified')),
                                            'Household Size' = SD6,
                                            'Household Children' = ifelse(is.na(SD7),0,as.numeric(SD7)),
                                            'Own Risk COVID-19' = car::recode(COS1,
                                                                              "2 = 'do not know';
                                                                        1 = 'no';
                                                                        3 = 'yes'",
                                                                              as.factor = TRUE,
                                                                              levels = c('no', 'do not know', 'yes')),
                                            'Own Infection COVID-19' = car::recode(COS2,
                                                                                   "2 = 'do not know';
                                                                        1 = 'no';
                                                                        3 = 'yes'",
                                                                                   as.factor = TRUE,
                                                                                   levels = c('no', 'do not know', 'yes')),
                                            'Own Hospitalisation COVID-19' = car::recode(COS3,
                                                                                         "1 = 'no';
                                                                                    3 = 'yes'",
                                                                                         as.factor = TRUE,
                                                                                         levels = c('no', 'yes')),
                                            'Cohabitate Risk COVID-19' = car::recode(COS4,
                                                                                     "2 = 'do not know';
                                                                        1 = 'no';
                                                                        3 = 'yes';
                                                                        NA = 'no'",
                                                                                     as.factor = TRUE,
                                                                                     levels = c('no', 'do not know', 'yes')),
                                            'Acquaintances Infection COVID-19' = COS5,
                                            'Acquaintances Hospitalisation COVID-19' = ifelse(is.na(COS6), 0, COS6),
                                            'Work From Home COVID-19' = car::recode(COS7,
                                                                                    "1 = 'no';
                                                                                    3 = 'yes'",
                                                                                    as.factor = TRUE,
                                                                                    levels = c('no', 'yes')),
                                            'AD S1 Count COVID-19' = Count210112,
                                            'AD S2 Count COVID-19' = Count210201,
                                            'AD S1 Incidence COVID-19' = Incidence210112,
                                            'AD S2 Incidence COVID-19' = Incidence210201,
                                            'Diet' = car::recode(CCS1,
                                                                 "1 = 'vegan';
                                                           2 = 'vegetarian';
                                                           3 = 'eat meat less than once a week';
                                                           4 = 'eat meat once a week';
                                                           5 = 'eat meat several times a week';
                                                           6 = 'eat meat every day'",
                                                                 as.factor = TRUE,
                                                                 levels = c('vegan',
                                                                            'vegetarian',
                                                                            'eat meat less than once a week',
                                                                            'eat meat once a week',
                                                                            'eat meat several times a week',
                                                                            'eat meat every day')),
                                            'Car Ownership' = car::recode(CCS2,
                                                                          "1 = 'does not own a car';
                                                                    2 = 'does not own a car but can access one';
                                                                    3 = 'owns a car'",
                                                                          as.factor = TRUE,
                                                                          levels = c('does not own a car',
                                                                                     'does not own a car but can access one',
                                                                                     'owns a car')),
                                            'Distance Driven Private' = car::recode(CCS3,
                                                                                    "1 = 'none';
                                                                              2 = 'up to 5,000 km';
                                                                              3 = 'between 5,001 and 10,000 km';
                                                                              4 = 'between 10,001 and 15,000 km';
                                                                              5 = 'between 15,001 and 20,000 km';
                                                                              6 = 'more than 20.000 km';
                                                                              7 = 'do not know'",
                                                                                    as.factor = TRUE,
                                                                                    levels = c('none',
                                                                                               'up to 5,000 km',
                                                                                               'between 5,001 and 10,000 km',
                                                                                               'between 10,001 and 15,000 km',
                                                                                               'between 15,001 and 20,000 km',
                                                                                               'more than 20.000 km',
                                                                                               'do not know')),
                                            'Distance Driven Business' = car::recode(CCS4,
                                                                                     "1 = 'none';
                                                                              2 = 'up to 5,000 km';
                                                                              3 = 'between 5,001 and 10,000 km';
                                                                              4 = 'between 10,001 and 15,000 km';
                                                                              5 = 'between 15,001 and 20,000 km';
                                                                              6 = 'more than 20.000 km';
                                                                              7 = 'do not know'",
                                                                                     as.factor = TRUE,
                                                                                     levels = c('none',
                                                                                                'up to 5,000 km',
                                                                                                'between 5,001 and 10,000 km',
                                                                                                'between 10,001 and 15,000 km',
                                                                                                'between 15,001 and 20,000 km',
                                                                                                'more than 20.000 km',
                                                                                                'do not know'))
  )
  datacodedfull <- datacodedkn %>% cbind(datarest)
  return(
    list(
      coded = datacodedfull,
      descriptive = descstatalph
    )
    )
}
