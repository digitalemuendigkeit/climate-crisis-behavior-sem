library(tidyverse)
library(careless)
library(hcictools)

# Import, clean and anonymize data ----
sica = function(rs1f, rs2f) {
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
