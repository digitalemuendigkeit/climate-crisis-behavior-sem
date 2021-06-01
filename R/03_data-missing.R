library(tidyverse)

data1 <- read_rds(here::here("data",
                             "anonymized",
                             "S1-data-nd.RDS"))

# Data should be only missing (=7) for the colleagues questions, i.e., CCIN3, CCNDN3, COIN3, CODN3
# Data should only be replaced if amount < 15%
nrow(data1 %>% filter(CCIN3 == 7))/nrow(data1 %>% filter(!is.na(CCIN3)))
# 20.4 % -> drop CCIN3
nrow(data1 %>% filter(CCDN3 == 7))/nrow(data1 %>% filter(!is.na(CCIN3)))
# 19.5 % -> drop CCDN3 (strange difference)
nrow(data1 %>% filter(COIN3 == 7))/nrow(data1 %>% filter(!is.na(COIN3)))
# 30.0 % -> drop COIN3
nrow(data1 %>% filter(CODN3 == 7))/nrow(data1 %>% filter(!is.na(COIN3)))
# 30.0 % -> drop CODN3

# Is there any other data randomly missing?
# cc data
cctest <- data1 %>% select(starts_with("CC")) %>% filter(!is.na(CCKN1))
apply(is.na(cctest), 2, which)
# CCDN3 has one missing value, CCDN4 has two
# Isolate respondents
missingr <- data1 %>% filter(is.na(CCDN4) & !is.na(CCKN1))
# Demographic data: 18yo male and 40yo female
# Find males closest in age
imput1a <- data1 %>% filter(!is.na(CCDN4) & !is.na(CCKN1) & SD2 == 2)
x1a <- imput1a$SD1[abs(imput1a$SD1-18) %in% sort(abs(imput1a$SD1-18),partial=1:10)[1:10]]
imput1 <- mean(unlist(imput1a[imput1a$SD1 %in% x1a,"CCDN4"]))
# Find females closest in ages
imput2a <- data1 %>% filter(!is.na(CCDN4) & !is.na(CCKN1) & SD2 == 1)
x2a <- imput2a$SD1[abs(imput2a$SD1-40) %in% sort(abs(imput2a$SD1-40),partial=1:10)[1:10]]
imput2 <- mean(unlist(imput2a[imput2a$SD1 %in% x2a,"CCDN4"]))

#co data
cotest <- data1 %>% select(starts_with("CO")) %>% filter(!is.na(COKN2))

#Imputate mean replacement values
dataimp <- data1
dataimp[dataimp$ResponseId == missingr$ResponseId[1],"CCDN4"] <- imput1
dataimp[dataimp$ResponseId == missingr$ResponseId[2],"CCDN4"] <- imput2

# Delete other values
datanomissing <- dataimp %>% select(-c("CCIN3", "CCDN3", "COIN3", "CODN3"))
write_csv2(datanomissing,
           here::here("data",
                      "anonymized",
                      "S1-Data-nm.csv"))
write_rds(datanomissing,
          here::here("data",
                     "anonymized",
                     "S1-Data-nm.RDS"))
