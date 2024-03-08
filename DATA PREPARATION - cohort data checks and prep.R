###################### Length of stay 2023######################

#Exploratory checking of diagnosis cohorts

#Created November 2023 by Lizzie Augarde 
#Change log:
#15/11/2023 checked approach for excluding same day deaths and 
#diagnoses and those with diagnosis after death
#18/01/2024 removed code for 2015 and 2016 diagnoses
############################################################### 

#prep
library(NDRSAfunctions)
library(tidyverse)
library(lubridate)

casref01 <- createConnection()

#extract
los2014_cohort <- dbGetQueryOracle(casref01, "SELECT * FROM LOS2014_COHORT", rowlimit = NA)

#checking for duplicates/multiple tumours
nrow(los2014_cohort[duplicated(los2014_cohort), ])
n_distinct(los2014_cohort$PATIENTID)

multiple_tumours_2014 <- dbGetQueryOracle(casref01, 
                                          "SELECT TUMOURID, PATIENTID, DIAGNOSISDATEBEST, STAGE_BEST
                                          FROM AV2021.AT_TUMOUR_ENGLAND@CASREF01
                                          WHERE DIAGNOSISYEAR = 2014
                                          AND CASCADE_INCI_FLAG = 1
                                          AND DCO = 'N'", 
                                          rowlimit = NA)

multiple_tumours_2014 <- multiple_tumours_2014 %>%
  group_by(PATIENTID) %>%
  mutate(tumour_count = n()) 

test1 <- los2014_cohort %>% filter(PATIENTID == "103376581") #multiple tumours but only 1 stage 3, this should be the only one in cohort, 9/8/14
test2 <- los2014_cohort %>% filter(PATIENTID == "277334846") #2 tumours, stage 2 and 3, should include earlier one only 9/4/14

#checking for postmortem diagnoses/same day diagnoses and deaths
los2014_cohort <- los2014_cohort %>% 
  mutate(death_diag_comp = interval(DIAGNOSISDATEBEST, DEATHDATEBEST) / days(1))

nrow(los2014_cohort[los2014_cohort$death_diag_comp < 1 & !is.na(los2014_cohort$death_diag_comp), ])

los2014_cohort <- los2014_cohort %>% clean_names()



