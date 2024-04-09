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

#checking for duplicates/multiple tumours ######
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


##### CREATING AGE VARIABLES ########
los2014_cohort <- los2014_cohort %>% clean_names()

los2014_cohort_agevars <- los2014_cohort %>%
  mutate(patientid = as.character(patientid)) %>%
  select(c(patientid, diagnosisdatebest, birthdatebest)) %>%
  mutate(diag_age_days = difftime(as.Date(diagnosisdatebest), as.Date(birthdatebest), units = "days")) %>%
  
  #ageing on
  mutate(age_3months_postdiag = as.numeric(diag_age_days + 92),
         age_6months_postdiag = as.numeric(diag_age_days + 182),
         age_9months_postdiag = as.numeric(diag_age_days + 274),
         age_12months_postdiag = as.numeric(diag_age_days + 365),
         age_1.5years_postdiag = as.numeric(diag_age_days + 549),
         age_2years_postdiag = as.numeric(diag_age_days + 730),
         age_2.5years_postdiag = as.numeric(diag_age_days + 913),
         age_3years_postdiag = as.numeric(diag_age_days + 1095),
         age_3.5years_postdiag = as.numeric(diag_age_days + 1278),
         age_4years_postdiag = as.numeric(diag_age_days + 1460),
         age_4.5years_postdiag = as.numeric(diag_age_days + 1643),
         age_5years_postdiag = as.numeric(diag_age_days + 1825)) %>%
  mutate(age_3months_postdiag = floor(age_3months_postdiag/365),
         age_6months_postdiag = floor(age_6months_postdiag/365),
         age_9months_postdiag = floor(age_9months_postdiag/365),
         age_12months_postdiag = floor(age_12months_postdiag/365),
         age_1.5years_postdiag = floor(age_1.5years_postdiag/365),
         age_2years_postdiag = floor(age_2years_postdiag/365),
         age_2.5years_postdiag = floor(age_2.5years_postdiag/365),
         age_3years_postdiag = floor(age_3years_postdiag/365),
         age_3.5years_postdiag = floor(age_3.5years_postdiag/365),
         age_4years_postdiag = floor(age_4years_postdiag/365),
         age_4.5years_postdiag = floor(age_4.5years_postdiag/365),
         age_5years_postdiag = floor(age_5years_postdiag/365)) %>%
  
  #converting to age groups ----trying converting this to a function, this is a mess!!!
  mutate(age_3months_postdiag = case_when(age_3months_postdiag < 10 ~ "0-9", age_3months_postdiag < 20 & age_3months_postdiag > 9 ~ "10-19", age_3months_postdiag < 30 & age_3months_postdiag > 19 ~ "20-29",
                                        age_3months_postdiag < 40 & age_3months_postdiag > 29 ~ "30-39", age_3months_postdiag < 50 & age_3months_postdiag > 39 ~ "40-49", age_3months_postdiag < 60 & age_3months_postdiag > 49 ~ "50-59",
                                        age_3months_postdiag < 70 & age_3months_postdiag > 59 ~ "60-69", age_3months_postdiag < 80 & age_3months_postdiag > 69 ~ "70-79", age_3months_postdiag > 79 ~ "80+")) %>%
  mutate(age_6months_postdiag = case_when(age_6months_postdiag < 10 ~ "0-9", age_6months_postdiag < 20 & age_6months_postdiag > 9 ~ "10-19", age_6months_postdiag < 30 & age_6months_postdiag > 19 ~ "20-29",
                                        age_6months_postdiag < 40 & age_6months_postdiag > 29 ~ "30-39", age_6months_postdiag < 50 & age_6months_postdiag > 39 ~ "40-49", age_6months_postdiag < 60 & age_6months_postdiag > 49 ~ "50-59",
                                        age_6months_postdiag < 70 & age_6months_postdiag > 59 ~ "60-69", age_6months_postdiag < 80 & age_6months_postdiag > 69 ~ "70-79", age_6months_postdiag > 79 ~ "80+")) %>%
  mutate(age_9months_postdiag = case_when(age_9months_postdiag < 10 ~ "0-9", age_9months_postdiag < 20 & age_9months_postdiag > 9 ~ "10-19", age_9months_postdiag < 30 & age_9months_postdiag > 19 ~ "20-29",
                                        age_9months_postdiag < 40 & age_9months_postdiag > 29 ~ "30-39", age_9months_postdiag < 50 & age_9months_postdiag > 39 ~ "40-49", age_9months_postdiag < 60 & age_9months_postdiag > 49 ~ "50-59",
                                        age_9months_postdiag < 70 & age_9months_postdiag > 59 ~ "60-69", age_9months_postdiag < 80 & age_9months_postdiag > 69 ~ "70-79", age_9months_postdiag > 79 ~ "80+")) %>%
  mutate(age_12months_postdiag = case_when(age_12months_postdiag < 10 ~ "0-9", age_12months_postdiag < 20 & age_12months_postdiag > 9 ~ "10-19", age_12months_postdiag < 30 & age_12months_postdiag > 19 ~ "20-29",
                                      age_12months_postdiag < 40 & age_12months_postdiag > 29 ~ "30-39", age_12months_postdiag < 50 & age_12months_postdiag > 39 ~ "40-49", age_12months_postdiag < 60 & age_12months_postdiag > 49 ~ "50-59",
                                      age_12months_postdiag < 70 & age_12months_postdiag > 59 ~ "60-69", age_12months_postdiag < 80 & age_12months_postdiag > 69 ~ "70-79", age_12months_postdiag > 79 ~ "80+")) %>%
  mutate(age_1.5years_postdiag = case_when(age_1.5years_postdiag < 10 ~ "0-9", age_1.5years_postdiag < 20 & age_1.5years_postdiag > 9 ~ "10-19", age_1.5years_postdiag < 30 & age_1.5years_postdiag > 19 ~ "20-29",
                                         age_1.5years_postdiag < 40 & age_1.5years_postdiag > 29 ~ "30-39", age_1.5years_postdiag < 50 & age_1.5years_postdiag > 39 ~ "40-49", age_1.5years_postdiag < 60 & age_1.5years_postdiag > 49 ~ "50-59",
                                         age_1.5years_postdiag < 70 & age_1.5years_postdiag > 59 ~ "60-69", age_1.5years_postdiag < 80 & age_1.5years_postdiag > 69 ~ "70-79", age_1.5years_postdiag > 79 ~ "80+")) %>%
  mutate(age_2years_postdiag = case_when(age_2years_postdiag < 10 ~ "0-9", age_2years_postdiag < 20 & age_2years_postdiag > 9 ~ "10-19", age_2years_postdiag < 30 & age_2years_postdiag > 19 ~ "20-29",
                                       age_2years_postdiag < 40 & age_2years_postdiag > 29 ~ "30-39", age_2years_postdiag < 50 & age_2years_postdiag > 39 ~ "40-49", age_2years_postdiag < 60 & age_2years_postdiag > 49 ~ "50-59",
                                       age_2years_postdiag < 70 & age_2years_postdiag > 59 ~ "60-69", age_2years_postdiag < 80 & age_2years_postdiag > 69 ~ "70-79", age_2years_postdiag > 79 ~ "80+")) %>%
  mutate(age_2.5years_postdiag = case_when(age_2.5years_postdiag < 10 ~ "0-9", age_2.5years_postdiag < 20 & age_2.5years_postdiag > 9 ~ "10-19", age_2.5years_postdiag < 30 & age_2.5years_postdiag > 19 ~ "20-29",
                                         age_2.5years_postdiag < 40 & age_2.5years_postdiag > 29 ~ "30-39", age_2.5years_postdiag < 50 & age_2.5years_postdiag > 39 ~ "40-49", age_2.5years_postdiag < 60 & age_2.5years_postdiag > 49 ~ "50-59",
                                         age_2.5years_postdiag < 70 & age_2.5years_postdiag > 59 ~ "60-69", age_2.5years_postdiag < 80 & age_2.5years_postdiag > 69 ~ "70-79", age_2.5years_postdiag > 79 ~ "80+")) %>%
  mutate(age_3years_postdiag = case_when(age_3years_postdiag < 10 ~ "0-9", age_3years_postdiag < 20 & age_3years_postdiag > 9 ~ "10-19", age_3years_postdiag < 30 & age_3years_postdiag > 19 ~ "20-29",
                                       age_3years_postdiag < 40 & age_3years_postdiag > 29 ~ "30-39", age_3years_postdiag < 50 & age_3years_postdiag > 39 ~ "40-49", age_3years_postdiag < 60 & age_3years_postdiag > 49 ~ "50-59",
                                       age_3years_postdiag < 70 & age_3years_postdiag > 59 ~ "60-69", age_3years_postdiag < 80 & age_3years_postdiag > 69 ~ "70-79", age_3years_postdiag > 79 ~ "80+")) %>%
  mutate(age_3.5years_postdiag = case_when(age_3.5years_postdiag < 10 ~ "0-9", age_3.5years_postdiag < 20 & age_3.5years_postdiag > 9 ~ "10-19", age_3.5years_postdiag < 30 & age_3.5years_postdiag > 19 ~ "20-29",
                                         age_3.5years_postdiag < 40 & age_3.5years_postdiag > 29 ~ "30-39", age_3.5years_postdiag < 50 & age_3.5years_postdiag > 39 ~ "40-49", age_3.5years_postdiag < 60 & age_3.5years_postdiag > 49 ~ "50-59",
                                         age_3.5years_postdiag < 70 & age_3.5years_postdiag > 59 ~ "60-69", age_3.5years_postdiag < 80 & age_3.5years_postdiag > 69 ~ "70-79", age_3.5years_postdiag > 79 ~ "80+")) %>%
  mutate(age_4years_postdiag = case_when(age_4years_postdiag < 10 ~ "0-9", age_4years_postdiag < 20 & age_4years_postdiag > 9 ~ "10-19", age_4years_postdiag < 30 & age_4years_postdiag > 19 ~ "20-29",
                                       age_4years_postdiag < 40 & age_4years_postdiag > 29 ~ "30-39", age_4years_postdiag < 50 & age_4years_postdiag > 39 ~ "40-49", age_4years_postdiag < 60 & age_4years_postdiag > 49 ~ "50-59",
                                       age_4years_postdiag < 70 & age_4years_postdiag > 59 ~ "60-69", age_4years_postdiag < 80 & age_4years_postdiag > 69 ~ "70-79", age_4years_postdiag > 79 ~ "80+")) %>%
  mutate(age_4.5years_postdiag = case_when(age_4.5years_postdiag < 10 ~ "0-9", age_4.5years_postdiag < 20 & age_4.5years_postdiag > 9 ~ "10-19", age_4.5years_postdiag < 30 & age_4.5years_postdiag > 19 ~ "20-29",
                                         age_4.5years_postdiag < 40 & age_4.5years_postdiag > 29 ~ "30-39", age_4.5years_postdiag < 50 & age_4.5years_postdiag > 39 ~ "40-49", age_4.5years_postdiag < 60 & age_4.5years_postdiag > 49 ~ "50-59",
                                         age_4.5years_postdiag < 70 & age_4.5years_postdiag > 59 ~ "60-69", age_4.5years_postdiag < 80 & age_4.5years_postdiag > 69 ~ "70-79", age_4.5years_postdiag > 79 ~ "80+")) %>%
  mutate(age_5years_postdiag = case_when(age_5years_postdiag < 10 ~ "0-9", age_5years_postdiag < 20 & age_5years_postdiag > 9 ~ "10-19", age_5years_postdiag < 30 & age_5years_postdiag > 19 ~ "20-29",
                                       age_5years_postdiag < 40 & age_5years_postdiag > 29 ~ "30-39", age_5years_postdiag < 50 & age_5years_postdiag > 39 ~ "40-49", age_5years_postdiag < 60 & age_5years_postdiag > 49 ~ "50-59",
                                       age_5years_postdiag < 70 & age_5years_postdiag > 59 ~ "60-69", age_5years_postdiag < 80 & age_5years_postdiag > 69 ~ "70-79", age_5years_postdiag > 79 ~ "80+")) %>%
  select(-c(diagnosisdatebest, birthdatebest, diag_age_days))



