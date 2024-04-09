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


##### CREATING AGE VARIABLES 
los2014_cohort_agevars <- los2014_cohort %>%
  mutate(patientid = as.character(patientid)) %>%
  select(c(patientid, diagnosisdatebest, birthdatebest)) %>%
  mutate(diag_age_days = difftime(as.Date(diagnosisdatebest), as.Date(birthdatebest), units = "days")) %>%
  
  #ageing on
  mutate(age_3mths_postdiag = as.numeric(diag_age_days + 92),
         age_6mths_postdiag = as.numeric(diag_age_days + 182),
         age_9mths_postdiag = as.numeric(diag_age_days + 274),
         age_1yr_postdiag = as.numeric(diag_age_days + 365),
         age_1.5yrs_postdiag = as.numeric(diag_age_days + 549),
         age_2yrs_postdiag = as.numeric(diag_age_days + 730),
         age_2.5yrs_postdiag = as.numeric(diag_age_days + 913),
         age_3yrs_postdiag = as.numeric(diag_age_days + 1095),
         age_3.5yrs_postdiag = as.numeric(diag_age_days + 1278),
         age_4yrs_postdiag = as.numeric(diag_age_days + 1460),
         age_4.5yrs_postdiag = as.numeric(diag_age_days + 1643),
         age_5yrs_postdiag = as.numeric(diag_age_days + 1825)) %>%
  mutate(age_3mths_postdiag = floor(age_3mths_postdiag/365),
         age_6mths_postdiag = floor(age_6mths_postdiag/365),
         age_9mths_postdiag = floor(age_9mths_postdiag/365),
         age_1yr_postdiag = floor(age_1yr_postdiag/365),
         age_1.5yrs_postdiag = floor(age_1.5yrs_postdiag/365),
         age_2yrs_postdiag = floor(age_2yrs_postdiag/365),
         age_2.5yrs_postdiag = floor(age_2.5yrs_postdiag/365),
         age_3yrs_postdiag = floor(age_3yrs_postdiag/365),
         age_3.5yrs_postdiag = floor(age_3.5yrs_postdiag/365),
         age_4yrs_postdiag = floor(age_4yrs_postdiag/365),
         age_4.5yrs_postdiag = floor(age_4.5yrs_postdiag/365),
         age_5yrs_postdiag = floor(age_5yrs_postdiag/365)) %>%
  
  #converting to age groups ----trying converting this to a function, this is a mess!!!
  mutate(age_3mths_postdiag = case_when(age_3mths_postdiag < 10 ~ "0-9", age_3mths_postdiag < 20 & age_3mths_postdiag > 9 ~ "10-19", age_3mths_postdiag < 30 & age_3mths_postdiag > 19 ~ "20-29",
                                        age_3mths_postdiag < 40 & age_3mths_postdiag > 29 ~ "30-39", age_3mths_postdiag < 50 & age_3mths_postdiag > 39 ~ "40-49", age_3mths_postdiag < 60 & age_3mths_postdiag > 49 ~ "50-59",
                                        age_3mths_postdiag < 70 & age_3mths_postdiag > 59 ~ "60-69", age_3mths_postdiag < 80 & age_3mths_postdiag > 69 ~ "70-79", age_3mths_postdiag > 79 ~ "80+")) %>%
  mutate(age_6mths_postdiag = case_when(age_6mths_postdiag < 10 ~ "0-9", age_6mths_postdiag < 20 & age_6mths_postdiag > 9 ~ "10-19", age_6mths_postdiag < 30 & age_6mths_postdiag > 19 ~ "20-29",
                                        age_6mths_postdiag < 40 & age_6mths_postdiag > 29 ~ "30-39", age_6mths_postdiag < 50 & age_6mths_postdiag > 39 ~ "40-49", age_6mths_postdiag < 60 & age_6mths_postdiag > 49 ~ "50-59",
                                        age_6mths_postdiag < 70 & age_6mths_postdiag > 59 ~ "60-69", age_6mths_postdiag < 80 & age_6mths_postdiag > 69 ~ "70-79", age_6mths_postdiag > 79 ~ "80+")) %>%
  mutate(age_9mths_postdiag = case_when(age_9mths_postdiag < 10 ~ "0-9", age_9mths_postdiag < 20 & age_9mths_postdiag > 9 ~ "10-19", age_9mths_postdiag < 30 & age_9mths_postdiag > 19 ~ "20-29",
                                        age_9mths_postdiag < 40 & age_9mths_postdiag > 29 ~ "30-39", age_9mths_postdiag < 50 & age_9mths_postdiag > 39 ~ "40-49", age_9mths_postdiag < 60 & age_9mths_postdiag > 49 ~ "50-59",
                                        age_9mths_postdiag < 70 & age_9mths_postdiag > 59 ~ "60-69", age_9mths_postdiag < 80 & age_9mths_postdiag > 69 ~ "70-79", age_9mths_postdiag > 79 ~ "80+")) %>%
  mutate(age_1yr_postdiag = case_when(age_1yr_postdiag < 10 ~ "0-9", age_1yr_postdiag < 20 & age_1yr_postdiag > 9 ~ "10-19", age_1yr_postdiag < 30 & age_1yr_postdiag > 19 ~ "20-29",
                                      age_1yr_postdiag < 40 & age_1yr_postdiag > 29 ~ "30-39", age_1yr_postdiag < 50 & age_1yr_postdiag > 39 ~ "40-49", age_1yr_postdiag < 60 & age_1yr_postdiag > 49 ~ "50-59",
                                      age_1yr_postdiag < 70 & age_1yr_postdiag > 59 ~ "60-69", age_1yr_postdiag < 80 & age_1yr_postdiag > 69 ~ "70-79", age_1yr_postdiag > 79 ~ "80+")) %>%
  mutate(age_1.5yrs_postdiag = case_when(age_1.5yrs_postdiag < 10 ~ "0-9", age_1.5yrs_postdiag < 20 & age_1.5yrs_postdiag > 9 ~ "10-19", age_1.5yrs_postdiag < 30 & age_1.5yrs_postdiag > 19 ~ "20-29",
                                         age_1.5yrs_postdiag < 40 & age_1.5yrs_postdiag > 29 ~ "30-39", age_1.5yrs_postdiag < 50 & age_1.5yrs_postdiag > 39 ~ "40-49", age_1.5yrs_postdiag < 60 & age_1.5yrs_postdiag > 49 ~ "50-59",
                                         age_1.5yrs_postdiag < 70 & age_1.5yrs_postdiag > 59 ~ "60-69", age_1.5yrs_postdiag < 80 & age_1.5yrs_postdiag > 69 ~ "70-79", age_1.5yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_2yrs_postdiag = case_when(age_2yrs_postdiag < 10 ~ "0-9", age_2yrs_postdiag < 20 & age_2yrs_postdiag > 9 ~ "10-19", age_2yrs_postdiag < 30 & age_2yrs_postdiag > 19 ~ "20-29",
                                       age_2yrs_postdiag < 40 & age_2yrs_postdiag > 29 ~ "30-39", age_2yrs_postdiag < 50 & age_2yrs_postdiag > 39 ~ "40-49", age_2yrs_postdiag < 60 & age_2yrs_postdiag > 49 ~ "50-59",
                                       age_2yrs_postdiag < 70 & age_2yrs_postdiag > 59 ~ "60-69", age_2yrs_postdiag < 80 & age_2yrs_postdiag > 69 ~ "70-79", age_2yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_2.5yrs_postdiag = case_when(age_2.5yrs_postdiag < 10 ~ "0-9", age_2.5yrs_postdiag < 20 & age_2.5yrs_postdiag > 9 ~ "10-19", age_2.5yrs_postdiag < 30 & age_2.5yrs_postdiag > 19 ~ "20-29",
                                         age_2.5yrs_postdiag < 40 & age_2.5yrs_postdiag > 29 ~ "30-39", age_2.5yrs_postdiag < 50 & age_2.5yrs_postdiag > 39 ~ "40-49", age_2.5yrs_postdiag < 60 & age_2.5yrs_postdiag > 49 ~ "50-59",
                                         age_2.5yrs_postdiag < 70 & age_2.5yrs_postdiag > 59 ~ "60-69", age_2.5yrs_postdiag < 80 & age_2.5yrs_postdiag > 69 ~ "70-79", age_2.5yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_3yrs_postdiag = case_when(age_3yrs_postdiag < 10 ~ "0-9", age_3yrs_postdiag < 20 & age_3yrs_postdiag > 9 ~ "10-19", age_3yrs_postdiag < 30 & age_3yrs_postdiag > 19 ~ "20-29",
                                       age_3yrs_postdiag < 40 & age_3yrs_postdiag > 29 ~ "30-39", age_3yrs_postdiag < 50 & age_3yrs_postdiag > 39 ~ "40-49", age_3yrs_postdiag < 60 & age_3yrs_postdiag > 49 ~ "50-59",
                                       age_3yrs_postdiag < 70 & age_3yrs_postdiag > 59 ~ "60-69", age_3yrs_postdiag < 80 & age_3yrs_postdiag > 69 ~ "70-79", age_3yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_3.5yrs_postdiag = case_when(age_3.5yrs_postdiag < 10 ~ "0-9", age_3.5yrs_postdiag < 20 & age_3.5yrs_postdiag > 9 ~ "10-19", age_3.5yrs_postdiag < 30 & age_3.5yrs_postdiag > 19 ~ "20-29",
                                         age_3.5yrs_postdiag < 40 & age_3.5yrs_postdiag > 29 ~ "30-39", age_3.5yrs_postdiag < 50 & age_3.5yrs_postdiag > 39 ~ "40-49", age_3.5yrs_postdiag < 60 & age_3.5yrs_postdiag > 49 ~ "50-59",
                                         age_3.5yrs_postdiag < 70 & age_3.5yrs_postdiag > 59 ~ "60-69", age_3.5yrs_postdiag < 80 & age_3.5yrs_postdiag > 69 ~ "70-79", age_3.5yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_4yrs_postdiag = case_when(age_4yrs_postdiag < 10 ~ "0-9", age_4yrs_postdiag < 20 & age_4yrs_postdiag > 9 ~ "10-19", age_4yrs_postdiag < 30 & age_4yrs_postdiag > 19 ~ "20-29",
                                       age_4yrs_postdiag < 40 & age_4yrs_postdiag > 29 ~ "30-39", age_4yrs_postdiag < 50 & age_4yrs_postdiag > 39 ~ "40-49", age_4yrs_postdiag < 60 & age_4yrs_postdiag > 49 ~ "50-59",
                                       age_4yrs_postdiag < 70 & age_4yrs_postdiag > 59 ~ "60-69", age_4yrs_postdiag < 80 & age_4yrs_postdiag > 69 ~ "70-79", age_4yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_4.5yrs_postdiag = case_when(age_4.5yrs_postdiag < 10 ~ "0-9", age_4.5yrs_postdiag < 20 & age_4.5yrs_postdiag > 9 ~ "10-19", age_4.5yrs_postdiag < 30 & age_4.5yrs_postdiag > 19 ~ "20-29",
                                         age_4.5yrs_postdiag < 40 & age_4.5yrs_postdiag > 29 ~ "30-39", age_4.5yrs_postdiag < 50 & age_4.5yrs_postdiag > 39 ~ "40-49", age_4.5yrs_postdiag < 60 & age_4.5yrs_postdiag > 49 ~ "50-59",
                                         age_4.5yrs_postdiag < 70 & age_4.5yrs_postdiag > 59 ~ "60-69", age_4.5yrs_postdiag < 80 & age_4.5yrs_postdiag > 69 ~ "70-79", age_4.5yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_5yrs_postdiag = case_when(age_5yrs_postdiag < 10 ~ "0-9", age_5yrs_postdiag < 20 & age_5yrs_postdiag > 9 ~ "10-19", age_5yrs_postdiag < 30 & age_5yrs_postdiag > 19 ~ "20-29",
                                       age_5yrs_postdiag < 40 & age_5yrs_postdiag > 29 ~ "30-39", age_5yrs_postdiag < 50 & age_5yrs_postdiag > 39 ~ "40-49", age_5yrs_postdiag < 60 & age_5yrs_postdiag > 49 ~ "50-59",
                                       age_5yrs_postdiag < 70 & age_5yrs_postdiag > 59 ~ "60-69", age_5yrs_postdiag < 80 & age_5yrs_postdiag > 69 ~ "70-79", age_5yrs_postdiag > 79 ~ "80+")) %>%
  select(-c(diagnosisdatebest, birthdatebest, diag_age_days))



