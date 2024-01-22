###################### Length of stay 2023######################

#Script to process HES data for 2014, 2015 and 2016 diagnoses to 
#assess the impact of 2020/2021 on length of stay

#Created November 2023 by Lizzie Augarde 
#Change log:
############################################################### 

#prep
library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

los2014_apc_events <- dbGetQueryOracle(casref01, "SELECT * FROM LOS2014_COHORT", rowlimit = NA)

#checks 
nrow(los2014_apc_events[duplicated(los2014_apc_events), ]) #duplicate rows 
n_distinct(los2014_apc_events$PATIENTID) #number unique patient IDs is <= cohort number

los2014_apc_events_check <- los2014_apc_events %>% 
  mutate(death_diag_comp = interval(FOLLOW_UP_START, DEATHDATEBEST) / days(1)) %>%
  mutate(diag_epi_comp = interval(FOLLOW_UP_START, ADMIDATE) / days(1)) %>% 
  mutate(fuend_epi_comp = interval(ADMIDATE, FOLLOW_UP_END) / days(1)) %>%
  mutate(cohort_check = ifelse(PATIENTID %in% los2014_cohort$PATIENTID, "TRUE", "FALSE"))

nrow(los2014_apc_events_check[los2014_apc_events_check$death_diag_comp < 1 & !is.na(los2014_apc_events_check$death_diag_comp), ]) #no deaths before diagnoses
nrow(los2014_apc_events_check[los2014_apc_events_check$diag_epi_comp < 1 & !is.na(los2014_apc_events_check$diag_epi_comp), ]) #admission date before diag date - what to do about those diagnosed during a spell? 
nrow(los2014_apc_events_check[los2014_apc_events_check$fuend_epi_comp < 1 & !is.na(los2014_apc_events_check$fuend_epi_comp), ]) #some died on same day as admittance - what to do about these? 
nrow(los2014_apc_events_check[los2014_apc_events_check$cohort_check == "FALSE"]) #all patients in events are in cohort table


#defining episodes as cancer-related
los2014_apc_events <- los2014_apc_events %>%
  clean_names() %>%
  mutate(diag1_cancer = case_when(str_detect(diag_1, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag2_cancer = case_when(str_detect(diag_2, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag3_cancer = case_when(str_detect(diag_3, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(episode_cancer_related = case_when(diag1_cancer == "Y" | diag2_cancer == "Y" | diag3_cancer == "Y" ~ "Y", TRUE ~ "N"))

diagcols <- c("diag1_cancer", "diag2_cancer", "diag3_cancer")

los2014_apc_events <- los2014_apc_events %>%
  mutate(cancer_code_pos = apply(select(., diagcols) == "Y", 1, function(x) toString(which(x)))) %>%
  mutate(cancer_code_pos = ifelse(cancer_code_pos == "", "NA", cancer_code_pos))


#adjusting dates
los2014_apc_events <- los2014_apc_events %>%
  mutate(adj_disdate = if_else(disdate > follow_up_end, follow_up_end, disdate)) %>% #caps discharge dates to date of death or FU end if they exceed 
  #how long after diagnosis each admission occurs  
  mutate(adm_days_post_diag = difftime(as.Date(admidate), as.Date(follow_up_start), units = "days")) %>%
  mutate(adm_years_post_diag = case_when(adm_days_post_diag < 366 ~ "Within 1 year",
                                         adm_days_post_diag > 365 & adm_days_post_diag < 731 ~ "1-2 years",
                                         adm_days_post_diag > 730 & adm_days_post_diag < 1096  ~ "2-3 years",
                                         adm_days_post_diag > 1095 & adm_days_post_diag < 1461  ~ "3-4 years",
                                         adm_days_post_diag > 1460 ~ "4-5 years",)) %>%
  unique() #removing duplicate events




do this bit in a separate cohort data frame? 
  #identifying how long after diagnosis each patient died
  mutate(death_years_post_diag = case_when(difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 366 ~ "Within 1 year",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 731 ~ "1-2 years",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 1096 ~ "2-3 years",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 1461 ~ "3-4 years",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") > 1460  ~ "4-5 years",
                                           TRUE ~ "Alive at 5 years")) %>%
  #identifying whether a patient is alive at each year post diagnosis
  mutate(alive_years_post_diag = case_when(death_years_post_diag == "Within 1 year" ~ "Death within 1 year",
                                           death_years_post_diag == "1-2 years" ~ "At least 1 year",
                                           death_years_post_diag == "2-3 years" ~ "At least 2 years",
                                           death_years_post_diag == "3-4 years" ~ "At least 3 years",
                                           death_years_post_diag == "4-5 years" ~ "At least 4 years",
                                           TRUE ~ "At least 5 years")) 
  